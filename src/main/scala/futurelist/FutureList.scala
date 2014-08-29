package futurelist

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

sealed trait FutureList[+A] {

  def isEmpty: Boolean

  def headOption: Option[A]

  def !::[B >: A](prepended: B): !::[B] = new !::(prepended, Future.successful(this))

  def ++[B >: A](appended: FutureList[B])(implicit ec: ExecutionContext): FutureList[B]

  def map[B](f: A => B)(implicit ec: ExecutionContext): FutureList[B]

  def flatMap[B](f: A => FutureList[B])(implicit ec: ExecutionContext): Future[FutureList[B]]

  def toList(implicit ec: ExecutionContext): Future[List[A]]

}

object FutureList {

  object Implicits {

    implicit class FutureTailOps[+A](tail: Future[FutureList[A]]) {
      def !::[B >: A] (prepended: B): !::[B] = new !::(prepended, tail)
    }

  }

}

case class !::[+A](head: A, tail: Future[FutureList[A]]) extends FutureList[A] {

  import FutureList.Implicits._

  override val isEmpty: Boolean = false

  override val headOption: Option[A] = Some(head)

  override def ++[B >: A](appended: FutureList[B])(implicit ec: ExecutionContext): !::[B] = {
    head !:: tail.map(_ ++ appended)
  }

  override def map[B](f: (A) => B)(implicit ec: ExecutionContext): FutureList[B] = {
    f(head) !:: tail.map(_.map(f))
  }

  override def flatMap[B](f: (A) => FutureList[B])(implicit ec: ExecutionContext):
    Future[FutureList[B]] = {

    tail.flatMap(_.flatMap(f)).map(f(head) ++ _)
  }

  override def toList(implicit ec: ExecutionContext): Future[List[A]] = {
    tail.flatMap(_.toList).map(head :: _)
  }

}

case object FutureNil extends FutureList[Nothing] {

  override val isEmpty: Boolean = true

  override val headOption: Option[Nothing] = None

  override def ++[B >: Nothing](appended: FutureList[B])(implicit ec: ExecutionContext):
    FutureList[B] = appended

  override def map[B](f: (Nothing) => B)(implicit ec: ExecutionContext): FutureList[B] = FutureNil

  override def flatMap[B](f: (Nothing) => FutureList[B])(implicit ec: ExecutionContext):
    Future[FutureList[B]] = Future.successful(FutureNil)

  override def toList(implicit ec: ExecutionContext): Future[List[Nothing]] = Future.successful(Nil)

}
