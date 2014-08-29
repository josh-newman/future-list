package futurelist

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

sealed trait FutureList[+A] {

  def isEmpty: Boolean

  def headOption: Option[A]

  def !::[B >: A] (prepended: B): !::[B] = new !::(prepended, Future.successful(this))

  def map[B](f: A => B)(implicit ec: ExecutionContext): FutureList[B]

}

object FutureList {

  object Implicits {

    implicit class FutureTailOps[+A](tail: Future[FutureList[A]]) {
      def !::[B >: A] (prepended: B): FutureList[B] = new !::(prepended, tail)
    }

  }

}

case class !::[A](head: A, tail: Future[FutureList[A]]) extends FutureList[A] {

  import FutureList.Implicits._

  override val isEmpty: Boolean = false

  override val headOption: Option[A] = Some(head)

  override def map[B](f: (A) => B)(implicit ec: ExecutionContext): FutureList[B] = {
    f(head) !:: tail.map(_.map(f))
  }

}

case object FutureNil extends FutureList[Nothing] {

  override val isEmpty: Boolean = true

  override val headOption: Option[Nothing] = None

  override def map[B](f: (Nothing) => B)(implicit ec: ExecutionContext): FutureList[B] = this

}
