package futurelist

import futurelist.FutureList.Node

import scala.collection.immutable
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

final class FutureList[+A](private val head: Future[Node[A]]) {

  import FutureList._

  def isEmpty(implicit ec: ExecutionContext): Future[Boolean] = head.map(_.isEmpty)

  def headOption(implicit ec: ExecutionContext): Future[Option[A]] = head.map {
    case ConsNode(h, _) => Some(h)
    case NilNode => None
  }

  def tailOption(implicit ec: ExecutionContext): Future[Option[FutureList[A]]] = head.map {
    case ConsNode(_, tail) => Some(new FutureList(tail))
    case NilNode => None
  }

  def !::[AA >: A](prepended: AA): FutureList[AA] = {
    new FutureList(Future.successful(ConsNode(prepended, head)))
  }

  def ++[AA >: A](appended: FutureList[AA])(implicit ec: ExecutionContext): FutureList[AA] = {
    new FutureList(for {
      h <- head
      ah <- appended.head
    } yield {
      h ++ ah
    })
  }

  def map[B](f: A => B)(implicit ec: ExecutionContext): FutureList[B] = {
    new FutureList(head.map(_.map(f)))
  }

  def flatMap[B](f: A => FutureList[B])(implicit ec: ExecutionContext): FutureList[B] = {
    new FutureList(head.flatMap(_.futureFlatMap(f.andThen(_.head))))
  }

  def toList(implicit ec: ExecutionContext): Future[List[A]] = head.flatMap(_.toList)

}

object FutureList {

  val Nil = new FutureList(Future.successful(NilNode))

  object Implicits {

    implicit class FutureTailOps[+A](tail: Future[FutureList[A]]) {
      def !::[AA >: A] (prepended: AA)(implicit ec: ExecutionContext): FutureList[AA] = {
        new FutureList(Future.successful(ConsNode(prepended, tail.flatMap(_.head))))
      }
    }

  }

  private sealed trait Node[+A] {

    def isEmpty: Boolean

    def headOption: Option[A]

    def !::[AA >: A](prepended: AA): ConsNode[AA] = new ConsNode(prepended, Future.successful(this))

    def ++[AA >: A](appended: Node[AA])(implicit ec: ExecutionContext): Node[AA]

    def map[B](f: A => B)(implicit ec: ExecutionContext): Node[B]

    def futureFlatMap[B](f: A => Future[Node[B]])(implicit ec: ExecutionContext): Future[Node[B]]

    def toList(implicit ec: ExecutionContext): Future[List[A]]

  }

  private case class ConsNode[+A](head: A, tail: Future[Node[A]]) extends Node[A] {

    override val isEmpty: Boolean = false

    override val headOption: Option[A] = Some(head)

    override def ++[AA >: A](appended: Node[AA])(implicit ec: ExecutionContext): ConsNode[AA] = {
      ConsNode(head, tail.map(_ ++ appended))
    }

    override def map[B](f: (A) => B)(implicit ec: ExecutionContext): Node[B] = {
      ConsNode(f(head), tail.map(_.map(f)))
    }

    override def futureFlatMap[B](f: (A) => Future[Node[B]])(implicit ec: ExecutionContext):
      Future[Node[B]] = {

      for {
        fh <- f(head)
        t <- tail
        ft <- t.futureFlatMap(f)
      } yield fh ++ ft
    }

    override def toList(implicit ec: ExecutionContext): Future[List[A]] = {
      tail.flatMap(_.toList).map(head :: _)
    }

  }

  private case object NilNode extends Node[Nothing] {

    override val isEmpty: Boolean = true

    override val headOption: Option[Nothing] = None

    override def ++[AA >: Nothing](appended: Node[AA])(implicit ec: ExecutionContext): Node[AA] = {
      appended
    }

    override def map[B](f: (Nothing) => B)(implicit ec: ExecutionContext): Node[B] = NilNode

    override def futureFlatMap[B](f: (Nothing) => Future[Node[B]])(implicit ec: ExecutionContext):
      Future[Node[B]] = {

      Future.successful(NilNode)
    }

    override def toList(implicit ec: ExecutionContext): Future[List[Nothing]] = {
      Future.successful(immutable.Nil)
    }

  }

}
