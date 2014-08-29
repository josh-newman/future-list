package futurelist

import scala.concurrent.Future

sealed trait FutureList[+A] {

  def isEmpty: Boolean

  def headOption: Option[A]

}

case class FutureCons[A](head: A, tail: Future[FutureList[A]]) extends FutureList[A] {

  override val isEmpty: Boolean = false

  override val headOption: Option[A] = Some(head)

}

case object FutureNil extends FutureList[Nothing] {

  override val isEmpty: Boolean = true

  override val headOption: Option[Nothing] = None

}
