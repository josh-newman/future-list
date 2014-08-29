package futurelist

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

class FutureListTest extends AssertionsForJUnit {

  @Test
  def construct(): Unit = {
    val futureList = 1 !:: 2 !:: FutureNil
  }

  @Test
  def extract(): Unit = {
    val (head, _) = 1 !:: FutureNil match {
      case h !:: t => (h, t)
      case other => fail(s"Should have matched: $other")
    }

    assert(1 === head)
  }

  @Test
  def constructFutureTail(): Unit = {
    val futureList = 1 !:: 2 !:: FutureNil

    import FutureList.Implicits._
    val newList = 10 !:: futureList.tail

    assert(Some(10) === newList.headOption)
  }

  @Test
  def map(): Unit = {
    val intList = 1 !:: 2 !:: FutureNil

    import scala.concurrent.ExecutionContext.Implicits.global
    val stringList = intList.map(_.toString)

    assert(Some("1") === stringList.headOption)
  }

}
