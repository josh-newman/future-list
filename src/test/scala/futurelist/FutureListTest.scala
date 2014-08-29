package futurelist

import org.junit.Test
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.junit.AssertionsForJUnit

class FutureListTest extends AssertionsForJUnit with ScalaFutures {

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

  @Test
  def toList(): Unit = {
    val futureList = 1 !:: 2 !:: FutureNil

    import scala.concurrent.ExecutionContext.Implicits.global
    val list = futureList.toList.futureValue

    assert(List(1, 2) === list)
  }

  @Test
  def appended(): Unit = {
    val futureList1 = 1 !:: 2 !:: FutureNil
    val futureList2 = 3 !:: 4 !:: FutureNil

    import scala.concurrent.ExecutionContext.Implicits.global
    val combined = futureList1 ++ futureList2

    assert(List(1, 2, 3, 4) === combined.toList.futureValue)
  }

  @Test
  def flatMap(): Unit = {
    val futureList = 10 !:: 20 !:: FutureNil
    def f(i: Int): FutureList[Int] = i !:: i + 1 !:: i + 2 !:: FutureNil

    import scala.concurrent.ExecutionContext.Implicits.global
    val result = futureList.flatMap(f)

    assert(List(10, 11, 12, 20, 21, 22) === result.futureValue.toList.futureValue)
  }

}
