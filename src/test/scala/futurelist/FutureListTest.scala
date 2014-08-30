package futurelist

import org.junit.Test
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.junit.AssertionsForJUnit

class FutureListTest extends AssertionsForJUnit with ScalaFutures {

  @Test
  def construct(): Unit = {
    val futureList = 1 !:: 2 !:: FutureList.Nil
  }

  @Test
  def constructFutureTail(): Unit = {
    val futureList = 1 !:: 2 !:: FutureList.Nil

    import FutureList.Implicits._
    import scala.concurrent.ExecutionContext.Implicits.global
    val newList = 10 !:: futureList.tailOption.map(_.get)

    assert(Some(10) === newList.headOption.futureValue)
  }

  @Test
  def map(): Unit = {
    val intList = 1 !:: 2 !:: FutureList.Nil

    import scala.concurrent.ExecutionContext.Implicits.global
    val stringList = intList.map(_.toString)

    assert(Some("1") === stringList.headOption.futureValue)
  }

  @Test
  def toList(): Unit = {
    val futureList = 1 !:: 2 !:: FutureList.Nil

    import scala.concurrent.ExecutionContext.Implicits.global
    val list = futureList.toList.futureValue

    assert(List(1, 2) === list)
  }

  @Test
  def appended(): Unit = {
    val futureList1 = 1 !:: 2 !:: FutureList.Nil
    val futureList2 = 3 !:: 4 !:: FutureList.Nil

    import scala.concurrent.ExecutionContext.Implicits.global
    val combined = futureList1 ++ futureList2

    assert(List(1, 2, 3, 4) === combined.toList.futureValue)
  }

  @Test
  def flatMap(): Unit = {
    val futureList = 10 !:: 20 !:: FutureList.Nil
    def f(i: Int): FutureList[Int] = i !:: i + 1 !:: i + 2 !:: FutureList.Nil

    import scala.concurrent.ExecutionContext.Implicits.global
    val result = futureList.flatMap(f)

    assert(List(10, 11, 12, 20, 21, 22) === result.toList.futureValue)
  }

  @Test
  def take(): Unit = {
    val futureList = 1 !:: 2 !:: 3 !:: 4 !:: FutureList.Nil

    import scala.concurrent.ExecutionContext.Implicits.global
    assert(Nil === futureList.take(0).toList.futureValue)
    assert(List(1) === futureList.take(1).toList.futureValue)
    assert(List(1, 2) === futureList.take(2).toList.futureValue)
    assert(List(1, 2, 3, 4) === futureList.take(4).toList.futureValue)
    assert(List(1, 2, 3, 4) === futureList.take(5).toList.futureValue)

    intercept[IllegalArgumentException] {
      futureList.take(-1)
    }
  }

}
