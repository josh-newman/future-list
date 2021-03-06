package futurelist

import org.junit.Test
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.junit.AssertionsForJUnit

class FutureListTest extends AssertionsForJUnit with ScalaFutures {

  import FutureListTest._

  @Test
  def isEmpty(): Unit = {
    val futureList = 1 !:: 2 !:: FutureList.Nil

    import scala.concurrent.ExecutionContext.Implicits.global
    assert(true === FutureList.Nil.isEmpty.futureValue)
    assert(false === futureList.isEmpty.futureValue)
  }

  @Test
  def headOption(): Unit = {
    val futureList = 1 !:: 2 !:: FutureList.Nil

    import scala.concurrent.ExecutionContext.Implicits.global
    assert(None === FutureList.Nil.headOption.futureValue)
    assert(Some(1) === futureList.headOption.futureValue)
  }

  @Test
  def tailOption(): Unit = {
    val futureList1 = 1 !:: FutureList.Nil
    val futureList2 = 1 !:: 2 !:: FutureList.Nil

    import scala.concurrent.ExecutionContext.Implicits.global
    val tail1 = futureList1.tailOption.futureValue
    val tail2 = futureList2.tailOption.futureValue

    assert(None === FutureList.Nil.tailOption.futureValue)
    assert(Some(Nil) === tail1.map(_.toList.futureValue))
    assert(Some(List(2)) === tail2.map(_.toList.futureValue))
  }

  @Test
  def construct(): Unit = {
    val futureList = 1 !:: 2 !:: FutureList.Nil
  }

  @Test
  def append(): Unit = {
    val futureList1 = 1 !:: 2 !:: FutureList.Nil
    val futureList2 = 3 !:: 4 !:: FutureList.Nil

    import scala.concurrent.ExecutionContext.Implicits.global
    val combined = futureList1 ++ futureList2

    assert(List(1, 2, 3, 4) === combined.toList.futureValue)
  }

  @Test
  def map(): Unit = {
    val intList = 1 !:: 2 !:: FutureList.Nil

    import scala.concurrent.ExecutionContext.Implicits.global
    val stringList = intList.map(_.toString)

    assert(Some("1") === stringList.headOption.futureValue)
  }

  @Test
  def flatMap(): Unit = {
    val futureList = 10 !:: 20 !:: FutureList.Nil

    import scala.concurrent.ExecutionContext.Implicits.global
    val result = futureList.flatMap(nextFew)

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

  @Test
  def drop(): Unit = {
    val futureList = 1 !:: 2 !:: 3 !:: 4 !:: FutureList.Nil

    import scala.concurrent.ExecutionContext.Implicits.global
    assert(List(1, 2, 3, 4) === futureList.drop(0).toList.futureValue)
    assert(List(2, 3, 4) === futureList.drop(1).toList.futureValue)
    assert(List(3, 4) === futureList.drop(2).toList.futureValue)
    assert(Nil === futureList.drop(4).toList.futureValue)
    assert(Nil === futureList.drop(5).toList.futureValue)

    intercept[IllegalArgumentException] {
      futureList.drop(-1)
    }
  }

  @Test
  def filter(): Unit = {
    val futureList1: FutureList[Int] = FutureList.Nil
    val futureList2 = 1 !:: 2 !:: 3 !:: FutureList.Nil

    import scala.concurrent.ExecutionContext.Implicits.global
    val filtered1 = futureList1.filter(_ > 1).toList.futureValue
    val filtered2 = futureList2.filter(_ > 1).toList.futureValue

    assert(Nil === filtered1)
    assert(List(2, 3) === filtered2)
  }

  @Test
  def filterNot(): Unit = {
    val futureList1: FutureList[Int] = FutureList.Nil
    val futureList2 = 1 !:: 2 !:: 3 !:: FutureList.Nil

    import scala.concurrent.ExecutionContext.Implicits.global
    val filtered1 = futureList1.filterNot(_ > 1).toList.futureValue
    val filtered2 = futureList2.filterNot(_ > 1).toList.futureValue

    assert(Nil === filtered1)
    assert(List(1) === filtered2)
  }

  @Test
  def withFilter(): Unit = {
    val futureList1: FutureList[Int] = FutureList.Nil
    val futureList2 = 1 !:: 2 !:: 3 !:: FutureList.Nil

    import scala.concurrent.ExecutionContext.Implicits.global
    val filtered1 = futureList1.withFilter(_ > 1).toList.futureValue
    val filtered2 = futureList2.withFilter(_ > 1).toList.futureValue

    assert(Nil === filtered1)
    assert(List(2, 3) === filtered2)
  }

  @Test
  def forComprehension(): Unit = {
    val futureList = 10 !:: 21 !:: 30 !:: 41 !:: FutureList.Nil

    import scala.concurrent.ExecutionContext.Implicits.global
    val result = for {
      i <- futureList
      if i % 2 == 0
      j <- nextFew(i)
    } yield {
      j
    }

    assert(List(10, 11, 12, 30, 31, 32) === result.toList.futureValue)
  }

  @Test
  def toList(): Unit = {
    val futureList = 1 !:: 2 !:: FutureList.Nil

    import scala.concurrent.ExecutionContext.Implicits.global
    val list = futureList.toList.futureValue

    assert(List(1, 2) === list)
  }

  @Test
  def constructFutureTail(): Unit = {
    val futureList = 1 !:: 2 !:: FutureList.Nil

    import FutureList.Implicits._
    import scala.concurrent.ExecutionContext.Implicits.global
    val newList = 10 !:: futureList.tailOption.map(_.get)

    assert(Some(10) === newList.headOption.futureValue)
  }

}

object FutureListTest {
  def nextFew(i: Int): FutureList[Int] = i !:: i + 1 !:: i + 2 !:: FutureList.Nil
}
