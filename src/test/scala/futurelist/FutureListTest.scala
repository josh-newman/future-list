package futurelist

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

class FutureListTest extends AssertionsForJUnit {

  import futurelist._

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

}
