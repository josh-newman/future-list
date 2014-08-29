package futurelist

import org.junit.Test

import scala.concurrent.Future

class FutureListTest {

  import futurelist._

  @Test
  def construct(): Unit = {
    val futureList = 1 !:: 2 !:: FutureNil
  }

}
