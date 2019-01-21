package idealised

import opencl.executor.Executor
import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}

package object util {
  abstract class Tests extends FunSuite with Matchers

  abstract class TestsWithExecutor extends Tests with BeforeAndAfter {
    before {
      Executor.loadLibrary()
    }

    after {
      Executor.shutdown()
    }
  }
}
