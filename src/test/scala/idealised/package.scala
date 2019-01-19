import opencl.executor.Executor
import org.scalatest._

package object idealised {
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
