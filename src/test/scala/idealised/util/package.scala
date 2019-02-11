package idealised

import java.io.{File, PrintWriter}

import opencl.executor.Executor
import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}

package object util {
  abstract class Tests extends FunSuite with Matchers

  abstract class TestsWithExecutor extends Tests with BeforeAndAfter {
    before {
      Executor.loadLibrary()
      Executor.init(1, 0)
    }

    after {
      Executor.shutdown()
    }
  }

  def createTempFile(prefix: String, suffix: String): File = {
    val tmp = File.createTempFile(prefix, suffix)
    tmp.deleteOnExit()
    tmp
  }

  def writeToTempFile(prefix: String, suffix: String, content: String): File = {
    val tmp = createTempFile(prefix, suffix)
    new PrintWriter(tmp) {
      try {
        write(content)
      } finally {
        close()
      }
    }
    tmp
  }
}
