import opencl.executor.Executor
import org.scalactic.source.Position
import org.scalatest.exceptions.TestCanceledException
import org.scalatest.{BeforeAndAfter, Tag}
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import org.apache.logging.log4j.scala.Logging
import util.{AssertSame, Time, TimeSpan}

package object test_util {
  abstract class Tests extends AnyFunSuite with Matchers with Logging {
    def runsWithSameResult[R, U <: Time.Unit](runs: Seq[(String, (R, TimeSpan[U]))])
                                             (implicit assertSame: AssertSame[R]): Unit = {
      runs.tail.foreach(r => assertSame(r._2._1, runs.head._2._1, s"${r._1} had a different result"))
      runs.foreach(r => logger.debug(s"${r._1} time: ${r._2._2}"))
    }
  }

  abstract class TestsWithExecutor extends Tests with BeforeAndAfter {
    var openclIsAvailable = true
    before {
      try {
        Executor.loadLibrary()
        Executor.init()
      } catch {
        case _: UnsatisfiedLinkError =>
          openclIsAvailable = false
      }
    }

    after {
      try {
        Executor.shutdown()
      } catch {
        case _: UnsatisfiedLinkError =>
      }
    }

    override protected def test(testName: String, testTags: Tag*)
                               (testFun: => Any)
                               (implicit pos: Position): Unit = {
      super.test(testName, testTags:_*) {
        // try to execute test ...
        try {
          testFun
        } catch {
          // ... only if execution fails due to a unsatisfied link error we
          // enforce the assumption that OpenCL must be available.
          case _: UnsatisfiedLinkError =>
            assume(openclIsAvailable)
        }
      }
    }
  }

  def withExecutor[T](f: => T): T = {
    import opencl.executor._

    try {
      Executor.loadLibrary()
      Executor.init()
      try f
      finally Executor.shutdown()
    } catch {
      case e: UnsatisfiedLinkError =>
        throw new TestCanceledException("OpenCL not available", e, 0)
      case e : Throwable => throw e
    }
  }

  abstract class TestWithCUDA extends Tests with BeforeAndAfter {
    val executeCudaTests = sys.props.getOrElse("executeCudaTests", "false").toBoolean

    before {
      if (executeCudaTests)
        yacx.Executor.loadLibrary()
    }
  }

  private final def maxDifference = 2.5f

  def similar(f1: Float, f2: Float): Boolean = {
    java.lang.Math.abs(f1-f2) < maxDifference
  }

  def similar(af1: Array[Float], af2: Array[Float]): Boolean = {
    af1 zip af2 map{pair => similar(pair._1, pair._2)} reduce(_&&_)
  }

  def similar(af1: Array[Array[Float]], af2: Array[Array[Float]]): Boolean = {
    af1 zip af2 map{pair => similar(pair._1, pair._2)} reduce(_&&_)
  }
}
