import opencl.executor.Executor
import org.scalactic.source.Position
import org.scalatest.{BeforeAndAfter, Tag}
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import util.{AssertSame, Time, TimeSpan}

package object test_util {
  abstract class Tests extends AnyFunSuite with Matchers

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
        case _: Throwable =>
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

  def runsWithSameResult[R, U <: Time.Unit](runs: Seq[(String, (R, TimeSpan[U]))])
                                           (implicit assertSame: AssertSame[R]): Unit = {
    runs.tail.foreach(r => assertSame(r._2._1, runs.head._2._1, s"${r._1} had a different result"))
    runs.foreach(r => println(s"${r._1} time: ${r._2._2}"))
  }
}
