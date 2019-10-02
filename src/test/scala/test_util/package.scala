import com.github.ghik.silencer.silent
import opencl.executor.Executor
import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}
import util.{AssertSame, Time, TimeSpan}

package object test_util {
  @silent("define classes/objects inside of package objects")
  abstract class Tests extends FunSuite with Matchers

  @silent("define classes/objects inside of package objects")
  abstract class TestsWithExecutor extends Tests with BeforeAndAfter {
    before {
      Executor.loadLibrary()
      Executor.init()
    }

    after {
      Executor.shutdown()
    }
  }

  def runsWithSameResult[R, U <: Time.Unit](runs: Seq[(String, (R, TimeSpan[U]))])
                                           (implicit assertSame: AssertSame[R]): Unit = {
    runs.tail.foreach(r => assertSame(r._2._1, runs.head._2._1, s"${r._1} had a different result"))
    runs.foreach(r => println(s"${r._1} time: ${r._2._2}"))
  }
}
