import opencl.executor.Executor
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import util.{AssertSame, Time, TimeSpan}

package object test_util {
  abstract class Tests extends AnyFunSuite with Matchers

  abstract class TestsWithExecutor extends Tests with BeforeAndAfter {
    before {
      Executor.loadLibrary()
      Executor.init()
    }

    after {
      Executor.shutdown()
    }
  }

  abstract class TestsWithYACX extends Tests with BeforeAndAfter {
    val executeCudaTests = sys.props.getOrElse("executeCudaTests", "false").toBoolean

    before {
      if (executeCudaTests)
        yacx.Executor.loadLibrary()
    }
  }

  def runsWithSameResult[R, U <: Time.Unit](runs: Seq[(String, (R, TimeSpan[U]))])
                                           (implicit assertSame: AssertSame[R]): Unit = {
    runs.tail.foreach(r => assertSame(r._2._1, runs.head._2._1, s"${r._1} had a different result"))
    runs.foreach(r => println(s"${r._1} time: ${r._2._2}"))
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
