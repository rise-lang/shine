import java.io.{File, PrintWriter}

import com.github.ghik.silencer.silent
import opencl.executor.Executor
import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}

package object util {
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

  def readFile(path: String): String = {
    val source = io.Source.fromFile(path)
    try source.getLines.mkString("\n") finally source.close
  }

  import idealised.utils.{Time, TimeSpan}
  def runsWithSameResult[R, U <: Time.Unit](runs: Seq[(String, (R, TimeSpan[U]))])
                                           (implicit assertSame: AssertSame[R]): Unit = {
    runs.tail.foreach(r => assertSame(r._2._1, runs.head._2._1, s"${r._1} had a different result"))
    runs.foreach(r => println(s"${r._1} time: ${r._2._2}"))
  }
}
