package util

import scala.language.postfixOps
import scala.sys.process._

object Execute2 {
  case class Exception(msg: String) extends Throwable

  //noinspection ScalaUnnecessaryParentheses
  @throws[Exception]
  def apply(code: String): String = {
    try {
      val src = writeToTempFile("code-", ".c", code).getAbsolutePath
      val bin = createTempFile("bin-", "").getAbsolutePath
      (s"clang -O2 $src -o $bin -lm" !!)

      //repeat execution
      //take median as runtime
      val N = 10000
      val runtimes:Array[Double] = new Array[Double](N)
      for(i <- Range(0,N)){
        runtimes(i) = (s"$bin" !!).toDouble
      }
      runtimes.sorted.apply(N/2).toString()
    } catch {
      case e: Throwable =>
        Console.err.println("==========")
        Console.err.println(s"execution failed ($e) for code:")
        Console.err.println(code)
        Console.err.println("==========")
        throw Exception(s"execution failed ($e) for: `$code'")
    }
  }

}


