package util

import scala.language.postfixOps
import scala.sys.process._

object Execute {
  case class Exception(msg: String) extends Throwable

  //noinspection ScalaUnnecessaryParentheses
  @throws[Exception]
  def apply(code: String): String = {
    try {
      val src = writeToTempFile("code-", ".c", code).getAbsolutePath
      val bin = createTempFile("bin-", "").getAbsolutePath
      (s"clang -O2 $src -o $bin -lm" !!)
      (s"$bin" !!)
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

