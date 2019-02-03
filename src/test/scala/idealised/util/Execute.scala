package idealised.util

import sys.process._

import scala.language.postfixOps

object Execute {
  case class Exception(msg: String) extends Throwable

  @throws[Exception]
  def apply(code: String) = {
    try {
      val src = writeToTempFile("code-", ".c", code).getAbsolutePath
      val bin = createTempFile("bin-", "").getAbsolutePath
      (s"clang -O2 $src -o $bin" !!)
      (s"$bin" !!)
    } catch {
      case _: Throwable =>
        Console.err.println("==========")
        Console.err.println("execution failed for code:")
        Console.err.println(code)
        Console.err.println("==========")
        throw Exception(s"execution failed for: `$code'")
    }
  }
}
