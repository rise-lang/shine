package util

import scala.language.postfixOps
import scala.sys.process._

object SyntaxChecker {

  case class Exception(msg: String) extends Throwable

  @throws[SyntaxChecker.Exception]("if code doesn't pass the syntax check")
  def apply(code: String, extension: String = ".c", options: String = "-Werror -Wno-implicit-function-declaration"): Unit = {
    try {
      s"clang -fsyntax-only $options ${writeToTempFile("code-", extension, code).getAbsolutePath}" !!
    } catch {
      case _: Throwable =>
        Console.err.println("==========")
        Console.err.println("SyntaxChecker failed for code:")
        Console.err.println(code)
        Console.err.println("==========")
        throw Exception(s"Code: `$code' did not pass syntax check")
    }
  }

  @throws[SyntaxChecker.Exception]("if code doesn't pass the OpenCL syntax check")
  def checkOpenCL(code: String): Unit = {
    apply(code, ".cl", "-Xclang -finclude-default-header -cl-std=CL1.2")
  }
}
