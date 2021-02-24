package util

import scala.language.postfixOps
import scala.sys.process._

object SyntaxChecker {
  case class Exception(msg: String) extends Throwable

  val defaultOptions = Seq(
    "-Werror",
    "-Wno-implicit-function-declaration",
    "-Wno-parentheses-equality",
    "-Wno-unused-command-line-argument",
    "-Iruntime/",
    "-Ilib/executor/lib/Executor/include/").mkString(" ")

  @throws[SyntaxChecker.Exception]("if code doesn't pass the syntax check")
  def apply(code: String, extension: String = ".c", options: String = defaultOptions): Unit = {
    try {
      val f = writeToTempFile("code-", extension,
      // define loadKernel for syntax checking
      """#define loadKernel(ctx, k) loadKernelFromFile(ctx, "", "")""" + '\n' + code)
      s"clang -fsyntax-only $options ${f.getAbsolutePath}" !!
    } catch {
      case _: Throwable =>
        Console.err.println("==========")
        Console.err.println("SyntaxChecker failed for code:")
        Console.err.println(code.take(500 * 160))
        Console.err.println("==========")
        throw Exception(s"Code: `$code' did not pass syntax check")
    }
  }

  @throws[SyntaxChecker.Exception]("if code doesn't pass the OpenCL syntax check")
  def checkOpenCL(code: String): Unit = {
    apply(code, ".cl", "-Xclang -finclude-default-header -cl-std=CL1.2")
  }
}
