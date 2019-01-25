package idealised.util

import java.io.{ File, PrintWriter }
import sys.process._

import scala.language.postfixOps

object SyntaxChecker {

  case class Exception(msg: String) extends Throwable

  @throws[SyntaxChecker.Exception]("if code doesn't pass the syntax check")
  def apply(code: String, extension: String = ".c"): Unit = {
    val platformSpecificOptions = {
      extension match {
        case ".cl" => ""
        case _ => ""
      }
    }
    try {
      s"clang  $platformSpecificOptions  -fsyntax-only ${writeToTempFile(code, extension).getAbsolutePath}" !!
    } catch {
      case _: Throwable =>
        Console.err.println("==========")
        Console.err.println("SyntaxChecker failed for code:")
        Console.err.println(code)
        Console.err.println("==========")
        throw Exception(s"Code: `$code' did not pass syntax check")
    }
  }

  private def writeToTempFile(content: String, extension: String): File = {
    val tmp = File.createTempFile("code-", extension)
    tmp.deleteOnExit()
    new PrintWriter(tmp) {
      try {
        write(content)
      } finally {
        close()
      }
    }
    tmp
  }

}
