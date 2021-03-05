import java.io.{File, PrintWriter}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

package object util {
  def createTempFile(prefix: String, suffix: String): File = {
    val tmp = File.createTempFile(prefix, suffix)
    tmp.deleteOnExit()
    tmp
  }

  def writeToTempFile(prefix: String, suffix: String, content: String): File = {
    val tmp = createTempFile(prefix, suffix)
    writeToFile(tmp, content)
    tmp
  }

  def writeToPath(path: String, content: String): Unit = {
    writeToFile(new File(path), content)
  }

  def writeToFile(file: File, content: String): Unit = {
    new PrintWriter(file) {
      try {
        write(content)
      } finally {
        close()
      }
    }
  }

  def readFile(path: String): String = {
    val source = io.Source.fromFile(path)
    try source.getLines().mkString("\n")
    finally source.close
  }

  def assertSame[T](a: T, b: T, msg: String)
    (implicit same: AssertSame[T]): Unit =
  {
    same(a, b, msg)
  }

  def withExecutor[T](f: => T): Try[T] = {
    import opencl.executor._

    try Success({
      Executor.loadLibrary()
      Executor.init()
      try f
      finally Executor.shutdown()
    }) catch {
      case e: UnsatisfiedLinkError => Failure(e)
      case NonFatal(e) => Failure(e)
    }
  }

  def printTime[T](msg: String, block: => T): T = {
    val start = System.currentTimeMillis()
    val result = block
    val end = System.currentTimeMillis()

    val elapsed = end - start
    val milliseconds = elapsed % 1000
    val rest = elapsed / 1000
    val seconds = rest % 60
    val minutes = rest / 60

    val millisStr = s"${milliseconds}ms"
    val secondsStr = if (seconds > 0) s"${seconds}s " else ""
    val minutesStr = if (minutes > 0) s"${minutes}mn " else ""
    println(s"${msg}: ${minutesStr}${secondsStr}${millisStr}")
    result
  }

  def dotPrintTmp(
    name: String,
    r: elevate.core.RewriteResult[rise.elevate.Rise]
  ): Unit = r match {
    case elevate.core.Success(p) => dotPrintTmp(name, p)
    case _ =>
  }

  def dotPrintTmp(prefix: String, e: rise.core.Expr): Unit = {
    import scala.language.postfixOps
    import scala.sys.process._

    val dotString = rise.core.dotPrinter.generateDotString(e,
      printTypes = false,
      inlineLambdaIdentifier = true,
      applyNodes = false)
    val dotFile = File.createTempFile(prefix, ".dot")
    writeToFile(dotFile, dotString)
    val dotPath = dotFile.getPath()
    val svgPath = dotPath.replace(".dot", ".svg")
    (s"dot -Tsvg $dotPath -o $svgPath" !!)
    println(s"wrote $dotPath.svg and .dot")
  }
}
