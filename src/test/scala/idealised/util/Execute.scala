package idealised.util

import idealised.OpenCL._
import idealised.SurfaceLanguage.DSL.{fun, mapSeq, _}
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{->, Expr}
import lift.arithmetic.?

import scala.language.postfixOps
import scala.sys.process._

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

class ExecuteOpenCL extends idealised.util.TestsWithExecutor {
  test("Running a kernel") {
    val f:Expr[DataType -> DataType] = fun(ArrayType(8, int))(xs => xs :>> mapSeq(fun(x => x + 1)))
    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(f, Map()).toPhrase, 1, 1)
    println(kernel.code)
    SyntaxChecker.checkOpenCL(kernel.code)

    val kernelF = kernel.as[ScalaFunction`(`Array[Int]`)=>`Array[Int]]
    val xs = Array.fill(8)(0)

    val (result, time) =  kernelF(xs `;`)
    println(result)
    println(time)
  }
}
