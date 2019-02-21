package idealised.OpenCL

import idealised.SurfaceLanguage.DSL.{fun, mapSeq, _}
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{->, Expr, NatIdentifier, `(nat)->`}
import idealised.util.SyntaxChecker

import scala.language.postfixOps


class ExecuteOpenCL extends idealised.util.TestsWithExecutor {
  test("Running a kernel") {
    val f:Expr[`(nat)->`[DataType -> DataType]] =
      dFun((n: NatIdentifier) => fun(ArrayType(n, int))(xs => xs :>> mapSeq(fun(x => x + 1))))

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
