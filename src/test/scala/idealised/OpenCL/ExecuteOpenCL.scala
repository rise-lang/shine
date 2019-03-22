package idealised.OpenCL

import idealised.OpenCL.SurfaceLanguage.DSL.mapGlobal
import idealised.SurfaceLanguage.DSL.{fun, mapSeq, _}
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

import scala.language.{postfixOps, reflectiveCalls}


class ExecuteOpenCL extends idealised.util.TestsWithExecutor {
  test("Running a simple kernel with generic input size") {
    val f: Expr =
      nFun(n => fun(ArrayType(n, int))(xs => xs :>> mapSeq(fun(x => x + 1))))

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(f, Map())))
    println(kernel.code)
    SyntaxChecker.checkOpenCL(kernel.code)

    val kernelF = kernel.as[ScalaFunction`(`Int`,`Array[Int]`)=>`Array[Int]].withSizes(1, 1)
    val xs = Array.fill(8)(0)

    val (result, time) = kernelF(8`,`xs)
    println(time)

    val gold = Array.fill(8)(1)
    assertResult(gold)(result)
  }

  test("Running a simple kernel with fixed input size") {
    val n = 8
    val f: Expr =
      fun(ArrayType(n, int))(xs => xs :>> mapSeq(fun(x => x + 1)))

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(f, Map())))
    println(kernel.code)
    SyntaxChecker.checkOpenCL(kernel.code)

    val kernelF = kernel.as[ScalaFunction`(`Array[Int]`)=>`Array[Int]].withSizes(1, 1)
    val xs = Array.fill(n)(0)

    val (result, time) = kernelF(xs`;`)
    println(time)

    val gold = Array.fill(n)(1)
    assertResult(gold)(result)
  }

  test("Running a simple kernel with nat-dependent split") {
    val n = 8
    val f: Expr =
      fun(ArrayType(n, int))(xs => nFun(s => xs :>> split(s) :>> mapSeq(mapSeq(fun(x => x + 1))) :>> join))

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(f, Map())))
    println(kernel.code)
    SyntaxChecker.checkOpenCL(kernel.code)

    val kernelF = kernel.as[ScalaFunction`(`Array[Int]`,`Int`)=>`Array[Int]]
    val xs = Array.fill(n)(0)

    val (result, _) = kernelF(1, 1)(xs`,`n)

    val gold = Array.fill(n)(1)
    assertResult(gold)(result)
  }

  test("Running a simple kernel with multiple generic input sizes") {
    val m = 4
    val n = 8
    val f: Expr =
      nFun((m, n) =>
          fun(ArrayType(m, ArrayType(n, int)))(xs =>
            xs :>> mapSeq(mapSeq(fun(x => x + 1)))))

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(f, Map())))
    println(kernel.code)
    SyntaxChecker.checkOpenCL(kernel.code)

    val kernelF = kernel.as[ScalaFunction`(`Int`,`Int`,`Array[Array[Int]]`)=>`Array[Int]].withSizes(1, 1)
    val xs = Array.fill(m)(Array.fill(n)(0))

    val (result, time) =  kernelF(m`,`n`,`xs)
    println(time)

    val gold = Array.fill(m)(Array.fill(n)(1)).flatten
    assertResult(gold)(result)
  }

  test("Running a simple kernel mixing nat-dependent with normal functions") {
    val n = 8
    val s = 2
    val f: Expr =
      nFun(n =>
        fun(ArrayType(n, int))(xs =>
          nFun(s =>
            xs :>> split(s) :>> mapSeq(mapSeq(fun(x => x + 1))) :>> join())))

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(f, Map())))
    println(kernel.code)
    SyntaxChecker.checkOpenCL(kernel.code)

    val kernelF = kernel.as[ScalaFunction`(`Int`,`Array[Int]`,`Int`)=>`Array[Int]]

    val xs = Array.fill(n)(2)
    val (result, _) =  kernelF(1,1)(n`,`xs`,`s)

    val gold = xs.map(x => x + 1)
    assertResult(gold)(result)
  }

  test("Running a simple kernel with fixed input size and multiple threads") {
    val n = 8
    val f: Expr =
      fun(ArrayType(n, int))(xs => xs :>> mapGlobal(fun(x => x + 1)))

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(f, Map())))
    println(kernel.code)
    SyntaxChecker.checkOpenCL(kernel.code)

    val kernelF = kernel.as[ScalaFunction`(`Array[Int]`)=>`Array[Int]].withSizes(1, 1)
    val xs = Array.fill(n)(0)

    val (result, time) =  kernelF(xs`;`)
    println(time)

    val gold = Array.fill(n)(1)
    assertResult(gold)(result)
  }
}
