package idealised.OpenCL

import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.LiteralExpr
import idealised.SurfaceLanguage.Semantics.FloatData
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import lift.arithmetic.{?, SizeVar}

import scala.language.postfixOps

class MemAccess extends idealised.util.TestsWithExecutor {
  test("mapSeq copying to global memory, race condition is caught in OpenCL") {
    val N = SizeVar("N")
    val id = fun(x => x)
    val prog = fun(ArrayType(N, float))(x => x :>> mapSeq(id))

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(prog, Map()).toPhrase, 32, 32)
    println(kernel.code)
    SyntaxChecker.checkOpenCL(kernel.code)

    val input = Array.tabulate(8) { i => 1.0f * i}
    val kernelFun = kernel.as[ScalaFunction `(` Array[Float] `)=>` Array[Float]]
    val (output, _) = kernelFun(input `;`)

    fail()
  }

  test("mapGlobal over local memory, race condition is caught in OpenCL") {
    val N = SizeVar("N")
    val id = fun(x => x)
    val prog = fun(ArrayType(N, float))(x => x :>> toLocal(mapGlobal(id)))

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(prog, Map()).toPhrase, ?, ?)
    println(kernel.code)
    SyntaxChecker.checkOpenCL(kernel.code)

    val input = Array.tabulate(8) {i => 1.0f * i}
    val kernelFun = kernel.as[ScalaFunction `(` Array[Float] `)=>` Array[Float]]
    val (output, _) = kernelFun(input `;`)

    fail()
  }

  test("map matrix rows to local memory, illegal access after transpose is caught in OpenCL") {
    val M = SizeVar("M")
    val N = SizeVar("N")
    val id = fun(x => x)
    val prog = fun(ArrayType(M, ArrayType(N, float)))(x =>
      x :>>
        toLocal(mapWorkgroup(mapLocal(id))) :>>
          transpose() :>>
            mapWorkgroup(mapLocal(id)))

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(prog, Map()).toPhrase, 4, 8)
    println(kernel.code)
    SyntaxChecker.checkOpenCL(kernel.code)

    val input = Array.tabulate(8, 8) {(i, j) => 1.0f * i}
    val kernelFun = kernel.as[ScalaFunction `(` Array[Array[Float]] `)=>` Array[Float]]
    val (output, _) = kernelFun(input `;`)

    fail()
  }

  test("mapGlobal matrix to private memory, illegal access after gather is caught in OpenCL") {
    val M = SizeVar("M")
    val N = SizeVar("N")
    val id = fun(x => x)
    val incr = fun(x => x + LiteralExpr(FloatData(1.0f)))
    val prog = fun(ArrayType(M, ArrayType(N, float)))(x =>
      x :>>
        toLocal(mapGlobal(1)(mapGlobal(0)(toPrivate(incr) o toPrivate(incr) o toPrivate(incr)))) :>>
          transpose() :>>
            mapGlobal(1)(mapGlobal(0)(id)))

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(prog, Map()).toPhrase, ?, ?)
    println(kernel.code)
    SyntaxChecker.checkOpenCL(kernel.code)

    val input = Array.tabulate(8) {i => 1.0f * i}
    val kernelFun = kernel.as[ScalaFunction `(` Array[Float] `)=>` Array[Float]]
    val (output, _) = kernelFun(input `;`)

    fail()
  }
}
