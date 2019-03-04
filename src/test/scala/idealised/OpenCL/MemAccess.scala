package idealised.OpenCL

import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Semantics.FloatData
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import lift.arithmetic.{?, SizeVar}

import scala.language.postfixOps
import scala.language.reflectiveCalls

class MemAccess extends idealised.util.TestsWithExecutor {
  def printSyntaxCheckAnd[T <: Type](exec: Kernel => Array[Float], prog: Expr[T]): Array[Float] = {
     val kernel = idealised.OpenCL.KernelGenerator
      .makeCode(TypeInference(prog, Map()).toPhrase, 8, 32)
    println(kernel.code)
    SyntaxChecker.checkOpenCL(kernel.code)

    exec(kernel)
  }

  def runWithMatrixInput(kernel: Kernel): Array[Float] = {
    val input = Array.tabulate(8, 8) {(i, j) => 1.0f * i}
    val kernelFun = kernel.as[ScalaFunction `(` Array[Array[Float]] `)=>` Array[Float]]
    kernelFun(input `;`)._1
  }

  def runWithVectorInput(kernel: Kernel): Array[Float] = {
    val input = Array.tabulate(8) { i => 1.0f * i}
    val kernelFun = kernel.as[ScalaFunction `(` Array[Float] `)=>` Array[Float]]
    kernelFun(input `;`)._1
  }

  val M = SizeVar("M")
  val N = SizeVar("N")
  val id = fun(x => x)
  val incr = fun(x => x + LiteralExpr(FloatData(1.0f)))

  ignore("mapSeq copying to global memory, race condition is caught in OpenCL") {
    val prog = fun(ArrayType(N, float))(x => x :>> mapSeq(id))

    val output = printSyntaxCheckAnd(runWithVectorInput, prog)
    println(output)
    fail()
  }

  test("mapGlobal over local memory, race condition is caught in OpenCL") {
    val prog = fun(ArrayType(N, float))(x => x :>> mapGlobal(toLocal(id)))

    assertThrows[Exception] {
      printSyntaxCheckAnd(runWithVectorInput, prog)
    }
  }

  ignore("mapWorkgroup followed by another map wrapped in toLocal cannot be generated") {
    val prog = fun(ArrayType(M, ArrayType(N, float)))(x =>
      x :>>
        toLocal(mapWorkgroup(toLocal(mapLocal(id)))) :>>
          mapWorkgroup(toGlobal(mapLocal(id))))

    val output = printSyntaxCheckAnd(runWithMatrixInput, prog)
    println(output)

    fail()
  }

  ignore("map matrix rows to local memory, illegal access after transpose is caught in OpenCL") {
    val prog = fun(ArrayType(M, ArrayType(N, float)))(x =>
      x :>>
        mapWorkgroup(toLocal(mapLocal(id))) :>>
          transpose() :>>
            mapWorkgroup(toGlobal(mapLocal(id))))

    val output = printSyntaxCheckAnd(runWithMatrixInput, prog)
    println(output)

    fail()
  }

  ignore("mapGlobal matrix to private memory, followed by join is rejected (copying should be explicit)," +
    "illegal access after gather is caught in OpenCL") {
    val prog = fun(ArrayType(M, ArrayType(N, float)))(x =>
      x :>>
        mapGlobal(1)(mapGlobal(0)(toPrivate(incr))) :>>
          join())

    val output = printSyntaxCheckAnd(runWithMatrixInput, prog)
    println(output)
    fail()
  }

  ignore("Generates correct code for local memory transpose in OpenCL") {
    val tile =
      dFun((rows : NatIdentifier) =>
        dFun((columns : NatIdentifier) =>
          map(map(transpose()) o split(columns) o transpose()) o split(rows)))

    val untile2D = join() o map(map(join()) o transpose())

    val prog =
      dFun((tileRows : NatIdentifier) =>
        dFun((tileColumns : NatIdentifier) =>
          fun(ArrayType(M, ArrayType(N, float)))(x =>
            x :>> tile(tileRows)(tileColumns) :>>
              mapWorkgroup(1)(mapWorkgroup(0)(fun(tile =>
                tile :>>
                  toLocal(mapLocal(1)(mapLocal(0)(id))) :>>
                    transpose() :>>
                      mapLocal(1)(mapLocal(0)(id))))) :>>
              map(transpose()) :>> untile2D)))

    val output = printSyntaxCheckAnd(runWithMatrixInput, prog)
  }
}
