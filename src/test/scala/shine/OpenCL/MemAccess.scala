/* TODO
package idealised.OpenCL

import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Semantics.FloatData
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import idealised.util.SyntaxChecker
import lift.arithmetic.SizeVar

import scala.language.{postfixOps, reflectiveCalls}

class MemAccess extends idealised.util.TestsWithExecutor {
  def printSyntaxCheckAnd(exec: KernelNoSizes => Array[Float], prog: Expr): Array[Float] = {
     val kernel = idealised.OpenCL.KernelGenerator
      .makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(prog, Map())))
    logger.debug(kernel.code)
    SyntaxChecker.checkOpenCL(kernel.code)

    exec(kernel)
  }

  def runWithMatrixInput(kernel: KernelNoSizes): Array[Float] = {
    val input = Array.tabulate(8, 8) {(i, j) => 1.0f * i}
    val kernelFun = kernel.as[ScalaFunction `(` Array[Array[Float]] `)=>` Array[Float]]
    kernelFun(LocalSize(4), GlobalSize(8))(input `;`)._1
  }

  def runWithVectorInput(kernel: KernelNoSizes): Array[Float] = {
    val input = Array.tabulate(8) { i => 1.0f * i}
    val kernelFun = kernel.as[ScalaFunction `(` Array[Float] `)=>` Array[Float]]
    kernelFun(LocalSize(4), GlobalSize(8))(input `;`)._1
  }

  val M = SizeVar("M")
  val N = SizeVar("N")
  val id = fun(x => x)
  val incr = fun(x => x + LiteralExpr(FloatData(1.0f)))

  ignore("mapSeq copying to global memory, race condition is caught in OpenCL") {
    val prog = fun(ArrayType(N, float))(x => x :>> mapSeq(id))

    val output = printSyntaxCheckAnd(runWithVectorInput, prog)
    logger.debug(output)
    fail()
  }

  ignore("mapGlobal over local memory, race condition is caught in OpenCL") {
    // TODO: @Bastian.Koepcke: why is this now an illegal program?
    val prog = fun(ArrayType(N, float))(x => x :>> mapGlobal(fun(x => toLocal(x))))

    assertThrows[Exception] {
      printSyntaxCheckAnd(runWithVectorInput, prog)
    }
  }

  ignore("mapWorkgroup followed by another map wrapped in toLocal cannot be generated") {
    val prog = fun(ArrayType(M, ArrayType(N, float)))(x =>
      x :>>
        mapWorkgroup(fun(x => toLocal(mapLocal(id, x)))) :>>
         fun(x => toLocal(x)) :>>
          mapWorkgroup(fun(x => toGlobal(mapLocal(id, x)))))

    val output = printSyntaxCheckAnd(runWithMatrixInput, prog)
    logger.debug(output)

    fail()
  }

  ignore("map matrix rows to local memory, illegal access after transpose is caught in OpenCL") {
    val prog = fun(ArrayType(M, ArrayType(N, float)))(x =>
      x :>>
        mapWorkgroup(fun(x => toLocal(mapLocal(id, x)))) :>>
          transpose() :>>
            mapWorkgroup(fun(x => toGlobal(mapLocal(id, x)))))

    val output = printSyntaxCheckAnd(runWithMatrixInput, prog)
    logger.debug(output)

    fail()
  }

  ignore("mapGlobal matrix to private memory, followed by join is rejected (copying should be explicit)," +
    "illegal access after gather is caught in OpenCL") {
    val prog = fun(ArrayType(M, ArrayType(N, float)))(x =>
      x :>>
        mapGlobal(1)(mapGlobal(0)(fun(x => toPrivate(incr(x))))) :>>
          join())

    val output = printSyntaxCheckAnd(runWithMatrixInput, prog)
    logger.debug(output)
    fail()
  }

  ignore("Generates correct code for local memory transpose in OpenCL") {
    val tile =
      nFun(rows =>
        nFun(columns =>
          map(map(transpose()) o split(columns) o transpose()) o split(rows)))

    val untile2D = join() o map(map(join()) o transpose())

    val prog =
      nFun(tileRows =>
        nFun(tileColumns=>
          fun(ArrayType(M, ArrayType(N, float)))(x =>
            x :>> tile(tileRows)(tileColumns) :>>
              mapWorkgroup(1)(mapWorkgroup(0)(fun(tile =>
                tile :>>
                  fun(x => toLocal(mapLocal(1)(mapLocal(0)(id))(x))) :>>
                    transpose() :>>
                      mapLocal(1)(mapLocal(0)(id))))) :>>
              map(transpose()) :>> untile2D)))

    printSyntaxCheckAnd(runWithMatrixInput, prog)
  }
}
 */