/*
package idealised.DPIA

import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.OpenCL._
import idealised.OpenMP.SurfaceLanguage.DSL.mapPar
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

import scala.language.{postfixOps, reflectiveCalls}

class NatDepLambdaGen extends idealised.util.TestsWithExecutor {
  val id = fun(x => x)

  test("Generate code for top-level nat-dependent lambdas in OpenCL") {
    val natDepProg =
      nFun((x, y, n) =>
        fun(ArrayType(n, float))(in => in :>> split(x) :>> map(split(y)) :>> mapGlobal(mapSeq(mapSeq(id)))))

    val compiledProg =
      idealised.OpenCL.KernelGenerator.makeCode(LocalSize(8), GlobalSize(32))(idealised.DPIA.FromSurfaceLanguage(TypeInference(natDepProg, Map())))
    println(compiledProg.code)
    SyntaxChecker.checkOpenCL(compiledProg.code)
  }

  test("Generate code for top-level nat-dependent lambdas in C") {
   val natDepProg =
    nFun((x, y, n) =>
      fun(ArrayType(n, float))(in => in :>> split(x) :>> map(split(y)) :>>
        mapSeq(mapSeq(mapSeq(id))) :>> join() :>> join))

    val compiledProg =
      idealised.C.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(natDepProg, Map())))
    println(compiledProg.code)
    SyntaxChecker(compiledProg.code)
  }

  test("Generate code for top-level nat-dependent lambdas in OpenMP") {
   val natDepProg =
     nFun((n, x, y) =>
           fun(ArrayType(n, float))(in =>
             in :>> split(x) :>> map(split(y)) :>> mapPar(mapSeq(mapSeq(id)))))

    val compiledProg =
      idealised.OpenMP.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(natDepProg, Map())))
    println(compiledProg.code)
    SyntaxChecker(compiledProg.code)
  }

  ignore("Generates correct code for local memory transpose in OpenCL") {
    val id = fun(x => x)

    val tile =
      nFun((rows, columns) =>
          map(map(transpose()) o split(columns) o transpose()) o split(rows))

    //val untile2D = join() o map(map(join()) o transposeW())

    val prog =
      nFun(m => nFun(n =>
        nFun(tileRows =>
          nFun(tileColumns =>
            fun(ArrayType(m, ArrayType(n, float)))(x =>
              x :>> tile(tileRows)(tileColumns) :>>
                mapWorkgroup(1)(mapWorkgroup(0)(fun(tile =>
                  tile :>>
                    mapLocal(1)(mapLocal(0)(fun(x => toLocal(x)))) :>>
                    transpose() :>>
                    fun(x => toGlobal(x)) :>>
                transpose()))))))))// :>> untile2D)))

    val kernel =
      idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(prog, Map())))
    println(kernel.code)

    SyntaxChecker.checkOpenCL(kernel.code)

    val kernelF = kernel
      .as[ScalaFunction`(`Int`,`Int`,`Int`,`Int`,`Array[Array[Float]]`)=>`Array[Float]]
    val M = 4
    val N = 4
    val mSplit = 2
    val nSplit = 2
    val xs = Array.tabulate(M)(i => Array.fill(N)(1.0f * i))

    kernelF(LocalSize(1),GlobalSize(1))((M`;`) `,` N `,` mSplit `,` nSplit `,` xs)
  }
}
 */