package idealised.apps

import idealised.OpenCL._
import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.OpenMP.SurfaceLanguage.DSL.mapPar
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import idealised.util.SyntaxChecker
import lift.arithmetic.SizeVar

import scala.language.postfixOps
import scala.language.reflectiveCalls

class transpose extends idealised.util.TestsWithExecutor {
  val id = fun(x => x)


  test("Generate code for top-level nat-dependent lambdas in OpenCL") {
    val natDepProg =
      dFun((x : NatIdentifier) =>
        dFun((y : NatIdentifier) =>
          dFun((n : NatIdentifier) =>
            fun(ArrayType(n, float))(in => in :>> split(x) :>> map(split(y)) :>> mapGlobal(mapSeq(mapSeq(id)))))))

    val compiledProg =
      idealised.OpenCL.KernelGenerator.makeCode(8, 32)(TypeInference(natDepProg, Map()).toPhrase)
    println(compiledProg.code)
    SyntaxChecker.checkOpenCL(compiledProg.code)
  }

  test("Generate code for top-level nat-dependent lambdas in C") {
   val natDepProg =
    dFun((x : NatIdentifier) =>
      dFun((y : NatIdentifier) =>
        dFun((n : NatIdentifier) =>
          fun(ArrayType(n, float))(in => in :>> split(x) :>> map(split(y)) :>>
            mapSeq(mapSeq(mapSeq(id))) :>> join() :>> join))))

    val compiledProg =
      idealised.C.ProgramGenerator.makeCode(TypeInference(natDepProg, Map()).toPhrase)
    println(compiledProg.code)
    SyntaxChecker(compiledProg.code)
  }

  test("Generate code for top-level nat-dependent lambdas in OpenMP") {
   val natDepProg =
     dFun((n: NatIdentifier) =>
       dFun((x : NatIdentifier) =>
         dFun((y : NatIdentifier) =>
           fun(ArrayType(n, float))(in =>
             in :>> split(x) :>> map(split(y)) :>> mapPar(mapSeq(mapSeq(id)))))))

    val compiledProg =
      idealised.OpenMP.ProgramGenerator.makeCode(TypeInference(natDepProg, Map()).toPhrase)
    println(compiledProg.code)
    SyntaxChecker(compiledProg.code)
  }

  ignore("Generates correct code for local memory transpose in OpenCL") {
    val id = fun(x => x)

    val tile =
      dFun((rows : NatIdentifier) =>
        dFun((columns : NatIdentifier) =>
          map(map(transpose()) o split(columns) o transpose()) o split(rows)))

    val untile2D = join() o map(map(join()) o transposeW())

    val prog =
      dFun((m : NatIdentifier) => dFun((n : NatIdentifier) =>
        dFun((tileRows : NatIdentifier) =>
          dFun((tileColumns : NatIdentifier) =>
            fun(ArrayType(m, ArrayType(n, float)))(x =>
              x :>> tile(tileRows)(tileColumns) :>>
                mapWorkgroup(1)(mapWorkgroup(0)(fun(tile =>
                  tile :>>
                    toGlobal(mapLocal(1)(mapLocal(0)(toLocal(id))) >>>
                      transpose()) :>>
                transposeW()))))))))// :>> untile2D)))

    val kernel =
      idealised.OpenCL.KernelGenerator.makeCode(TypeInference(prog, Map()).toPhrase)
    println(kernel.code)

    SyntaxChecker.checkOpenCL(kernel.code)

    val kernelF = kernel
      .as[ScalaFunction`(`Int`,`Int`,`Int`,`Int`,`Array[Array[Float]]`)=>`Array[Float]](1, 1)
    val M = 4
    val N = 4
    val mSplit = 2
    val nSplit = 2
    val xs = Array.tabulate(M)(i => Array.fill(N)(1.0f * i))

    val (result, time) =  kernelF((M`;`) `,` N `,` mSplit `,` nSplit `,` xs)
  }
}
