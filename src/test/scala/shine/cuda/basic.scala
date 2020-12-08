package shine.cuda

import rise.core.freshName
import shine.DPIA.FunctionalPrimitives.{Split, Transpose, Zip}
import shine.DPIA.NatIdentifier
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.OpenCL.FunctionalPrimitives.MapGlobal
import shine.cuda.primitives.functional.MapThreads

class basic extends test_util.Tests {

  test("id with mapGlobal compiles to syntactically correct Cuda") {
    val n = NatIdentifier("n")
    val arr = Identifier("arr", ExpType(ArrayType(n, f32), read))
    val x = Identifier("x", ExpType(f32, read))
    val mapId =
      DepLambda[NatKind](n)(
        Lambda(arr,
          MapThreads('x')(n, f32, f32, Lambda(x, x), arr)
      ))

    val code = KernelGenerator().makeCode(mapId).code
    println(code)
  }

  test("id with mapGlobal compiles to syntactically correct CUDA") {
    val n = NatIdentifier("n")
    val arr = Identifier("arr", ExpType(ArrayType(n, f32), read))
    val x = Identifier("x", ExpType(f32, read))
    val mapId =
      DepLambda[NatKind](n)(
        Lambda(arr,
          MapGlobal('x')(n, f32, f32, Lambda(x, x), arr)
        ))

    val code = KernelGenerator().makeCode(mapId).code
    println(code)
  }
}
