package shine.cuda

import shine.DPIA.NatIdentifier
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.OpenCL.{Global, Local}
import util.gen

class basic extends test_util.Tests {

  test("id with mapGlobal compiles to syntactically correct Cuda") {
    val n = NatIdentifier("n")
    val arr = Identifier("arr", ExpType(ArrayType(n, f32), read))
    val x = Identifier("x", ExpType(f32, read))
    val mapId =
      DepLambda[NatKind](n)(
        Lambda(arr,
          shine.cuda.primitives.functional.Map(Local, 'x')(n, f32, f32, Lambda(x, x), arr)
      ))

    val code = gen.cuda.kernel.asStringFromPhrase(mapId)
    println(code)
  }

  test("id with mapGlobal compiles to syntactically correct CUDA") {
    val n = NatIdentifier("n")
    val arr = Identifier("arr", ExpType(ArrayType(n, f32), read))
    val x = Identifier("x", ExpType(f32, read))
    val mapId =
      DepLambda[NatKind](n)(
        Lambda(arr,
          shine.cuda.primitives.functional.Map(Global, 'x')(n, f32, f32, Lambda(x, x), arr)
        ))

    val code = gen.cuda.kernel.asStringFromPhrase(mapId)
    println(code)
  }
}
