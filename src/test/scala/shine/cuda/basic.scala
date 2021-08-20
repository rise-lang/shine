package shine.cuda

import rise.core.types.DataType._
import rise.core.types._
import shine.DPIA.DSL.{depFun, fun}
import shine.DPIA.Types.ExpType
import shine.OpenCL.{Global, Local}
import util.gen

class basic extends test_util.Tests {

  test("id with mapThreads compiles to syntactically correct Cuda") {
    val mapId = depFun(NatKind)(n =>
      fun(ExpType(ArrayType(n, f32), read))(array =>
        shine.cuda.primitives.functional.Map(Local, 'x')(n, f32, f32, fun(ExpType(f32, read))(x => x), array))
    )

    val code = gen.cuda.kernel.asStringFromPhrase(mapId)
    logger.debug(code)
  }

  test("id with mapGlobal compiles to syntactically correct CUDA") {
    val mapId = depFun(NatKind)(n =>
      fun(ExpType(ArrayType(n, f32), read))(array =>
        shine.cuda.primitives.functional.Map(Global, 'x')(n, f32, f32, fun(ExpType(f32, read))(x => x), array))
    )

    val code = gen.cuda.kernel.asStringFromPhrase(mapId)
    logger.debug(code)
  }
}
