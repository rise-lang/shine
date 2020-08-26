package apps

import rise.core.DSL._
import rise.core.types._
import rise.openCL.DSL._
import shine.OpenCL.get_global_size
import rise.core.TypeLevelDSL._
import rise.core.HighLevelConstructs.reorderWithStride
import util.gen

class test extends shine.test_util.Tests {

  val abs =
    dtFun(t => foreignFun("my_abs", Seq("y"), "{ return fabs(y); }", t ->: t))

  private def xsT(N: NatIdentifier) = ArrayType(N, f32)

  test("VectorScal test") {
    val vectorScalTest = nFun(n => fun(f32)(alpha => fun(xsT(n))(xs =>
      xs |>
      split(1024) |>
      mapWorkGroup(
        split(4) >>
        mapLocal(mapSeq(fun(x => alpha * x))) >>
        join
      ) |> join
    )))

    gen.OpenCLKernel(vectorScalTest)
  }

  test("Nvidia Absolute Sum test") {
    val nvidiaAbsSumTest = nFun(n => fun(xsT(n))(xs =>
      xs |>
      split(2048) |>
      split(128) |>
      mapWorkGroup(
        reorderWithStride(2048) >>
        mapLocal(
          oclReduceSeq(AddressSpace.Private)(
            fun(a => fun(x => a + abs(f32)(x)))
          )(l(0.0f))
        )
      ) |>
        toGlobal |>
      join |>
        oclReduceSeq(AddressSpace.Private)(
          fun(a => fun(x => a + abs(f32)(x)))
        )(l(0.0f))
    ))

    gen.OpenCLKernel(nvidiaAbsSumTest)
  }

  test("Loop test") {
    val loopTest = nFun(n => fun(xsT(n))(in =>
      in |>
        split(2048) |>
        transpose |>
        mapWorkGroup(
          split(128) >>
            transpose >>
          mapLocal(
          oclReduceSeq(AddressSpace.Private)(
            fun(a => fun(x => a + x))
          )(l(0.0f))
        )) /* |>
        toGlobal |>
        split(128) |>
        mapWorkGroup(
          toLocalFun(
            mapLocal(fun(x => x))
          ) >>
            toLocalFun(
              oclIterate(AddressSpace.Local)(6)(
                nFun(_ =>
                  split(2) >> mapLocal(
                    oclReduceSeq(AddressSpace.Private)(add)(l(0.0f))
                  )
                )
              )
            ) >>
            mapLocal(fun(x => x))
        ) |>
        join*/
    ))

    gen.OpenCLKernel(loopTest)
  }

  test("Old Loop test") {
    val oldLoopTest = nFun(n => fun(xsT(n))(in =>
      in |>
        split(get_global_size(0)) |>
        transpose |>
        mapGlobal(
          oclReduceSeq(AddressSpace.Private)(
            fun(a => fun(x => a + x))
          )(l(0.0f))
        ) |>
        toGlobal |>
        mapLocal(
          fun(x => x)
        ) |>
        toLocal |>
        mapLocal(
          fun(x => x)
        )
    ))

    gen.OpenCLKernel(oldLoopTest)
  }

}
