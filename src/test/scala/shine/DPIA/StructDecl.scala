package shine.DPIA

import rise.core.DSL._
import rise.core.primitives._
import rise.core.types.DataType._
import rise.openCL.DSL._
import rise.openMP.primitives._
import util.gen
import util.gen.c.function

class StructDecl extends test_util.Tests {
  val id = fun(x => x)
  val addT = fun(t => t.`1` + t.`2`)

  test("Program with tuples in output and tuple input, can be generated in C.") {
    val tupleOut = fun(ArrayType(8, PairType(f32, f32)))(xs => xs |> mapSeq(id))

    function.asStringFromExpr(tupleOut)
  }

  test("Program with tuples in output and input, can be generated in OpenMP.") {
    val tupleOut = fun(ArrayType(8, PairType(f32, f32)))(xs => xs |> mapPar(id))

    gen.openmp.function.asStringFromExpr(tupleOut)
  }

  test("Program with tuples in input and output, can be generated in OpenCL.") {
    val tupleOut = fun(ArrayType(8, PairType(f32, f32)))(xs => xs |> mapSeq(id))

    gen.opencl.kernel.fromExpr(tupleOut)
  }

  test(
    "Program using intermediary tuples but not in input or output, can be generated in C."
  ) {
    val tupleOut = fun(ArrayType(8, f32))(xs =>
      fun(ArrayType(8, f32))(ys =>
        zip(xs)(ys) |> toMemFun(mapSeq(id)) |> mapSeq(addT))
    )

    function.asStringFromExpr(tupleOut)
  }

  test(
    "Program using intermediary tuples but not in input or output, can be generated in OpenMP."
  ) {
    val tupleOut = fun(ArrayType(8, f32))(xs =>
      fun(ArrayType(8, f32))(ys =>
        zip(xs)(ys) |> toMemFun(mapPar(id)) |> mapPar(addT))
    )

    gen.openmp.function.asStringFromExpr(tupleOut)
  }

  test(
    "Program using intermediary tuples but not in input or output, can be generated in OpenCL."
  ) {
    val tupleOut = fun(ArrayType(8, f32))(xs =>
      fun(ArrayType(8, f32))(ys =>
        zip(xs)(ys) |> mapSeq(id) |> fun(x => toGlobal(x)) |> mapSeq(addT)
      )
    )

    gen.opencl.kernel.fromExpr(tupleOut)
  }
}
