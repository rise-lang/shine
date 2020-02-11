package shine.DPIA

import rise.core.DSL._
import rise.core.types._
import rise.OpenCL.DSL._
import rise.OpenMP.DSL._
import util.gen

class StructDecl extends shine.test_util.Tests {
  val id = fun(x => x)
  val addT = fun(t => t._1 + t._2)

  test("Program with tuples in output and tuple input, can be generated in C.") {
    val tupleOut = fun(ArrayType(8, PairType(f32, f32)))(xs => xs |> mapSeq(id))

    gen.CProgram(tupleOut)
  }

  test("Program with tuples in output and input, can be generated in OpenMP.") {
    val tupleOut = fun(ArrayType(8, PairType(f32, f32)))(xs => xs |> mapPar(id))

    gen.OpenMPProgram(tupleOut)
  }

  test("Program with tuples in input and output, can be generated in OpenCL.") {
    val tupleOut = fun(ArrayType(8, PairType(f32, f32)))(xs => xs |> mapSeq(id))

    gen.OpenCLKernel(tupleOut)
  }

  test(
    "Program using intermediary tuples but not in input or output, can be generated in C."
  ) {
    val tupleOut = fun(ArrayType(8, f32))(xs =>
      fun(ArrayType(8, f32))(ys => zip(xs, ys) |> mapSeq(id) |> mapSeq(addT))
    )

    gen.CProgram(tupleOut)
  }

  test(
    "Program using intermediary tuples but not in input or output, can be generated in OpenMP."
  ) {
    val tupleOut = fun(ArrayType(8, f32))(xs =>
      fun(ArrayType(8, f32))(ys => zip(xs, ys) |> mapPar(id) |> mapPar(addT))
    )

    gen.OpenMPProgram(tupleOut)
  }

  test(
    "Program using intermediary tuples but not in input or output, can be generated in OpenCL."
  ) {
    val tupleOut = fun(ArrayType(8, f32))(xs =>
      fun(ArrayType(8, f32))(ys =>
        zip(xs, ys) |> mapSeq(id) |> fun(x => toGlobal(x)) |> mapSeq(addT)
      )
    )

    gen.OpenCLKernel(tupleOut)
  }
}
