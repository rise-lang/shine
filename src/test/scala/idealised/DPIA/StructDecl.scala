package idealised.DPIA

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import lift.OpenCL.DSL._
import lift.OpenMP.DSL._
import util.gen

class StructDecl extends test_util.Tests {
  val id = fun(x => x)
  val addT = fun(t => t._1 + t._2)

  test("Program with tuples in output and tuple input, can be generated in C.") {
    val tupleOut = fun(ArrayType(8, TupleType(float, float)))(xs =>
      xs |> mapSeq(id)
    )

    gen.CProgram(tupleOut)
  }

  test("Program with tuples in output and input, can be generated in OpenMP.") {
    val tupleOut = fun(ArrayType(8, TupleType(float, float)))(xs =>
      xs |> mapPar(id)
    )

    gen.OpenMPProgram(tupleOut)
  }

  test("Program with tuples in input and output, can be generated in OpenCL.") {
    val tupleOut = fun(ArrayType(8, TupleType(float, float)))(xs =>
      xs |> mapSeq(id)
    )

    gen.OpenCLKernel(tupleOut)
  }

  test("Program using intermediary tuples but not in input or output, can be generated in C.") {
    val tupleOut = fun(ArrayType(8, float))(xs =>
      fun(ArrayType(8, float))(ys =>
        zip(xs, ys) |> mapSeq(id) |> mapSeq(addT)))

    gen.CProgram(tupleOut)
  }

  test("Program using intermediary tuples but not in input or output, can be generated in OpenMP.") {
    val tupleOut = fun(ArrayType(8, float))(xs =>
      fun(ArrayType(8, float))(ys =>
        zip(xs, ys) |> mapPar(id) |> mapPar(addT)))

    gen.OpenMPProgram(tupleOut)
  }

  test("Program using intermediary tuples but not in input or output, can be generated in OpenCL.") {
    val tupleOut = fun(ArrayType(8, float))(xs =>
      fun(ArrayType(8, float))(ys =>
        zip(xs, ys) |> mapSeq(id) |> fun(x => toGlobal(x)) |> mapSeq(addT)))

    gen.OpenCLKernel(tupleOut)
  }
}
