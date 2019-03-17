package idealised.DPIA

import idealised.SurfaceLanguage.DSL._
import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.OpenMP.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

class StructDecl extends idealised.util.Tests {
  val id = fun(x => x)
  val addT = fun(t => t._1 + t._2)

  test("Program with tuples in output and tuple input, can be generated in C.") {
    val tupleOut = fun(ArrayType(8, TupleType(float, float)))(xs =>
      xs :>> mapSeq(id)
    )

    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(tupleOut, Map()))
    val program = idealised.C.ProgramGenerator.makeCode(phrase)
    println(program.code)
    SyntaxChecker(program.code)
  }

  test("Program with tuples in output and input, can be generated in OpenMP.") {
    val tupleOut = fun(ArrayType(8, TupleType(float, float)))(xs =>
      xs :>> mapPar(id)
    )

    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(tupleOut, Map()))
    val program = idealised.OpenMP.ProgramGenerator.makeCode(phrase)
    println(program.code)
    SyntaxChecker(program.code)
  }

  test("Program with tuples in input and output, can be generated in OpenCL.") {
    val tupleOut = fun(ArrayType(8, TupleType(float, float)))(xs =>
      xs :>> mapSeq(id)
    )

    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(tupleOut, Map()))
    val program = idealised.OpenCL.KernelGenerator.makeCode(phrase)
    println(program.code)
    SyntaxChecker.checkOpenCL(program.code)
  }

  test("Program using intermediary tuples but not in input or output, can be generated in C.") {
    val tupleOut = fun(ArrayType(8, float))(xs =>
      fun(ArrayType(8, float))(ys =>
        zip(xs, ys) :>> mapSeq(id) :>> mapSeq(addT)))

    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(tupleOut, Map()))
    val program = idealised.C.ProgramGenerator.makeCode(phrase)
    println(program.code)
    SyntaxChecker(program.code)
  }

  test("Program using intermediary tuples but not in input or output, can be generated in OpenMP.") {
    val tupleOut = fun(ArrayType(8, float))(xs =>
      fun(ArrayType(8, float))(ys =>
        zip(xs, ys) :>> mapPar(id) :>> mapPar(addT)))

    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(tupleOut, Map()))
    val program = idealised.OpenMP.ProgramGenerator.makeCode(phrase)
    println(program.code)
    SyntaxChecker(program.code)
  }

  test("Program using intermediary tuples but not in input or output, can be generated in OpenCL.") {
    val tupleOut = fun(ArrayType(8, float))(xs =>
      fun(ArrayType(8, float))(ys =>
        zip(xs, ys) :>> toGlobal(mapSeq(id)) :>> mapSeq(addT)))

    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(tupleOut, Map()))
    val program = idealised.OpenCL.KernelGenerator.makeCode(phrase)
    println(program.code)
    SyntaxChecker.checkOpenCL(program.code)
  }
}
