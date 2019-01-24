package idealised.apps

import idealised.OpenCL.SurfaceLanguage.DSL.depMapGlobal
import idealised.OpenMP.SurfaceLanguage.DSL.depMapPar
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import idealised.util.SyntaxChecker
import lift.arithmetic.?

import scala.language.{implicitConversions, postfixOps}

class triangleVectorMult extends idealised.util.Tests {

  val mult = fun(x => x._1 * x._2)

  val add = fun(x => fun(y => x + y))

  val triangleVectorMultSeq: Expr[DataType -> (DataType -> DataType)] =
    fun(DepArrayType(8, i => ArrayType(i + 1, int)))(triangle =>
      fun(ArrayType(8, int))(vector =>
        depMapSeq(fun(row => zip(row, take(Macros.GetLength(row), vector))
          :>> mapSeq(mult) :>> reduceSeq(add, 0)
        ), triangle)
      )
    )

  val triangleVectorMultPar: Expr[DataType -> (DataType -> DataType)] =
    fun(DepArrayType(8, i => ArrayType(i + 1, int)))(triangle =>
      fun(ArrayType(8, int))(vector =>
        depMapPar(fun(row => zip(row, take(Macros.GetLength(row), vector))
          :>> mapSeq(mult) :>> reduceSeq(add, 0)
        ), triangle)
      )
    )

  val triangleVectorMultGlobal: Expr[DataType -> (DataType -> DataType)] =
    fun(DepArrayType(8, i => ArrayType(i + 1, int)))(triangle =>
      fun(ArrayType(8, int))(vector =>
        depMapGlobal(fun(row => zip(row, take(Macros.GetLength(row), vector))
          :>> mapSeq(mult) :>> reduceSeq(add, 0)
        ), triangle)
      )
    )

  test("Basic triangle vector multiplication type inference works") {
    noException should be thrownBy {
      // TODO: fix comparison of DepArrayType objects in the SurfaceLanguage.
      //assertResult(FunctionType(DepArrayType(8, i => ArrayType(i + 1, int)), FunctionType(ArrayType(8, int), DepArrayType(8, _ => int)))) {
      TypeInference(triangleVectorMultSeq, Map()).t.get
    }
  }

  test("Basic sequential triangle vector multiplication compiles to syntactically correct C") {
    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(triangleVectorMultSeq, Map()).toPhrase)
    SyntaxChecker(p.code)
  }

  test("Basic sequential triangle vector multiplication compiles to syntactically correct OpenMP") {
    val p = idealised.OpenMP.ProgramGenerator.makeCode(TypeInference(triangleVectorMultSeq, Map()).toPhrase)
    SyntaxChecker(p.code)
  }

  test("Basic parallel triangle vector multiplication compiles to syntactically correct OpenMP") {
    val p = idealised.OpenMP.ProgramGenerator.makeCode(TypeInference(triangleVectorMultPar, Map()).toPhrase)
    SyntaxChecker(p.code)
  }

  test("Basic sequential triangle vector multiplication compiles to syntactically correct OpenCL") {
    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(triangleVectorMultSeq, Map()).toPhrase, ?, ?)
    SyntaxChecker(kernel.code, ".cl")
  }

  test("Basic parallel triangle vector multiplication compiles to syntactically correct OpenCL") {
    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(triangleVectorMultGlobal, Map()).toPhrase, ?, ?)
    SyntaxChecker(kernel.code, ".cl")
  }
}
