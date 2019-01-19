package idealised.apps

import idealised.OpenCL.SurfaceLanguage.DSL.depMap
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import idealised.util.SyntaxChecker
import lift.arithmetic.?

import scala.language.{implicitConversions, postfixOps}

class triangleVectorMult extends idealised.Tests {

  val mult = fun(x => x._1 * x._2)

  val add = fun(x => fun(y => x + y))

  val triangleVectorMult: Expr[DataType -> (DataType -> DataType)] =
    fun(DepArrayType(8, i => ArrayType(i + 1, int)))(triangle =>
      fun(ArrayType(8, int))(vector =>
        depMap(fun(row => zip(row, take(Macros.GetLength(row), vector))
          :>> map(mult) :>> reduce(add, 0)
        ), triangle)
      )
    )

  test("Basic triangle vector multiplication type inference works") {
    noException should be thrownBy {
      // TODO: fix comparison of DepArrayType objects in the SurfaceLanguage.
      //assertResult(FunctionType(DepArrayType(8, i => ArrayType(i + 1, int)), FunctionType(ArrayType(8, int), DepArrayType(8, _ => int)))) {
      TypeInference(triangleVectorMult, Map()).t.get
    }
  }

  test("Basic triangle vector multiplication compiles to syntactically correct C") {
    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(triangleVectorMult, Map()).toPhrase)
    SyntaxChecker(p.code)
  }

  test("Basic triangle vector multiplication compiles to syntactically correct OpenMP") {
    val p = idealised.OpenMP.ProgramGenerator.makeCode(TypeInference(triangleVectorMult, Map()).toPhrase)
    SyntaxChecker(p.code)
  }

  test("Basic triangle vector multiplication compiles to syntactically correct OpenCL") {
    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(triangleVectorMult, Map()).toPhrase, ?, ?)
    println(kernel.code)
    SyntaxChecker(kernel.code)
  }

}
