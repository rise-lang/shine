package idealised.apps

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.{->, Expr}
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import lift.arithmetic._

class dot extends idealised.Tests {

  val N = SizeVar("N")
  val xsT = ArrayType(N, float)
  val ysT = ArrayType(N, float)

  val mult = fun(x => x._1 * x._2)
  val add = fun((x, a) => x + a)

  val high_level: Expr[DataType -> (DataType -> DataType)] = fun(xsT)(xs => fun(ysT)(ys =>
    reduce(add, 0.0f) o map(mult) $ zip(xs, ys)
  ))

  test("High level dot product type inference works") {
    val typed = TypeInference(high_level, Map())

    assertResult(FunctionType(xsT, FunctionType(ysT, float))) {
      typed.t.get
    }
  }

  test("High level dot product translation to phrase works and preserves types") {
    import idealised.DPIA._
    import idealised.DPIA.Types.float
    val phrase = TypeInference(high_level, Map()).convertToPhrase

    val dt = float
    assertResult(exp"[$N.$dt]" -> (exp"[$N.$dt]" -> exp"[$dt]")) {
      phrase.t
    }
  }

  test("High level dot product compiles to syntactically correct C") {
    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(high_level, Map()).toPhrase)
    SyntaxChecker(p.code)
  }

}
