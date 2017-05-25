package idealised.SurfaceLanguage.Primitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr}
import idealised.{DPIA, SurfaceLanguage}

abstract class AbstractMap(f: Expr[ExpType -> ExpType], array: DataExpr)
  extends PrimitiveExpr {

  def makeMap: (Expr[ExpType -> ExpType], DataExpr) => AbstractMap

  def makePhraseMap: (Nat, DataType, DataType, Phrase[ExpType -> ExpType], Phrase[ExpType]) =>
    idealised.DPIA.FunctionalPrimitives.AbstractMap

  override def inferTypes(subs: TypeInference.SubstitutionMap): Primitive[ExpType] = {
    import TypeInference._
    val array_ = TypeInference(array, subs)
    array_.t match {
      case ExpType(ArrayType(n_, dt1_)) =>
        val f_ = setParamAndInferType(f, exp"[$dt1_]", subs)
        f_.t match {
          case FunctionType(ExpType(dt1__), ExpType(dt2_)) =>
            if (dt1_ == dt1__) {
              makePhraseMap(n_, dt1_, dt2_, f_, array_)
            } else {
              error(expr = s"${this.getClass.getSimpleName}($f_, $array_)",
                found = s"`$dt1__'", expected = s"`$dt1_'")
            }
          case x => error(expr = s"${this.getClass.getSimpleName}($f_, $array_)",
            found = s"`${x.toString}'", expected = "exp[dt1] -> exp[dt2]")
        }
      case x => error(expr = s"${this.getClass.getSimpleName}($f, $array_)",
        found = s"`${x.toString}'", expected = "exp[n.dt]")
    }
  }

  override def visitAndRebuild(fun: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    makeMap(SurfaceLanguage.VisitAndRebuild(f, fun), SurfaceLanguage.VisitAndRebuild(array, fun))
  }

}

final case class Map(f: Expr[ExpType -> ExpType], array: DataExpr)
  extends AbstractMap(f, array) {

  override def makePhraseMap = DPIA.FunctionalPrimitives.Map

  override def makeMap = Map
}
