package idealised.SurfaceLanguage.Primitives

import idealised.DPIA.Types.{ExpressionToPhrase, _}
import idealised.DPIA._
import idealised.DPIA.Phrases.Primitive
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr

final case class Split(n: Nat, array: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: ExpressionToPhrase.SubstitutionMap): Primitive[ExpType] = {
    import ExpressionToPhrase._
    val array_ = ExpressionToPhrase(array, subs)
    array_.t match {
      case ExpType(ArrayType(mn_, dt_)) =>
        DPIA.FunctionalPrimitives.Split(n, mn_ /^ n, dt_, array_)
      case x => error(x.toString, "ArrayType")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Split(f(n), SurfaceLanguage.VisitAndRebuild(array, f))
  }
}
