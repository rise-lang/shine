package idealised.SurfaceLanguage.Primitives

import idealised.DPIA.Types.{ExpressionToPhrase, _}
import idealised.DPIA.Phrases.Primitive
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr

final case class Join(array: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: ExpressionToPhrase.SubstitutionMap): Primitive[ExpType] = {
    import ExpressionToPhrase._
    val array_ = ExpressionToPhrase(array, subs)
    array_.t match {
      case ExpType(ArrayType(n_, ArrayType(m_, dt_))) =>
        DPIA.FunctionalPrimitives.Join(n_, m_, dt_, array_)
      case x => error(x.toString, "ExpType(ArrayType(ArrayType))")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Join(SurfaceLanguage.VisitAndRebuild(array, f))
  }
}
