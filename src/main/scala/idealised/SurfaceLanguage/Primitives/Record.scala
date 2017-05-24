package idealised.SurfaceLanguage.Primitives

import idealised.DPIA.Types.{ExpType, ExpressionToPhrase}
import idealised.DPIA.Types.ExpressionToPhrase.SubstitutionMap
import idealised.DPIA.Phrases.Primitive
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr

final case class Record(fst: DataExpr, snd: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: SubstitutionMap): Primitive[ExpType] = {
    val fst_ = ExpressionToPhrase(fst, subs)
    val snd_ = ExpressionToPhrase(snd, subs)
    DPIA.FunctionalPrimitives.Record(fst_.t.dataType, snd_.t.dataType, fst_, snd_)
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Record(SurfaceLanguage.VisitAndRebuild(fst, f), SurfaceLanguage.VisitAndRebuild(snd, f))
  }

}
