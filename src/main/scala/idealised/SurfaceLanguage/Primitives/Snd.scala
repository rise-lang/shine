package idealised.SurfaceLanguage.Primitives

import idealised.utils._
import idealised.DPIA.Types.{ExpressionToPhrase, _}
import idealised.DPIA.Phrases.Primitive
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr

final case class Snd(record: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: ExpressionToPhrase.SubstitutionMap): Primitive[ExpType] = {
    import idealised.DPIA.Types.ExpressionToPhrase._
    val record_ = ExpressionToPhrase(record, subs)
    record_.t match {
      case ExpType(RecordType(dt1_, dt2_)) =>
        DPIA.FunctionalPrimitives.Snd(dt1_, dt2_, record_)

      case x => error(x.toString, "ExpType(RecordType)")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Snd(SurfaceLanguage.VisitAndRebuild(record, f))
  }

}
