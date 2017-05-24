package idealised.SurfaceLanguage.Primitives

import idealised.DPIA.Types.{ExpressionToPhrase, _}
import idealised.DPIA.Phrases.Primitive
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr

final case class Fst(record: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: ExpressionToPhrase.SubstitutionMap): Primitive[ExpType] = {
    import idealised.DPIA.Types.ExpressionToPhrase._
    val record_ = ExpressionToPhrase(record, subs)
    record_.t match {
      case ExpType(RecordType(dt1_, dt2_)) =>
        DPIA.FunctionalPrimitives.Fst(dt1_, dt2_, record_)

      case x => error(x.toString, "ExpType(RecordType)")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Fst(SurfaceLanguage.VisitAndRebuild(record, f))
  }

}
