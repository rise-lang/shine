package idealised.DSL.untyped.FunctionalPrimitives

import idealised.Core._
import idealised.DSL.untyped.ExpressionToPhrase.SubstitutionMap
import idealised.DSL.untyped.{VisitAndRebuild, _}

final case class Snd(record: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: SubstitutionMap): Primitive[ExpType] = {
    import ExpressionToPhrase._
    val record_ = ExpressionToPhrase(record, subs)
    record_.t match {
      case ExpType(RecordType(dt1_, dt2_)) =>
        idealised.FunctionalPrimitives.Snd(dt1_, dt2_, record_)

      case x => error(x.toString, "ExpType(RecordType)")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr = {
    Snd(VisitAndRebuild(record, f))
  }

}
