package idealised.DSL.untyped.FunctionalPrimitives

import idealised.Core._
import idealised.DSL.untyped.ExpressionToPhrase.SubstitutionMap
import idealised.DSL.untyped.{VisitAndRebuild, _}

final case class Fst(record: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: SubstitutionMap): Primitive[ExpType] = {
    import ExpressionToPhrase._
    val record_ = ExpressionToPhrase(record, subs)
    record_.t match {
      case ExpType(RecordType(dt1_, dt2_)) =>
        idealised.FunctionalPrimitives.Fst(dt1_, dt2_, record_)

      case x => error(x.toString, "ExpType(RecordType)")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr = {
    Fst(VisitAndRebuild(record, f))
  }

}
