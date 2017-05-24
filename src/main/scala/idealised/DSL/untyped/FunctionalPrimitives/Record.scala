package idealised.DSL.untyped.FunctionalPrimitives

import idealised.Core._
import idealised.DSL.untyped.ExpressionToPhrase.SubstitutionMap
import idealised.DSL.untyped.{VisitAndRebuild, _}

final case class Record(fst: DataExpr, snd: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: SubstitutionMap): Primitive[ExpType] = {
    val fst_ = ExpressionToPhrase(fst, subs)
    val snd_ = ExpressionToPhrase(snd, subs)
    idealised.FunctionalPrimitives.Record(fst_.t.dataType, snd_.t.dataType, fst_, snd_)
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr = {
    Record(VisitAndRebuild(fst, f), VisitAndRebuild(snd, f))
  }

}
