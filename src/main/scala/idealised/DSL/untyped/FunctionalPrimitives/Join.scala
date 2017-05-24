package idealised.DSL.untyped.FunctionalPrimitives

import idealised.Core._
import idealised.DSL.untyped.{ExpressionToPhrase, DataExpr, PrimitiveExpr, VisitAndRebuild}

final case class Join(array: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: ExpressionToPhrase.SubstitutionMap): Primitive[ExpType] = {
    import ExpressionToPhrase._
    val array_ = ExpressionToPhrase(array, subs)
    array_.t match {
      case ExpType(ArrayType(n_, ArrayType(m_, dt_))) =>
        idealised.FunctionalPrimitives.Join(n_, m_, dt_, array_)
      case x => error(x.toString, "ExpType(ArrayType(ArrayType))")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr = {
    Join(VisitAndRebuild(array, f))
  }
}
