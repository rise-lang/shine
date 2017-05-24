package idealised.DSL.untyped.FunctionalPrimitives

import idealised.Core._
import idealised.DSL.untyped.{ExpressionToPhrase, DataExpr, PrimitiveExpr, VisitAndRebuild}

final case class Split(n: Nat, array: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: ExpressionToPhrase.SubstitutionMap): Primitive[ExpType] = {
    import ExpressionToPhrase._
    val array_ = ExpressionToPhrase(array, subs)
    array_.t match {
      case ExpType(ArrayType(mn_, dt_)) =>
        idealised.FunctionalPrimitives.Split(n, mn_ /^ n, dt_, array_)
      case x => error(x.toString, "ArrayType")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr = {
    Split(f(n), VisitAndRebuild(array, f))
  }
}
