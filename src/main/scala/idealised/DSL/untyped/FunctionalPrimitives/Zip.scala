package idealised.DSL.untyped.FunctionalPrimitives

import idealised.Core._
import idealised.DSL.untyped.{VisitAndRebuild, _}

final case class Zip(e1: DataExpr,
                     e2: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: ExpressionToPhrase.SubstitutionMap): Primitive[ExpType] = {
    import ExpressionToPhrase._
    val lhs_ = ExpressionToPhrase(e1, subs)
    val rhs_ = ExpressionToPhrase(e2, subs)
    (lhs_.t, rhs_.t) match {
      case (ExpType(ArrayType(n_, dt1_)), ExpType(ArrayType(m_, dt2_))) =>
        if (n_ == m_)
          idealised.FunctionalPrimitives.Zip(n_, dt1_, dt2_, lhs_, rhs_)
        else
          error(s"Length $n_ and $m_ does not match")
      case x => error(x.toString(), "PairOfArrayTypes")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr = {
    Zip(VisitAndRebuild(e1, f), VisitAndRebuild(e2, f))
  }
}
