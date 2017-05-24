package idealised.SurfaceLanguage.Primitives

import idealised.DPIA.Types.{ExpressionToPhrase, _}
import idealised.DPIA.Phrases.Primitive
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr

final case class Zip(e1: DataExpr,
                     e2: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: ExpressionToPhrase.SubstitutionMap): Primitive[ExpType] = {
    import ExpressionToPhrase._
    val lhs_ = ExpressionToPhrase(e1, subs)
    val rhs_ = ExpressionToPhrase(e2, subs)
    (lhs_.t, rhs_.t) match {
      case (ExpType(ArrayType(n_, dt1_)), ExpType(ArrayType(m_, dt2_))) =>
        if (n_ == m_)
          DPIA.FunctionalPrimitives.Zip(n_, dt1_, dt2_, lhs_, rhs_)
        else
          error(s"Length $n_ and $m_ does not match")
      case x => error(x.toString(), "PairOfArrayTypes")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Zip(SurfaceLanguage.VisitAndRebuild(e1, f), SurfaceLanguage.VisitAndRebuild(e2, f))
  }
}
