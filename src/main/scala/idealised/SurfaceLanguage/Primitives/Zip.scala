package idealised.SurfaceLanguage.Primitives

import idealised.DPIA.Phrases.Primitive
import idealised.DPIA.Types.{TypeInference, _}
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr
import idealised.{DPIA, SurfaceLanguage}

final case class Zip(e1: DataExpr,
                     e2: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: TypeInference.SubstitutionMap): Primitive[ExpType] = {
    import TypeInference._
    val lhs_ = TypeInference(e1, subs)
    val rhs_ = TypeInference(e2, subs)
    (lhs_.t, rhs_.t) match {
      case (ExpType(ArrayType(n_, dt1_)), ExpType(ArrayType(m_, dt2_))) =>
        if (n_ == m_)
          DPIA.FunctionalPrimitives.Zip(n_, dt1_, dt2_, lhs_, rhs_)
        else
          error(expr = s"Zip($lhs_, $rhs_)",
            msg = s"Array length $n_ and $m_ does not match")
      case x =>
        error(expr = s"Zip($lhs_, $rhs_)",
          found = s"`${x.toString}'", expected = "(exp[n.dt1], exp[m.dt2])")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Zip(SurfaceLanguage.VisitAndRebuild(e1, f), SurfaceLanguage.VisitAndRebuild(e2, f))
  }
}