package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.Types._

final case class Zip(e1: DataExpr, e2: DataExpr,
                     override val t: Option[DataType] = None)
  extends PrimitiveExpr
{
  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    (e1.t, e2.t) match {
      case (Some(ArrayType(n, dt1)), Some(ArrayType(m, dt2))) if n == m =>
        DPIA.FunctionalPrimitives.Zip(n, dt1, dt2,
          e1.toPhrase[DPIA.Types.ExpType],
          e2.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Zip = {
    import TypeInference._
    val lhs_ = TypeInference(e1, subs)
    val rhs_ = TypeInference(e2, subs)
    (lhs_.t, rhs_.t) match {
      case (Some(ArrayType(n, dt1)), Some(ArrayType(m, dt2))) =>
        if (n == m)
          Zip(lhs_, rhs_, Some(ArrayType(n, TupleType(dt1, dt2))))
        else
          error(expr = s"Zip($lhs_, $rhs_)",
            msg = s"Array length $n and $m does not match")
      case x =>
        error(expr = s"Zip($lhs_, $rhs_)",
          found = s"`${x.toString}'", expected = "(n.dt1, m.dt2)")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Zip(SurfaceLanguage.VisitAndRebuild(e1, f),
      SurfaceLanguage.VisitAndRebuild(e2, f),
      t.map(f(_)))
  }
}