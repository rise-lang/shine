package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.Types._

final case class Zip(lhs: DataExpr, rhs: DataExpr,
                     override val t: Option[DataType] = None)
  extends PrimitiveExpr
{
  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    (lhs.t, rhs.t) match {
      case (Some(ArrayType(n, dt1)), Some(ArrayType(m, dt2))) if n == m =>
        DPIA.FunctionalPrimitives.Zip(n, dt1, dt2,
          lhs.toPhrase[DPIA.Types.ExpType],
          rhs.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Zip = {
    import TypeInference._
    TypeInference(lhs, subs) |> (lhs =>
    TypeInference(rhs, subs) |> (rhs =>
      (lhs.t, rhs.t) match {
        case (Some(ArrayType(n, dt1)), Some(ArrayType(m, dt2))) =>
          if (n == m)
            Zip(lhs, rhs, Some(ArrayType(n, TupleType(dt1, dt2))))
          else
            error(expr = s"Zip($lhs, $rhs)", msg = s"Array length $n and $m does not match")
        case x =>
          error(expr = s"Zip($lhs, $rhs)", found = s"`${x.toString}'", expected = "(n.dt1, m.dt2)")
      }))
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Zip(SurfaceLanguage.VisitAndRebuild(lhs, f),
      SurfaceLanguage.VisitAndRebuild(rhs, f),
      t.map(f(_)))
  }
}