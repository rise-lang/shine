package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.Types._

final case class Unzip(e: DataExpr,
                       override val t: Option[DataType])
  extends PrimitiveExpr
{
  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    e.t match {
      case Some(ArrayType(n, TupleType(dt1, dt2))) =>
        DPIA.FunctionalPrimitives.Unzip(n, dt1, dt2, e.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Unzip = {
    import TypeInference._
    TypeInference(e, subs) |> (e =>
      e.t match {
        case Some(ArrayType(n, TupleType(dt1, dt2))) => Unzip(e, Some(TupleType(ArrayType(n, dt1), ArrayType(n, dt2))))
        case x => error(expr = s"Unzip($e)", found = s"`${x.toString}'", expected = "n.(dt1,dt2)")
      })
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Unzip(SurfaceLanguage.VisitAndRebuild(e, f), t.map(f(_)))
  }
}