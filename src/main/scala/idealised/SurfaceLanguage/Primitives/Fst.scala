package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.Types._

final case class Fst(tuple: DataExpr, override val t: Option[DataType] = None)
  extends PrimitiveExpr
{

  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    tuple.t match {
      case Some(TupleType(dt1, dt2)) =>
        DPIA.FunctionalPrimitives.Fst(dt1, dt2, tuple.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Fst = {
    import TypeInference._
    TypeInference(tuple, subs) |> (tuple =>
      tuple.t match {
        case Some(TupleType(dt1, _)) => Fst(tuple, Some(dt1))
        case x => error(expr = s"Fst($tuple)",
          found = s"`${x.toString}'", expected = "(dt1, dt2)")
      })
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Fst(SurfaceLanguage.VisitAndRebuild(tuple, f), t.map(f(_)))
  }

  override def toString: String = s"$tuple._1"
}
