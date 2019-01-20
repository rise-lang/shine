package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.{PrimitiveExpr, VisitAndRebuild}
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.Types._

final case class Tuple(fst: DataExpr, snd: DataExpr,
                       override val t: Option[DataType])
  extends PrimitiveExpr
{


  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    (fst.t, snd.t) match {
      case (Some(dt1), Some(dt2)) =>
        DPIA.FunctionalPrimitives.Record(dt1, dt2,
          fst.toPhrase[DPIA.Types.ExpType],
          snd.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Tuple = {
    TypeInference(fst, subs) |> (fst =>
    TypeInference(snd, subs) |> (snd =>
      (fst.t, snd.t) match {
        case (Some(ft), Some(st)) => Tuple(fst, snd, Some(TupleType(ft, st)))
        case _ => TypeInference.error(this.toString, "")
      }))
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr = {
    Tuple(VisitAndRebuild(fst, f), VisitAndRebuild(snd, f), t.map(f(_)))
  }

}
