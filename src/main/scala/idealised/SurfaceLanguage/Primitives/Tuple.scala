package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr, VisitAndRebuild}

final case class Tuple(fst: Expr, snd: Expr,
                       override val t: Option[DataType])
  extends PrimitiveExpr
{

  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    (fst.t, snd.t) match {
      case (Some(dt1: DataType), Some(dt2: DataType)) =>
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
        case (Some(ft: DataType), Some(st: DataType)) => Tuple(fst, snd, Some(TupleType(ft, st)))
        case _ => TypeInference.error(this.toString, "")
      }))
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Expr = {
    Tuple(VisitAndRebuild(fst, f), VisitAndRebuild(snd, f), t.map(f(_)))
  }

}
