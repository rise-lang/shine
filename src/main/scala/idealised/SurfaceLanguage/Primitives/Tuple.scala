package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.{PrimitiveExpr, VisitAndRebuild}
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.Types._

final case class Tuple(fst: DataExpr, snd: DataExpr,
                       override val `type`: Option[DataType] = None)
  extends PrimitiveExpr
{


  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    (fst.`type`, snd.`type`) match {
      case (Some(dt1), Some(dt2)) =>
        DPIA.FunctionalPrimitives.Record(dt1, dt2,
          fst.toPhrase[DPIA.Types.ExpType],
          snd.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Tuple = {
    val fst_ = TypeInference(fst, subs)
    val snd_ = TypeInference(snd, subs)
    (fst_.`type`, snd_.`type`) match {
      case (Some(ft), Some(st)) => Tuple(fst_, snd_, Some(TupleType(ft, st)))
      case _ => TypeInference.error(this.toString, "")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr = {
    Tuple(VisitAndRebuild(fst, f), VisitAndRebuild(snd, f), `type`.map(f(_)))
  }

}
