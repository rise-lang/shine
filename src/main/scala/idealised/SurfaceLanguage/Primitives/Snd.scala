package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.{PrimitiveExpr, ToDPIA}
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.Types._

final case class Snd(tuple: DataExpr,
                     override val `type`: Option[DataType] = None)
  extends PrimitiveExpr
{


  override def toDPIA: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    tuple.`type` match {
      case Some(TupleType(dt1, dt2)) =>
        DPIA.FunctionalPrimitives.Snd(dt1, dt2, ToDPIA(tuple))
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Snd = {
    import TypeInference._
    val tuple_ = TypeInference(tuple, subs)
    tuple_.`type` match {
      case Some(TupleType(dt1_, dt2_)) =>
        Snd(tuple_, Some(dt2_))

      case x => error(expr = s"Snd($tuple_)",
        found = s"`${x.toString}'", expected = "(dt1, dt2)")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Snd(SurfaceLanguage.VisitAndRebuild(tuple, f), `type`.map(f(_)))
  }

  override def toString: String = s"$tuple._2"

}
