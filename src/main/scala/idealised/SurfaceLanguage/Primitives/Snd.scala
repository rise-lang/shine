package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.PrimitiveExpr
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.Types._

final case class Snd(tuple: Expr,
                     override val t: Option[DataType])
  extends PrimitiveExpr
{


  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    tuple.t match {
      case Some(TupleType(dt1, dt2)) =>
        DPIA.FunctionalPrimitives.Snd(dt1, dt2, tuple.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Snd = {
    import TypeInference._
    TypeInference(tuple, subs) |> (tuple =>
      tuple.t match {
        case Some(TupleType(_, dt2)) => Snd(tuple, Some(dt2))
        case x => error(expr = s"Snd($tuple)",
          found = s"`${x.toString}'", expected = "(dt1, dt2)")
      })
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): Expr = {
    Snd(SurfaceLanguage.VisitAndRebuild(tuple, f), t.map(f(_)))
  }

  override def toString: String = s"$tuple._2"

}
