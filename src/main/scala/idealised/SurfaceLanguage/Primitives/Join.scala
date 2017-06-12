package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.Types._

final case class Join(array: DataExpr, override val `type`: Option[DataType] = None)
  extends PrimitiveExpr
{
  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    array.`type` match {
      case Some(ArrayType(n, ArrayType(m, dt))) =>
        DPIA.FunctionalPrimitives.Join(n, m, dt, array.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Join = {
    import TypeInference._
    val array_ = TypeInference(array, subs)
    array_.`type` match {
      case Some(ArrayType(n, ArrayType(m, dt))) =>
        Join(array_, Some(ArrayType(n * m, dt)))
      case x => error(expr = s"Join($array_)",
        found = s"`${x.toString}'", expected = "n.m.dt")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Join(SurfaceLanguage.VisitAndRebuild(array, f), `type`.map(f(_)))
  }
}
