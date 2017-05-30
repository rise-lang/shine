package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage._
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.Types._

final case class Split(n: Nat, array: DataExpr,
                       override val `type`: Option[DataType] = None)
  extends PrimitiveExpr
{


  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    array.`type` match {
      case Some(ArrayType(mn, dt)) =>
        DPIA.FunctionalPrimitives.Split(n, mn /^ n, dt, array.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Split = {
    import TypeInference._
    val array_ = TypeInference(array, subs)
    array_.`type` match {
      case Some(ArrayType(mn, dt)) =>
        Split(n, array_, Some(ArrayType(mn /^ n, ArrayType(n, dt))))
      case x => error(expr = s"Split($n, $array_)",
        found = s"`${x.toString}'", expected = "n.dt")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Split(f(n), SurfaceLanguage.VisitAndRebuild(array, f), `type`.map(f(_)))
  }
}
