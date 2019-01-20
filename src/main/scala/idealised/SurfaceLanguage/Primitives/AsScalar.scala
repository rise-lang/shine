package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr
import idealised.SurfaceLanguage.Types.{ArrayType, DataType, TypeInference, VectorType}
import idealised.{DPIA, SurfaceLanguage}

final case class AsScalar(array: DataExpr, override val t: Option[DataType] = None)
  extends PrimitiveExpr
{

  override def convertToPhrase: DPIA.FunctionalPrimitives.AsScalar = {
    array.t match {
      case Some(ArrayType(n, VectorType(m, dt))) =>
        DPIA.FunctionalPrimitives.AsScalar(n, m, dt, array.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): AsScalar = {
    import TypeInference._
    val array_ = TypeInference(array, subs)
    array_.t match {
      case Some(ArrayType(n, VectorType(m, dt))) =>
        AsScalar(array_, Some(ArrayType(n*m, dt)))
      case x => error(this.toString, s"`${x.toString}'", "n.<m.dt>")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    AsScalar(SurfaceLanguage.VisitAndRebuild(array, f), t.map(f(_)))
  }

}
