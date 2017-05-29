package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.OpenCL.FunctionalPrimitives
import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.{PrimitiveExpr, ToDPIA}

final case class AsScalar(array: DataExpr, override val `type`: Option[DataType] = None)
  extends PrimitiveExpr
{

  override def toDPIA: FunctionalPrimitives.AsScalar = {
    array.`type` match {
      case Some(ArrayType(n, VectorType(m, dt))) =>
        FunctionalPrimitives.AsScalar(n, m, dt, ToDPIA(array))
      case None => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): AsScalar = {
    import TypeInference._
    val array_ = TypeInference(array, subs)
    array_.`type` match {
      case Some(ArrayType(n, VectorType(m, dt))) =>
        AsScalar(array_, Some(ArrayType(n*m, dt)))
      case x => error(this.toString, s"`${x.toString}'", "n.<m.dt>")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    AsScalar(SurfaceLanguage.VisitAndRebuild(array, f), `type`.map(f(_)))
  }

}
