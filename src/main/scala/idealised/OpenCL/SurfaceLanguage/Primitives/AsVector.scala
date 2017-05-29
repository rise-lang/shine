package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.OpenCL.FunctionalPrimitives
import idealised.SurfaceLanguage
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.DSL.DataExpr

final case class AsVector(n: Nat, array: DataExpr,
                          override val `type`: Option[DataType] = None)
  extends PrimitiveExpr
{
  override def toDPIA: FunctionalPrimitives.AsVector = {
    array.`type` match {
      case Some(ArrayType(mn, st: ScalarType)) =>
        FunctionalPrimitives.AsVector(n, mn /^ n, st, ToDPIA(array))
      case None => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): AsVector = {
    import TypeInference._
    val array_ = TypeInference(array, subs)
    array_.`type` match {
      case Some(ArrayType(mn, dt: ScalarType)) =>
        AsVector(n, array_, Some(ArrayType(mn /^ n, VectorType(n, dt))))
      case x => error(this.toString, s"`${x.toString}'", "exp[n.st]")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    AsVector(f(n), SurfaceLanguage.VisitAndRebuild(array, f), `type`.map(f(_)))
  }

}
