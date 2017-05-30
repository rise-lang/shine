package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.OpenCL.FunctionalPrimitives
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.DSL.DataExpr

final case class AsVector(n: Nat, array: DataExpr,
                          override val `type`: Option[DataType] = None)
  extends PrimitiveExpr
{
  override def convertToPhrase: FunctionalPrimitives.AsVector = {
    array.`type` match {
      case Some(ArrayType(mn, st: ScalarType)) =>
        FunctionalPrimitives.AsVector(n, mn /^ n, st, array.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
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
