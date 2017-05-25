package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.DPIA.Phrases.Primitive
import idealised.DPIA.Types.{TypeInference, _}
import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr

final case class AsScalar(array: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: TypeInference.SubstitutionMap): Primitive[ExpType] = {
    import TypeInference._
    val array_ = TypeInference(array, subs)
    array_.t match {
      case ExpType(ArrayType(n_, VectorType(m_, dt_))) =>
        idealised.OpenCL.FunctionalPrimitives.AsScalar(n_, m_, dt_, array_)
      case x => error(this.toString, s"`${x.toString}'", "exp[n.<m.dt>]")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    AsScalar(SurfaceLanguage.VisitAndRebuild(array, f))
  }

}
