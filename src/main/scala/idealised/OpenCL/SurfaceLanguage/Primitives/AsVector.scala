package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.DPIA.Phrases.Primitive
import idealised.DPIA.Types.{TypeInference, _}
import idealised.DPIA._
import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr

final case class AsVector(n: Nat, array: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: TypeInference.SubstitutionMap): Primitive[ExpType] = {
    import TypeInference._
    val array_ = TypeInference(array, subs)
    array_.t match {
      case ExpType(ArrayType(mn_, dt_)) if dt_.isInstanceOf[ScalarType] =>
        idealised.OpenCL.FunctionalPrimitives.AsVector(n, mn_ /^ n, dt_.asInstanceOf[ScalarType], array_)
      case x => error(this, x.toString, "exp[n.st]")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    AsVector(f(n), SurfaceLanguage.VisitAndRebuild(array, f))
  }

}
