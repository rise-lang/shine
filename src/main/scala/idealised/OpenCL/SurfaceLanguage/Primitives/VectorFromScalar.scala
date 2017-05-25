package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.DPIA.Phrases.Primitive
import idealised.DPIA.Types.{ExpType, ScalarType, TypeInference}
import idealised.DPIA._
import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr

final case class VectorFromScalar(n: Nat, arg: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: TypeInference.SubstitutionMap): Primitive[ExpType] = {
    import TypeInference._
    val arg_ = TypeInference(arg, subs)
    arg_.t match {
      case ExpType(dt_) if dt_.isInstanceOf[ScalarType] =>
        idealised.OpenCL.FunctionalPrimitives.VectorFromScalar(n, dt_.asInstanceOf[ScalarType], arg_)
      case x => error(this.toString, s"`${x.toString}'", "exp[st]")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    VectorFromScalar(f(n), SurfaceLanguage.VisitAndRebuild(arg, f))
  }

}
