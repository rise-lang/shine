package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.utils._
import idealised.DPIA._
import idealised.DPIA.Phrases.Primitive
import idealised.DPIA.Types.{ExpType, ExpressionToPhrase, ScalarType}
import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr

final case class VectorFromScalar(n: Nat, arg: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: ExpressionToPhrase.SubstitutionMap): Primitive[ExpType] = {
    import ExpressionToPhrase._
    val arg_ = ExpressionToPhrase(arg, subs)
    arg_.t match {
      case ExpType(dt_) if dt_.isInstanceOf[ScalarType] =>
        idealised.OpenCL.FunctionalPrimitives.VectorFromScalar(n, dt_.asInstanceOf[ScalarType], arg_)
      case x => error(x.toString, "ExpType(ScalarType)")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    VectorFromScalar(f(n), SurfaceLanguage.VisitAndRebuild(arg, f))
  }

}
