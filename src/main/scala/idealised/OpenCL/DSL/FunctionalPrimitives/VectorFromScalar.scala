package idealised.OpenCL.DSL.FunctionalPrimitives

import idealised.Core._
import idealised.DSL.untyped.{VisitAndRebuild, _}

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

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr = {
    VectorFromScalar(f(n), VisitAndRebuild(arg, f))
  }

}
