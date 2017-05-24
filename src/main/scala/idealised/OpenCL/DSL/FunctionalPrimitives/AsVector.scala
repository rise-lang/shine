package idealised.OpenCL.DSL.FunctionalPrimitives

import idealised.Core._
import idealised.DSL.untyped.{VisitAndRebuild, _}

final case class AsVector(n: Nat, array: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: ExpressionToPhrase.SubstitutionMap): Primitive[ExpType] = {
    import ExpressionToPhrase._
    val array_ = ExpressionToPhrase(array, subs)
    array_.t match {
      case ExpType(ArrayType(mn_, dt_)) if dt_.isInstanceOf[ScalarType] =>
        idealised.OpenCL.FunctionalPrimitives.AsVector(n, mn_ /^ n, dt_.asInstanceOf[ScalarType], array_)
      case x => error(x.toString, "ExpType(ArrayType)")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr = {
    AsVector(f(n), VisitAndRebuild(array, f))
  }

}
