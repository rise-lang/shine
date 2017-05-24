package idealised.OpenCL.DSL.FunctionalPrimitives

import idealised.Core._
import idealised.DSL.untyped.{VisitAndRebuild, _}

final case class AsScalar(array: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: ExpressionToPhrase.SubstitutionMap): Primitive[ExpType] = {
    import ExpressionToPhrase._
    val array_ = ExpressionToPhrase(array, subs)
    array_.t match {
      case ExpType(ArrayType(n_, VectorType(m_, dt_))) =>
        idealised.OpenCL.FunctionalPrimitives.AsScalar(n_, m_, dt_, array_)
      case x => error(x.toString, "ExpType(ArrayType(VectorType))")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): DataExpr = {
    AsScalar(VisitAndRebuild(array, f))
  }

}
