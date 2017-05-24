package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.DPIA.Types.{ExpressionToPhrase, _}
import idealised.DPIA.Phrases.Primitive
import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr

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

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    AsScalar(SurfaceLanguage.VisitAndRebuild(array, f))
  }

}
