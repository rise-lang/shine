package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.DPIA.Phrases.Primitive
import idealised.DPIA.Types.{ExpressionToPhrase, _}
import idealised.DPIA._
import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr

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

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    AsVector(f(n), SurfaceLanguage.VisitAndRebuild(array, f))
  }

}
