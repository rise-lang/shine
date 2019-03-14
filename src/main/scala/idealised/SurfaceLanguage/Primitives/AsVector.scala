package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, Nat, PrimitiveExpr}
import idealised.{DPIA, SurfaceLanguage}

final case class AsVector(n: Nat, array: Expr,
                          override val t: Option[DataType] = None)
  extends PrimitiveExpr
{
  override def convertToPhrase: DPIA.FunctionalPrimitives.AsVector = {
    array.t match {
      case Some(ArrayType(mn, st: ScalarType)) =>
        DPIA.FunctionalPrimitives.AsVector(n, mn /^ n, st, array.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): AsVector = {
    import TypeInference._
    val array_ = TypeInference(array, subs)
    array_.t match {
      case Some(ArrayType(mn, dt: ScalarType)) =>
        AsVector(n, array_, Some(ArrayType(mn /^ n, VectorType(n, dt))))
      case x => error(this.toString, s"`${x.toString}'", "exp[n.st]")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): Expr = {
    AsVector(f(n), SurfaceLanguage.VisitAndRebuild(array, f), t.map(f(_)))
  }

}
