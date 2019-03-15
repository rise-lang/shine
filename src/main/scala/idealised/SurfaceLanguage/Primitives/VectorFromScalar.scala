package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types.{DataType, ScalarType, TypeInference, VectorType}
import idealised.SurfaceLanguage.{Expr, Nat, PrimitiveExpr, VisitAndRebuild}

final case class VectorFromScalar(n: Nat, arg: Expr,
                                  override val t: Option[DataType] = None)
  extends PrimitiveExpr {

  override def inferType(subs: TypeInference.SubstitutionMap): VectorFromScalar = {
    import TypeInference._
    val arg_ = TypeInference(arg, subs)
    arg_.t match {
      case Some(dt: ScalarType) =>
        VectorFromScalar(n, arg_, Some(VectorType(n, dt)))
      case x => error(this.toString, s"`${x.toString}'", "st")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Expr = {
    VectorFromScalar(f(n), VisitAndRebuild(arg, f), t.map(f(_)))
  }

}
