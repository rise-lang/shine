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

  override def children: Seq[Any] = Seq(n, arg, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(n: Nat, arg: Expr, t: Option[DataType]) =>
      VectorFromScalar(n, arg, t)
  }
}
