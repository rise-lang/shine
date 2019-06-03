package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, Nat, PrimitiveExpr}
import idealised.{DPIA, SurfaceLanguage}

final case class AsVector(n: Nat, array: Expr,
                          override val t: Option[DataType] = None)
  extends PrimitiveExpr
{

  override def inferType(subs: TypeInference.SubstitutionMap): AsVector = {
    import TypeInference._
    val array_ = TypeInference(array, subs)
    array_.t match {
      case Some(ArrayType(mn, dt: ScalarType)) =>
        AsVector(n, array_, Some(ArrayType(mn /^ n, VectorType(n, dt))))
      case x => error(this.toString, s"`${x.toString}'", "exp[n.st]")
    }
  }

  override def children: Seq[Any] = Seq(n, array, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(n: Nat, array: Expr, t: Option[DataType]@unchecked) => AsVector(n, array, t)
  }
}
