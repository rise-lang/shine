package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import idealised.{DPIA, SurfaceLanguage}

final case class Drop(n: Nat, array: Expr,
                       override val t: Option[DataType])
  extends PrimitiveExpr
{

  override def inferType(subs: TypeInference.SubstitutionMap): Drop = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(m, dt)) => Drop(n, array, Some(ArrayType(m - n, dt)))
          //TODO: Check that n < m
        case x => error(expr = s"Drop($n, $array)", found = s"`${x.toString}'", expected = "n.dt")
      })
  }

  override def children: Seq[Any] = Seq(n, array, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(n: Nat, array: Expr, t: Option[DataType]) => Drop(n, array, t)
  }
}
