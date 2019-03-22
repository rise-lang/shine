package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

final case class Take(n: Nat, array: Expr,
                      override val t: Option[DataType])
  extends PrimitiveExpr {

  override def inferType(subs: TypeInference.SubstitutionMap): Take = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(_, dt)) => Take(n, array, Some(ArrayType(n, dt)))
        //TODO: Check that n < m
        case x => error(expr = s"Take($n, $array)", found = s"`${x.toString}'", expected = "n.dt")
      })
  }

  override def children: Seq[Any] = Seq(n, array, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(n: Nat, array: Expr, t: Option[DataType]) =>
      Take(n, array, t)
  }
}
