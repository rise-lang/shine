package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

final case class Pad(l: Nat, r: Nat, padExpr: Expr, array: Expr,
                     override val t: Option[DataType])
  extends PrimitiveExpr {

  override def inferType(subs: TypeInference.SubstitutionMap): Pad = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      TypeInference(padExpr, subs) |> (padExpr =>
        array.t match {
          case Some(ArrayType(n, dt)) => Pad(l, r, padExpr, array, Some(ArrayType(l + n + r, dt)))
          case x => error(expr = s"Pad($array)", found = s"`${x.toString}'", expected = "n.dt")
        })
      )
  }

  override def children: Seq[Any] = Seq(l, r, padExpr, array, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(l: Nat, r: Nat, padExpr: Expr, array: Expr, t: Option[DataType]@unchecked) =>
      Pad(l, r, padExpr, array, t)
  }
}
