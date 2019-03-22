package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr}

final case class Zip(lhs: Expr, rhs: Expr,
                     override val t: Option[DataType])
  extends PrimitiveExpr {

  override def inferType(subs: TypeInference.SubstitutionMap): Zip = {
    import TypeInference._
    TypeInference(lhs, subs) |> (lhs =>
      TypeInference(rhs, subs) |> (rhs =>
        (lhs.t, rhs.t) match {
          case (Some(ArrayType(n, dt1)), Some(ArrayType(m, dt2))) =>
            if (n == m)
              Zip(lhs, rhs, Some(ArrayType(n, TupleType(dt1, dt2))))
            else
              error(expr = s"Zip($lhs, $rhs)", msg = s"Array length $n and $m does not match")
          case x =>
            error(expr = s"Zip($lhs, $rhs)", found = s"`${x.toString}'", expected = "(n.dt1, m.dt2)")
        }))
  }

  override def children: Seq[Any] = Seq(lhs, rhs, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(lhs: Expr, rhs: Expr, t: Option[DataType]) =>
      Zip(lhs, rhs, t)
  }
}