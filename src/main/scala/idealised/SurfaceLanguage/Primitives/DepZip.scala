package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.{Expr, NatIdentifier, PrimitiveExpr}
import idealised.SurfaceLanguage.Types.{DataType, DepArrayType, DependentFunctionType, TupleType, Type, TypeInference}
import idealised.SurfaceLanguage.Types.TypeInference.{SubstitutionMap, error}

final case class DepZip(lhs: Expr, rhs: Expr,
                        override val t: Option[DataType])
  extends PrimitiveExpr
{
  override def inferType(subs: SubstitutionMap): DepZip = {
    import TypeInference._
    TypeInference(lhs, subs) |> (lhs =>
      TypeInference(rhs, subs) |> (rhs =>
        (lhs.t, rhs.t) match {
          case (Some(DepArrayType(n, DependentFunctionType(j1: NatIdentifier, df1))),
                Some(DepArrayType(m, DependentFunctionType(j2: NatIdentifier, df2)))) =>
            if (n == m)
              DepZip(lhs, rhs, Some(DepArrayType(n, i => TupleType(Type.substitute(i, `for`=j1, `in`=df1), Type.substitute(i, `for`=j2, `in`=df2)))))
            else
              error(expr = s"DepZip($lhs, $rhs)", msg = s"Array length $n and $m does not match")
          case x =>
            error(expr = s"DepZip($lhs, $rhs)", found = s"`${x.toString}'", expected = "something else")
        }))
  }

  override def rebuild: Seq[Any] => Expr = {
    case Seq(lhs: Expr, rhs: Expr, t: Option[DataType]@unchecked) =>
      DepZip(lhs, rhs, t)
  }

  override def children: Seq[Any] = Seq(lhs, rhs, t)
}
