package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr}
import lift.arithmetic.BigSum

final case class Join(array: Expr, override val t: Option[DataType])
  extends PrimitiveExpr
{

  override def inferType(subs: TypeInference.SubstitutionMap): Join = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(n, ArrayType(m, dt))) =>
          Join(array, Some(ArrayType(n * m, dt)))
        case Some(ArrayType(n, DepArrayType(m, NatDependentFunctionType(i, dt)))) => ???
        case Some(DepArrayType(n, NatDependentFunctionType(i, ArrayType(d_n, dt)))) =>
          Join(array, Some(ArrayType(BigSum(from=0, upTo = n-1, `for`=i, d_n), dt)))
        case Some(DepArrayType(n, NatDependentFunctionType(i, DepArrayType(m, NatDependentFunctionType(j, dt))))) => ???
        case x => error(expr = s"Join($array)", found = s"`${x.toString}'", expected = "n.m.dt")
      })
  }

  override def children: Seq[Any] = Seq(array, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(array: Expr, t: Option[DataType]) => Join(array, t)
  }
}
