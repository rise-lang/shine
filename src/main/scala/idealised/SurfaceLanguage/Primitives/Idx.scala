package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.{Expr, PrimitiveExpr}
import idealised.SurfaceLanguage.Types.{ArrayType, DataType, IndexType, TypeInference}
import lift.arithmetic.BoolExpr.ArithPredicate

case class Idx(e:Expr, idx:Expr, override val t:Option[DataType] = None) extends PrimitiveExpr{

  override def inferType(subs: TypeInference.SubstitutionMap): Idx = {
    import TypeInference._
    TypeInference(this.e, subs) |> (array =>
      TypeInference(this.idx, subs) |> (index =>
        (array.t, index.t) match {
          case (Some(ArrayType(n, dt)), Some(IndexType(idxLen))) =>
            ArithPredicate(n, idxLen, ArithPredicate.Operator.<=).evaluate match {
              case Some(false) => error(expr = s"($array @ $index)", found = s"$idxLen > $n", expected = s"$idxLen < $n")
              case _ => Idx(array, index, Some(dt))
            }

          case x => error(expr = s"($array @ $index)", found = s"${x.toString()}", expected = "(n.dt, idx(n))")
        }
        )
      )
  }

  override def children: Seq[Any] = Seq(e, idx, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(e:Expr, idx:Expr, t: Option[DataType]) => Idx(e, idx, t)
  }

  override def toString: String = s"$e @ $idx"
}
