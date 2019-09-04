package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr}

final case class Fst(tuple: Expr, override val t: Option[DataType] = None)
  extends PrimitiveExpr
{

  override def inferType(subs: TypeInference.SubstitutionMap): Fst = {
    import TypeInference._
    TypeInference(tuple, subs) |> (tuple =>
      tuple.t match {
        case Some(TupleType(dt1, _)) => Fst(tuple, Some(dt1))
        case x => error(expr = s"Fst($tuple)",
          found = s"`${x.toString}'", expected = "(dt1, dt2)")
      })
  }

  override def children: Seq[Any] = Seq(tuple, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(tuple: Expr, t: Option[DataType]@unchecked) => Fst(tuple, t)
  }

  override def toString: String = s"$tuple._1"
}
