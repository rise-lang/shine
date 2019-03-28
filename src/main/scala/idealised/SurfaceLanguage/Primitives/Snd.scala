package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr}
import idealised.SurfaceLanguage.Types._

final case class Snd(tuple: Expr,
                     override val t: Option[DataType])
  extends PrimitiveExpr {

  override def inferType(subs: TypeInference.SubstitutionMap): Snd = {
    import TypeInference._
    TypeInference(tuple, subs) |> (tuple =>
      tuple.t match {
        case Some(TupleType(_, dt2)) => Snd(tuple, Some(dt2))
        case x => error(expr = s"Snd($tuple)",
          found = s"`${x.toString}'", expected = "(dt1, dt2)")
      })
  }

  override def children: Seq[Any] = Seq(tuple, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(tuple: Expr, t: Option[DataType]) => Snd(tuple, t)
  }

  override def toString: String = s"$tuple._2"

}
