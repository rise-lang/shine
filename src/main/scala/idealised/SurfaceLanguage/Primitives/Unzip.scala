package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr}

final case class Unzip(e: Expr,
                       override val t: Option[DataType])
  extends PrimitiveExpr {

  override def inferType(subs: TypeInference.SubstitutionMap): Unzip = {
    import TypeInference._
    TypeInference(e, subs) |> (e =>
      e.t match {
        case Some(ArrayType(n, TupleType(dt1, dt2))) => Unzip(e, Some(TupleType(ArrayType(n, dt1), ArrayType(n, dt2))))
        case x => error(expr = s"Unzip($e)", found = s"`${x.toString}'", expected = "n.(dt1,dt2)")
      })
  }

  override def children: Seq[Any] = Seq(e, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(e: Expr, t: Option[DataType]) => Unzip(e, t)
  }
}