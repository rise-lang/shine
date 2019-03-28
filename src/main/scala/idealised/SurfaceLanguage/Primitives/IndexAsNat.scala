package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{PrimitiveExpr, _}

final case class IndexAsNat(e: Expr, override val t: Option[DataType] = Some(NatType))
  extends PrimitiveExpr {

  override def inferType(subs: TypeInference.SubstitutionMap): IndexAsNat = {
    import TypeInference._
    TypeInference(e, subs) |> (e =>
      e.t match {
        case Some(IndexType(_)) => IndexAsNat(e, Some(NatType))
        case x => error(expr = s"AsNat($e)", found = s"`${x.toString}'", expected = "expr[idx[_]]")
      }
    )
  }

  override def children: Seq[Any] = Seq(e, t)

  override def rebuild: Seq[Any] => Expr = {
    case Seq(e: Expr, t: Option[DataType]) => IndexAsNat(e, t)
  }
}
