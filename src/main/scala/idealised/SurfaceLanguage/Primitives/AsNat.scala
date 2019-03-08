package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{PrimitiveExpr, _}
import idealised.DPIA

final case class AsNat(e: DataExpr, override val t: Option[DataType] = None)
  extends PrimitiveExpr
{
  override def convertToPhrase: DPIA.FunctionalPrimitives.AsNat = {
    e.t match {
      case Some(IndexType(n)) =>
        DPIA.FunctionalPrimitives.AsNat(n, e.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): AsNat = {
    import TypeInference._
    TypeInference(e, subs) |> (e =>
      e.t match {
        case Some(IndexType(_)) => AsNat(e, Some(NatType))
        case x => error(expr = s"AsNat($e)", found = s"`${x.toString}'", expected = "expr[idx[_]]")
      }
    )
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): DataExpr = {
    AsNat(VisitAndRebuild(e, fun), t.map(fun(_)))
  }
}
