package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{PrimitiveExpr, _}
import idealised.DPIA

final case class IndexAsNat(e: DataExpr, override val t: Option[DataType] = Some(NatType))
  extends PrimitiveExpr
{
  override def convertToPhrase: DPIA.FunctionalPrimitives.IndexAsNat = {
    e.t match {
      case Some(IndexType(n)) =>
        DPIA.FunctionalPrimitives.IndexAsNat(n, e.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("The expression given to AsNat must be of type expr[idx[_]].")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): IndexAsNat = {
    import TypeInference._
    TypeInference(e, subs) |> (e =>
      e.t match {
        case Some(IndexType(_)) => IndexAsNat(e, Some(NatType))
        case x => error(expr = s"AsNat($e)", found = s"`${x.toString}'", expected = "expr[idx[_]]")
      }
    )
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): DataExpr = {
    IndexAsNat(VisitAndRebuild(e, fun), t.map(fun(_)))
  }
}
