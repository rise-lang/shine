package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{PrimitiveExpr, _}
import idealised.DPIA

final case class UnsafeAsIndex(n: Nat, e: DataExpr, override val t: Option[DataType] = None)
  extends PrimitiveExpr
{
  override def convertToPhrase: DPIA.FunctionalPrimitives.UnsafeAsIndex = {
    e.t match {
      case Some(NatType) =>
        DPIA.FunctionalPrimitives.UnsafeAsIndex(n, e.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("The expression given to UnsafeAsIndex must be of type exp[nat].")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): UnsafeAsIndex = {
    import TypeInference._
    TypeInference(e, subs) |> (e =>
      e.t match {
        //TODO lift e to n: Nat and use n.max to create IndexType or at least to check bounds?
        case Some(NatType) => UnsafeAsIndex(n, e, Some(IndexType(n)))
        case x => error(expr = s"AsIndex($e)", found = s"`${x.toString}'", expected = "expr[nat]")
      }
    )
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): DataExpr = {
    UnsafeAsIndex(fun(n), VisitAndRebuild(e, fun), t.map(fun(_)))
  }
}
