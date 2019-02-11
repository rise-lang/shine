package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

final case class Generate(n: Nat, f: Expr[`(nat)->`[DataType -> DataType]], override val t: Option[DataType] = None)
  extends PrimitiveExpr
{
  override def convertToPhrase: DPIA.FunctionalPrimitives.Generate = {
    f.t match {
      case Some(NatDependentFunctionType(_, FunctionType(_, dt))) =>
        DPIA.FunctionalPrimitives.Generate(n, dt,
          f.toPhrase[DPIA.Types.NatDependentFunctionType[DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.ExpType]]])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Generate = {
    import TypeInference._
    TypeInference(f, subs) |> (f =>
      f.t match {
        case Some(NatDependentFunctionType(_, FunctionType(IndexType(_), dt))) => Generate(n, f, Some(ArrayType(n, dt)))
        case x => error(expr = s"Generator($n, $f)", found = s"`${x.toString}'", expected = "idx[n] -> dt")
      }
    )
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): DataExpr = {
    Generate(fun(n), VisitAndRebuild(f, fun), t.map(fun(_)))
  }
}
