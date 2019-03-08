package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage

final case class Cast(dt: BasicType, e: DataExpr, override val t: Option[DataType] = None)
  extends PrimitiveExpr
{
  override def convertToPhrase: DPIA.FunctionalPrimitives.Cast = {
    e.t match {
      case Some(edt) =>
        DPIA.FunctionalPrimitives.Cast(toDPIABasicType(edt), toDPIABasicType(dt), e.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Cast = {
    import TypeInference._
    TypeInference(e, subs) |> (e =>
      e.t match {
        case Some(_) => Cast(dt, e, Some(dt))
        case x => error(expr = s"Cast($dt, $e)", found = s"`${x.toString}'", expected = "expr[dt]")
      }
    )
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): DataExpr = {
    Cast(fun(dt), VisitAndRebuild(e, fun), t.map(fun(_)))
  }

  private def toDPIABasicType(bt: DataType): DPIA.Types.BasicType = {
    bt match {
      case SurfaceLanguage.Types.NatType => DPIA.Types.NatType
      case SurfaceLanguage.Types.IndexType(n) => DPIA.Types.IndexType(n)
      case SurfaceLanguage.Types.bool => DPIA.Types.bool
      case SurfaceLanguage.Types.int => DPIA.Types.int
      case SurfaceLanguage.Types.float => DPIA.Types.float
      case SurfaceLanguage.Types.double => DPIA.Types.double
      case dt => throw new Exception(s"Expected BasicType but found $dt")
    }
  }
}
