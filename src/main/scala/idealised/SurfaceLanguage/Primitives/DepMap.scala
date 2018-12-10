package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Types.TypeInference.SubstitutionMap
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

abstract class AbstractDepMap(df: Expr[`(nat)->`[DataType -> DataType]],
                              array: DataExpr,
                              override val t: Option[DataType])
  extends PrimitiveExpr
{

  def makeMap: (Expr[`(nat)->`[DataType -> DataType]], DataExpr, Option[DataType]) => AbstractDepMap

  def makeDPIAMap: (
    DPIA.Nat,
    DPIA.Types.DataType,
    DPIA.Types.DataType,
    DPIA.Phrases.Phrase[DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.ExpType]],
    DPIA.Phrases.Phrase[DPIA.Types.ExpType]
    ) => DPIA.FunctionalPrimitives.AbstractMap


  override def convertToPhrase: DPIA.FunctionalPrimitives.AbstractMap = {
    (df.t, array.t) match {
      case (Some(NatDependentFunctionType(idx, FunctionType(dt1, dt2))), Some(ArrayType(n, dt1_))) if dt1 == dt1_ =>
        ???
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: SubstitutionMap): DataExpr = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      this.df match {
        case NatDependentLambdaExpr(l, f) =>
          array.t match {
            case Some(ArrayType(n, dt1)) =>
              setParamAndInferType(f, dt1, subs) |> (f =>
                f.t match {
                  case Some(FunctionType(dt1_, dt2)) =>
                    if (dt1 == dt1_) {
                      makeMap(NatDependentLambdaExpr(l, f), array, Some(ArrayType(n, dt2)))
                    } else {
                      error(expr = s"${this.getClass.getSimpleName}($f, $array)",
                        found = s"`$dt1_'", expected = s"`$dt1'")
                    }
                  case x => error(expr = s"${this.getClass.getSimpleName}($f, $array)",
                    found = s"`${x.toString}'", expected = "dt1 -> dt2")
                })
            case x => error(expr = s"${this.getClass.getSimpleName}($f, $array)",
              found = s"`${x.toString}'", expected = "n.dt")
          }
        case _ => error(expr = s"${this.getClass.getSimpleName}($df, $array)",
          found = s"`${df.toString}'", expected = NatDependentLambdaExpr.toString)
      })
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): DataExpr = {
    makeMap(VisitAndRebuild(df, fun), VisitAndRebuild(array, fun), t.map(fun(_)))
  }

}

final case class DepMap(df: Expr[`(nat)->`[DataType -> DataType]], array: DataExpr,
                        override val t: Option[DataType] = None)
  extends AbstractDepMap(df, array, t) {

  override def makeDPIAMap = ???

  override def makeMap = DepMap
}
