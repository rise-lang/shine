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

  def dfI: NatIdentifier = df match {
    case NatDependentLambdaExpr(i, _) => i
    case x => TypeInference.error(expr = this.toString, found= x.toString, expected = NatDependentLambdaExpr.toString)
  }

  def dfF: Expr[->[DataType, DataType]] = df match {
    case NatDependentLambdaExpr(_, f) => f
    case x => TypeInference.error(expr = this.toString, found= x.toString, expected = NatDependentLambdaExpr.toString)
  }

  override def inferType(subs: SubstitutionMap): DataExpr = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(DepArrayType(n, NatDependentFunctionType(j: NatIdentifier, df1))) =>

          setParamsAndInferTypes(df, Type.substitute(_, `for`=j, in=df1), subs) |> (df =>
            df.t match {
              case Some(NatDependentFunctionType(i, FunctionType(_, df2))) =>
                makeMap(df, array, Some(DepArrayType(n, Type.substitute(_, `for`=i, `in`=df2))))
              case x => error(expr = s"${this.getClass.getSimpleName}($df, $array)",
                              found = s"`${x.toString}'", expected = "(nat) -> df1 -> df2")
            }
          )
        case x => error(expr = s"${this.getClass.getSimpleName}($df, $array)",
          found = s"`${x.toString}'", expected = "n.(j:nat -> dft)")
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
