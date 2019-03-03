package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.DPIA.NatDataTypeFunction
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
      DPIA.NatDataTypeFunction,
      DPIA.NatDataTypeFunction,
      DPIA.Phrases.Phrase[DPIA.Types.NatDependentFunctionType[DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.ExpType]]],
      DPIA.Phrases.Phrase[DPIA.Types.ExpType]
    ) => DPIA.FunctionalPrimitives.AbstractDepMap


  override def convertToPhrase: DPIA.FunctionalPrimitives.AbstractDepMap = {
    (df.t, array.t) match {
      case (Some(NatDependentFunctionType(k, FunctionType(df1_k: DataType, df2_k: DataType))), Some(DepArrayType(n, NatDependentFunctionType(_, _)))) =>

        val ft1 = NatDataTypeFunction(n, (x:NatIdentifier) => Type.substitute[DataType](x, `for`=k, in=df1_k))
        val ft2 = NatDataTypeFunction(n, (x:NatIdentifier) => Type.substitute[DataType](x, `for`=k, in=df2_k))

        val fPhrase = df.toPhrase[DPIA.Types.NatDependentFunctionType[DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.ExpType]]]
        val inputPhrase = array.toPhrase[DPIA.Types.ExpType]

    makeDPIAMap(n, ft1, ft2, fPhrase, inputPhrase)
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
        case Some(DepArrayType(n, NatDependentFunctionType(j, df1))) =>

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
