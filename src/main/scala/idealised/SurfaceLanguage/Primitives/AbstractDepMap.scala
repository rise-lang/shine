package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.DPIA.freshName
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Types.TypeInference.SubstitutionMap
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import lift.arithmetic.NamedVar

abstract class AbstractDepMap(df: Expr[`(nat)->`[DataType -> DataType]],
                              array: DataExpr,
                              override val t: Option[DataType])
  extends PrimitiveExpr
{

  def makeMap: (Expr[`(nat)->`[DataType -> DataType]], DataExpr, Option[DataType]) => AbstractDepMap

  def makeDPIAMap: (
    DPIA.Nat,
      DPIA.NatIdentifier,
      DPIA.Types.DataType,
      DPIA.NatIdentifier,
      DPIA.Types.DataType,
      DPIA.Phrases.Phrase[DPIA.Types.NatDependentFunctionType[DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.ExpType]]],
      DPIA.Phrases.Phrase[DPIA.Types.ExpType]
    ) => DPIA.FunctionalPrimitives.AbstractDepMap


  override def convertToPhrase: DPIA.FunctionalPrimitives.AbstractDepMap = {
    (df.t, array.t) match {
      case (Some(NatDependentFunctionType(k, FunctionType(df1_k: DataType, df2_k: DataType))), Some(DepArrayType(n, NatDependentFunctionType(_, _)))) =>
        val i1 = NamedVar(freshName(), k.range)
        val dt1: DataType = Type.substitute(i1, `for`=k, in=df1_k)

        val i2 = NamedVar(freshName(), k.range)
        val dt2: DataType = Type.substitute(i2, `for`=k, in=df2_k)

        val fPhrase = df.toPhrase[DPIA.Types.NatDependentFunctionType[DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.ExpType]]]
        val inputPhrase = array.toPhrase[DPIA.Types.ExpType]

    makeDPIAMap(n, i1, dt1, i2, dt2,
          fPhrase,
      inputPhrase)
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
