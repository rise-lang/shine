package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types.TypeInference.SubstitutionMap
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

abstract class AbstractDepMap(val df: Expr,
                              val array: Expr,
                              override val t: Option[DataType])
  extends PrimitiveExpr
{

  def makeMap: (Expr, Expr, Option[DataType]) => AbstractDepMap

  def dfI: NatIdentifier = df match {
    case NatDependentLambdaExpr(i, _) => i
    case x => TypeInference.error(expr = this.toString, found= x.toString, expected = NatDependentLambdaExpr.toString)
  }

  def dfF: Expr = df match {
    case NatDependentLambdaExpr(_, f) => f
    case x => TypeInference.error(expr = this.toString, found= x.toString, expected = NatDependentLambdaExpr.toString)
  }

  override def inferType(subs: SubstitutionMap): Expr = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(DepArrayType(n, NatDependentFunctionType(j, df1))) =>

          setParamsAndInferTypes(df, Type.substitute(_, `for`=j, in=df1), subs) |> (df =>
            df.t match {
              case Some(NatDependentFunctionType(i, FunctionType(_, df2: DataType))) =>
                makeMap(df, array, Some(DepArrayType(n, Type.substitute(_, `for`=i, `in`=df2))))
              case x => error(expr = s"${this.getClass.getSimpleName}($df, $array)",
                found = s"`${x.toString}'", expected = "(nat) -> df1 -> df2")
            }
            )
        case x => error(expr = s"${this.getClass.getSimpleName}($df, $array)",
          found = s"`${x.toString}'", expected = "n.(j:nat -> dft)")
      })
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Expr = {
    makeMap(VisitAndRebuild(df, fun), VisitAndRebuild(array, fun), t.map(fun(_)))
  }

}
