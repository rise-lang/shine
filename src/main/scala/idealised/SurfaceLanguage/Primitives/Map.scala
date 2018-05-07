package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.Types.TypeInference.SubstitutionMap
import idealised.SurfaceLanguage._

abstract class AbstractMap(f: Expr[DataType -> DataType],
                           array: DataExpr,
                           override val t: Option[DataType])
  extends PrimitiveExpr
{

  def makeMap: (Expr[DataType -> DataType], DataExpr, Option[DataType]) => AbstractMap

  def makeDPIAMap: (
    DPIA.Nat,
    DPIA.Types.DataType,
    DPIA.Types.DataType,
    DPIA.Phrases.Phrase[DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.ExpType]],
    DPIA.Phrases.Phrase[DPIA.Types.ExpType]
    ) => DPIA.FunctionalPrimitives.AbstractMap


  override def convertToPhrase: DPIA.FunctionalPrimitives.AbstractMap = {
    (f.t, array.t) match {
      case (Some(FunctionType(dt1, dt2)), Some(ArrayType(n, dt1_))) if dt1 == dt1_ =>
        makeDPIAMap(n, dt1, dt2,
          f.toPhrase[DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.ExpType]],
          array.toPhrase[DPIA.Types.ExpType]
        )
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: SubstitutionMap): DataExpr = {
    import TypeInference._
    TypeInference(array, subs) |> (array =>
      array.t match {
        case Some(ArrayType(n, dt1)) =>
          setParamAndInferType(f, dt1, subs) |> (f =>
            f.t match {
              case Some(FunctionType(dt1_, dt2)) =>
                if (dt1 == dt1_) {
                  makeMap(f, array, Some(ArrayType(n, dt2)))
                } else {
                  error(expr = s"${this.getClass.getSimpleName}($f, $array)",
                    found = s"`$dt1_'", expected = s"`$dt1'")
                }
              case x => error(expr = s"${this.getClass.getSimpleName}($f, $array)",
                found = s"`${x.toString}'", expected = "dt1 -> dt2")
            })
        case x => error(expr = s"${this.getClass.getSimpleName}($f, $array)",
          found = s"`${x.toString}'", expected = "n.dt")
      })
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): DataExpr = {
    makeMap(VisitAndRebuild(f, fun), VisitAndRebuild(array, fun), t.map(fun(_)))
  }

}

final case class Map(f: Expr[DataType -> DataType], array: DataExpr,
                     override val t: Option[DataType] = None)
  extends AbstractMap(f, array, t) {

  override def makeDPIAMap = DPIA.FunctionalPrimitives.Map

  override def makeMap = Map
}
