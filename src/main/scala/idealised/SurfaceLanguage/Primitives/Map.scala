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
    val array_ = TypeInference(array, subs)
    array_.t match {
      case Some(ArrayType(n, dt1)) =>
        val f_ = setParamAndInferType(f, dt1, subs)
        f_.t match {
          case Some(FunctionType(dt1_, dt2)) =>
            if (dt1 == dt1_) {
              makeMap(f_, array_, Some(ArrayType(n, dt2)))
            } else {
              error(expr = s"${this.getClass.getSimpleName}($f_, $array_)",
                found = s"`$dt1_'", expected = s"`$dt1'")
            }
          case x => error(expr = s"${this.getClass.getSimpleName}($f_, $array_)",
                      found = s"`${x.toString}'", expected = "dt1 -> dt2")
        }
      case x =>  error(expr = s"${this.getClass.getSimpleName}($f, $array_)",
        found = s"`${x.toString}'", expected = "n.dt")
    }
  }

//  override def inferTypes(subs: TypeInference.SubstitutionMap): Primitive[ExpType] = {
//    import TypeInference._
//    val array_ = TypeInference(array, subs)
//    array_.t match {
//      case ExpType(ArrayType(n_, dt1_)) =>
//        val f_ = setParamAndInferType(f, exp"[$dt1_]", subs)
//        f_.t match {
//          case FunctionType(ExpType(dt1__), ExpType(dt2_)) =>
//            if (dt1_ == dt1__) {
//              makePhraseMap(n_, dt1_, dt2_, f_, array_)
//            } else {
//              error(expr = s"${this.getClass.getSimpleName}($f_, $array_)",
//                found = s"`$dt1__'", expected = s"`$dt1_'")
//            }
//          case x => error(expr = s"${this.getClass.getSimpleName}($f_, $array_)",
//            found = s"`${x.toString}'", expected = "exp[dt1] -> exp[dt2]")
//        }
//      case x => error(expr = s"${this.getClass.getSimpleName}($f, $array_)",
//        found = s"`${x.toString}'", expected = "exp[n.dt]")
//    }
//  }

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
