package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types.TypeInference.SubstitutionMap
import idealised.SurfaceLanguage.Types._

abstract class AbstractReduce(f: Expr[DataType -> (DataType -> DataType)],
                              init: DataExpr, array: DataExpr,
                              override val `type`: Option[DataType])
  extends PrimitiveExpr {

  def makeReduce: (Expr[DataType -> (DataType -> DataType)], DataExpr, DataExpr, Option[DataType]) => AbstractReduce

  def makeDPIAReduce: (
      DPIA.Nat,
      DPIA.Types.DataType,
      DPIA.Types.DataType,
      DPIA.Phrases.Phrase[DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.ExpType]]],
      DPIA.Phrases.Phrase[DPIA.Types.ExpType],
      DPIA.Phrases.Phrase[DPIA.Types.ExpType]
    ) => DPIA.FunctionalPrimitives.AbstractReduce


  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    (f.`type`, init.`type`, array.`type`) match {
      case (Some(FunctionType(t1, FunctionType(t2, t3))), Some(dt2), Some(ArrayType(n, dt1)))
        if dt1 == t1 && dt2 == t2 && dt2 == t3 =>
          makeDPIAReduce(n, dt1, dt2,
            f.toPhrase[DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.FunctionType[DPIA.Types.ExpType, DPIA.Types.ExpType]]],
            init.toPhrase[DPIA.Types.ExpType],
            array.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }

  }

  override def inferType(subs: SubstitutionMap): AbstractReduce = {
    import TypeInference._
    val array_ = TypeInference(array, subs)
    val init_ = TypeInference(init, subs)
    (init_.`type`, array_.`type`) match {
      case (Some(dt2), Some(ArrayType(n, dt1))) =>
        val f_ = setParamsAndInferTypes(f, dt1, dt2, subs)
        f_.`type` match {
          case Some(FunctionType(t1, FunctionType(t2, t3))) =>
            if (dt1 == t1 && dt2 == t2 && dt2 == t3) {
              makeReduce(f_, init_, array_, Some(dt2))
            } else {
              error(this.toString,
                dt1.toString + ", " + t1.toString + " as well as " +
                dt2.toString + ", " + t2.toString + " and " + t3.toString,
                expected = "them to match")
            }
          case x => error(expr = s"${this.getClass.getSimpleName}($f_, $init_, $array_)",
                      found = s"`${x.toString}'", expected = "dt1 -> (dt2 -> dt3)")
        }
      case x => error(expr = s"${this.getClass.getSimpleName}($f, $init_, $array_)",
              found = s"`${x.toString}'", expected = "(dt1, n.dt2)")
    }
  }

//  override def inferTypes(subs: TypeInference.SubstitutionMap): idealised.DPIA.FunctionalPrimitives.AbstractReduce = {
//    import TypeInference._
//    val array_ = TypeInference(array, subs)
//    val init_ = TypeInference(init, subs)
//    (init_.t, array_.t) match {
//      case (ExpType(dt2_), ExpType(ArrayType(n_, dt1_))) =>
//        val f_ = setParamsAndInferTypes(f, exp"[$dt1_]", exp"[$dt2_]", subs)
//        f_.t match {
//          case FunctionType(ExpType(t1), FunctionType(ExpType(t2), ExpType(t3))) =>
//            if (dt1_ == t1 && dt2_ == t2 && dt2_ == t3) {
//              makePhraseReduce(n_, dt1_, dt2_, f_, init_, array_)
//            } else {
//              error(this.toString,
//                dt1_.toString + ", " + t1.toString + " as well as " +
//                dt2_.toString + ", " + t2.toString + " and " + t3.toString,
//                expected = "them to match")
//            }
//          case x => error(expr = s"${this.getClass.getSimpleName}($f_, $init_, $array_)",
//            found = s"`${x.toString}'", expected = "exp[dt1] -> (exp[dt2] -> exp[dt3])")
//        }
//      case x => error(expr = s"${this.getClass.getSimpleName}($f, $init_, $array_)",
//        found = s"`${x.toString}'", expected = "(exp[dt1], exp[n.dt2])")
//    }
//  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): DataExpr = {
    makeReduce(VisitAndRebuild(f, fun),
      VisitAndRebuild(init, fun),
      VisitAndRebuild(array, fun),
      `type`.map(fun(_)))
  }

}

final case class Reduce(f: Expr[DataType -> (DataType -> DataType)],
                        init: DataExpr,
                        array: DataExpr,
                        override val `type`: Option[DataType] = None)
  extends AbstractReduce(f, init, array, `type`) {

  override def makeDPIAReduce = DPIA.FunctionalPrimitives.Reduce

  override def makeReduce = Reduce

}
