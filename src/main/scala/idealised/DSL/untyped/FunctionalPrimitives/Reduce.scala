package idealised.DSL.untyped.FunctionalPrimitives

import idealised.Core._
import idealised.DSL.untyped.{VisitAndRebuild, _}

abstract class AbstractReduce(f: Expr[ExpType -> (ExpType -> ExpType)],
                              init: DataExpr, array: DataExpr)
  extends PrimitiveExpr {

  def makeReduce: (Expr[ExpType -> (ExpType -> ExpType)], DataExpr, DataExpr) => AbstractReduce

  def makePhraseReduce: (Nat, DataType, DataType,
    Phrase[ExpType -> (ExpType -> ExpType)], Phrase[ExpType], Phrase[ExpType]) =>
      idealised.FunctionalPrimitives.AbstractReduce

  override def inferTypes(subs: ExpressionToPhrase.SubstitutionMap): idealised.FunctionalPrimitives.AbstractReduce = {
    import ExpressionToPhrase._
    val array_ = ExpressionToPhrase(array, subs)
    val init_ = ExpressionToPhrase(init, subs)
    (init_.t, array_.t) match {
      case (ExpType(dt2_), ExpType(ArrayType(n_, dt1_))) =>
        val f_ = setParamsAndInferTypes(f, exp"[$dt1_]", exp"[$dt2_]", subs)
        f_.t match {
          case FunctionType(ExpType(t1), FunctionType(ExpType(t2), ExpType(t3))) =>
            if (dt1_ == t1 && dt2_ == t2 && dt2_ == t3) {
              makePhraseReduce(n_, dt1_, dt2_, f_, init_, array_)
            } else {
              error(dt1_.toString + ", " + t1.toString + " as well as " +
                dt2_.toString + ", " + t2.toString + " and " + t3.toString,
                expected = "them to match")
            }
          case x => error(x.toString, "FunctionType")
        }
      case x => error(x.toString, "(ExpType, ArrayType)")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): DataExpr = {
    makeReduce(VisitAndRebuild(f, fun), VisitAndRebuild(init, fun), VisitAndRebuild(array, fun))
  }

}

final case class Reduce(f: Expr[ExpType -> (ExpType -> ExpType)],
                        init: DataExpr,
                        array: DataExpr)
  extends AbstractReduce(f, init, array) {

  override def makePhraseReduce = idealised.FunctionalPrimitives.Reduce

  override def makeReduce = Reduce

}
