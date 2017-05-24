package idealised.DSL.untyped.FunctionalPrimitives

import idealised.Core._
import idealised.DSL.untyped.{VisitAndRebuild, _}

abstract class AbstractMap(f: Expr[ExpType -> ExpType], array: DataExpr)
  extends PrimitiveExpr {

  def makeMap: (Expr[ExpType -> ExpType], DataExpr) => AbstractMap

  def makePhraseMap: (Nat, DataType, DataType, Phrase[ExpType -> ExpType], Phrase[ExpType]) =>
    idealised.FunctionalPrimitives.AbstractMap

  override def inferTypes(subs: ExpressionToPhrase.SubstitutionMap): Primitive[ExpType] = {
    import ExpressionToPhrase._
    val array_ = ExpressionToPhrase(array, subs)
    array_.t match {
      case ExpType(ArrayType(n_, dt1_)) =>
        val f_ = setParamAndInferType(f, exp"[$dt1_]", subs)
        f_.t match {
          case FunctionType(ExpType(dt1__), ExpType(dt2_)) =>
            if (dt1_ == dt1__) {
              makePhraseMap(n_, dt1_, dt2_, f_, array_)
            } else {
              error(s"$dt1__", s"$dt1_")
            }
          case x => error(x.toString, "FunctionType")
        }
      case x => error(x.toString, "ArrayType")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): DataExpr = {
    makeMap(VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
  }

}

final case class Map(f: Expr[ExpType -> ExpType], array: DataExpr)
  extends AbstractMap(f, array) {

  override def makePhraseMap = idealised.FunctionalPrimitives.Map

  override def makeMap = Map

}
