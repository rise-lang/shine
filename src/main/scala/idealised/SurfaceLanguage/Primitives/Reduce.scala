package idealised.SurfaceLanguage.Primitives

import idealised.DPIA._
import idealised.DPIA.Types._
import idealised.DPIA.Phrases.Phrase
import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.{Expr, PrimitiveExpr}

abstract class AbstractReduce(f: Expr[ExpType -> (ExpType -> ExpType)],
                              init: DataExpr, array: DataExpr)
  extends PrimitiveExpr {

  def makeReduce: (Expr[ExpType -> (ExpType -> ExpType)], DataExpr, DataExpr) => AbstractReduce

  def makePhraseReduce: (Nat, DataType, DataType,
    Phrase[ExpType -> (ExpType -> ExpType)], Phrase[ExpType], Phrase[ExpType]) =>
      idealised.DPIA.FunctionalPrimitives.AbstractReduce

  override def inferTypes(subs: TypeInference.SubstitutionMap): idealised.DPIA.FunctionalPrimitives.AbstractReduce = {
    import TypeInference._
    val array_ = TypeInference(array, subs)
    val init_ = TypeInference(init, subs)
    (init_.t, array_.t) match {
      case (ExpType(dt2_), ExpType(ArrayType(n_, dt1_))) =>
        val f_ = setParamsAndInferTypes(f, exp"[$dt1_]", exp"[$dt2_]", subs)
        f_.t match {
          case FunctionType(ExpType(t1), FunctionType(ExpType(t2), ExpType(t3))) =>
            if (dt1_ == t1 && dt2_ == t2 && dt2_ == t3) {
              makePhraseReduce(n_, dt1_, dt2_, f_, init_, array_)
            } else {
              error(this,
                dt1_.toString + ", " + t1.toString + " as well as " +
                dt2_.toString + ", " + t2.toString + " and " + t3.toString,
                expected = "them to match")
            }
          case x => error(this, x.toString, "exp[dt1] -> (exp[dt2] -> exp[dt3])")
        }
      case x => error(this, x.toString, "(exp[dt1], exp[n.dt2])")
    }
  }

  override def visitAndRebuild(fun: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    makeReduce(SurfaceLanguage.VisitAndRebuild(f, fun), SurfaceLanguage.VisitAndRebuild(init, fun), SurfaceLanguage.VisitAndRebuild(array, fun))
  }

}

final case class Reduce(f: Expr[ExpType -> (ExpType -> ExpType)],
                        init: DataExpr,
                        array: DataExpr)
  extends AbstractReduce(f, init, array) {

  override def makePhraseReduce = DPIA.FunctionalPrimitives.Reduce

  override def makeReduce = Reduce

}
