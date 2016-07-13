package HighLevelCombinators

import Compiling.RewriteToImperative
import Core.OperationalSemantics._
import Core.VisitAndRebuild.fun
import Core._
import DSL.typed._

import scala.xml.Elem

final case class Gather(n: Nat,
                        dt: DataType,
                        idxF: Phrase[`(nat)->`[ExpType -> ExpType]],
                        array: Phrase[ExpType])
  extends HighLevelCombinator {

  override lazy val `type` = exp"[$n.$dt]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    idxF match {
      case NatDependentLambdaPhrase(m, _) =>
        (n: Nat) -> (dt: DataType) ->
          (idxF `:` t"($m : nat) -> exp[idx($m)] -> exp[idx($m)]") ->
          (array `:` exp"[$n.$dt]") ->
          `type`
      case _ => throw new Exception("This should not happen")
    }
  }

  override def inferTypes: Gather = {
    import TypeInference._
    val array_ = TypeInference(array)
    val idxF_ = TypeInference(idxF)
    array_.t match {
      case ExpType(ArrayType(n_, dt_)) => Gather(n_, dt_, idxF_, array_)
      case x => error(x.toString, "ExpType(ArrayType)")
    }
  }

  override def eval(s: Store): Data = {
//    import OperationalSemantics._
//    OperationalSemantics.eval(s, array) match {
//      case ArrayData(a) =>
//        val res = Array[Data](a.length)
//        for (i <- a.indices) {
//          res(i) = a(idxF(i, array.t.dataType).eval)
//        }
//        ArrayData(res.toVector)
//      case _ => throw new Exception("This should not happen")
//    }
    ???
  }

  override def visitAndRebuild(fun: fun): Phrase[ExpType] =
    Gather(fun(n), fun(dt), idxF, VisitAndRebuild(array, fun))

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    exp(this)(λ(exp"[$n.$dt]")(x =>
      acc(x)(A)
    ))
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    import RewriteToImperative._

    val e = array

    exp(e)(λ(exp"[$n.$dt]")( x =>
      C(Gather(n, dt, idxF, x))
    ))
  }

  override def prettyPrint: String = s"(gather idxF ${PrettyPrinter(array)})"

  override def xmlPrinter: Elem =
    <gather ixdF={ToString(idxF)}>
      {Core.xmlPrinter(array)}
    </gather>
}
