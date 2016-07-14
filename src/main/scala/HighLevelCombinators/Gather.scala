package HighLevelCombinators

import Compiling.RewriteToImperative
import Core.OperationalSemantics._
import Core.VisitAndRebuild.fun
import Core._
import DSL.typed._

import scala.language.postfixOps
import scala.language.reflectiveCalls

import scala.xml.Elem

final case class Gather(n: Nat,
                        dt: DataType,
                        idxF: Phrase[ExpType -> ExpType],
                        array: Phrase[ExpType])
  extends HighLevelCombinator {

  override lazy val `type` = exp"[$n.$dt]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (dt: DataType) ->
      (idxF `:` t"exp[idx($n)] -> exp[idx($n)]") ->
      (array `:` exp"[$n.$dt]") ->
      `type`
  }

  override def inferTypes: Gather = {
    import TypeInference._
    val array_ = TypeInference(array)
    array_.t match {
      case ExpType(ArrayType(n_, dt_)) =>
        val idxF_ = TypeInference(idxF)
        idxF_.t match {
          case FunctionType(ExpType(IndexType(m: NatIdentifier)), _) =>
            Gather(n_, dt_, idxF_ `[` n_ `/` m `]`, array_)
          case FunctionType(ExpType(IndexType(m1: Nat)), ExpType(IndexType(m2: Nat)))
            if n_ == m1 && n_ == m2 => Gather(n_, dt_, idxF_, array_)
          case x => error(x.toString, "exp[idx(n)] -> exp[idx(n)]")
        }
      case x => error(x.toString, "exp[n.dt]")
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

    exp(e)(λ(exp"[$n.$dt]")(x =>
      C(Gather(n, dt, idxF, x))
    ))
  }

  override def prettyPrint: String = s"(gather idxF ${PrettyPrinter(array)})"

  override def xmlPrinter: Elem =
    <gather>
      <idxF>{Core.xmlPrinter(idxF)}</idxF>
      <input>{Core.xmlPrinter(array)}</input>
    </gather>
}
