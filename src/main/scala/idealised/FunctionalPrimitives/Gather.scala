package idealised.FunctionalPrimitives

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._
import idealised.Core.VisitAndRebuild.Visitor
import idealised.Compiling.RewriteToImperative
import idealised.DSL.typed._

import scala.language.postfixOps
import scala.language.reflectiveCalls

import scala.xml.Elem

final case class Gather(n: Nat,
                        dt: DataType,
                        idxF: Phrase[ExpType -> ExpType],
                        array: Phrase[ExpType])
  extends ExpPrimitive {

  override lazy val `type` = exp"[$n.$dt]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (dt: DataType) ->
      (idxF :: t"exp[idx($n)] -> exp[idx($n)]") ->
      (array :: exp"[$n.$dt]") ->
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
    import OperationalSemantics._
    val idxFE = OperationalSemantics.eval(s, idxF)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(a) =>
        val res = new Array[Data](a.length)
        for (i <- a.indices) {
          res(i) = a(OperationalSemantics.evalIndexExp(s, idxFE(i)).eval)
        }
        ArrayData(res.toVector)
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(fun: Visitor): Phrase[ExpType] =
    Gather(fun(n), fun(dt), idxF, VisitAndRebuild(array, fun))

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._

    con(this)(λ(exp"[$n.$dt]")(x =>
      acc(x)(A)
    ))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    import RewriteToImperative._

    con(array)(λ(exp"[$n.$dt]")(x =>
      C(Gather(n, dt, idxF, x))
    ))
  }

  override def prettyPrint: String = s"(gather idxF ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <gather>
      <idxF>{Core.xmlPrinter(idxF)}</idxF>
      <input>{Core.xmlPrinter(array)}</input>
    </gather>
}
