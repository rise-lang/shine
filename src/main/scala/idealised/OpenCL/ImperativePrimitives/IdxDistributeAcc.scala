package idealised.OpenCL.ImperativePrimitives

import idealised.DPIA.{Nat, Phrases}
import idealised.DPIA.Phrases.{AccPrimitive, Phrase, PrettyPhrasePrinter, ToString, VisitAndRebuild}
import idealised.DPIA.Semantics.OperationalSemantics.{AccIdentifier, Store}
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.ParallelismLevel

import scala.xml.Elem

final case class IdxDistributeAcc(m: Nat,
                                  n: Nat,
                                  stride: Nat,
                                  parallelismLevel: ParallelismLevel,
                                  dt: DataType,
                                  array: Phrase[AccType])
  extends AccPrimitive {

  override val t: AccType =
    (m: Nat) ->: (n: Nat) ->: (stride: Nat) ->: (dt: DataType) ->:
      (array :: acc"[$m.$dt]") ->: acc"[$n.$dt]"

  override def eval(s: Store): AccIdentifier = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    IdxDistributeAcc(fun.nat(m), fun.nat(n), fun.nat(stride), parallelismLevel, fun.data(dt), VisitAndRebuild(array, fun))
  }

  override def prettyPrint: String = s"IdxStrideDistributeAcc($n, $stride, ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <idxStrideDistributeAcc n={ToString(n)} m={ToString(m)} stride={ToString(stride)} dt={ToString(dt)}>
      <input type={ToString(ExpType(ArrayType(n, dt), read))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </idxStrideDistributeAcc>
}
