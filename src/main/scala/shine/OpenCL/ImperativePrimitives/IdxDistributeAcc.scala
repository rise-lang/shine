package shine.OpenCL.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.{AccIdentifier, Store}
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA.{Nat, Phrases, _}
import shine.OpenCL.ParallelismLevel

import scala.xml.Elem

final case class IdxDistributeAcc(m: Nat,
                                  n: Nat,
                                  stride: Nat,
                                  parallelismLevel: ParallelismLevel,
                                  dt: DataType,
                                  array: Phrase[AccType])
  extends AccPrimitive {

  array :: accT(m`.`dt)
  override val t: AccType = accT(n`.`dt)

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
