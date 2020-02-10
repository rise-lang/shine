package shine.OpenCL.ImperativePrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.{Data, Store}
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA.{Nat, Phrases, _}
import shine.OpenCL.ParallelismLevel

import scala.xml.Elem

final case class IdxDistribute(m: Nat,
                               n: Nat,
                               stride: Nat,
                               parallelismLevel: ParallelismLevel,
                               dt: DataType,
                               array: Phrase[ExpType])
  extends ExpPrimitive {

  array :: expT(m`.`dt, read)
  override val t: ExpType = expT(n`.`dt, read)

  override def eval(s: Store): Data = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    IdxDistribute(fun.nat(m), fun.nat(n), fun.nat(stride), parallelismLevel, fun.data(dt), VisitAndRebuild(array, fun))
  }

  override def prettyPrint: String = s"IdxStrideDistribute($n, $stride, ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <idxStrideDistribute n={ToString(n)} m={ToString(m)} stride={ToString(stride)} dt={ToString(dt)}>
      <input type={ToString(ExpType(ArrayType(n, dt), read))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </idxStrideDistribute>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] =
    throw new Exception("This should never happen.")

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] =
    throw new Exception("This should never happen.")
}
