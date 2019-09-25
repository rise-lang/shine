package idealised.OpenCL.ImperativePrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.{Nat, Phrases}
import idealised.DPIA.Phrases.{ExpPrimitive, Phrase, PrettyPhrasePrinter, ToString, VisitAndRebuild}
import idealised.DPIA.Semantics.OperationalSemantics.{Data, Store}
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.ParallelismLevel

import scala.xml.Elem

final case class IdxDistribute(m: Nat,
                               n: Nat,
                               stride: Nat,
                               parallelismLevel: ParallelismLevel,
                               dt: DataType,
                               array: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (m: Nat) ->: (n: Nat) ->: (stride: Nat) ->: (dt: DataType) ->:
      (array :: exp"[$m.$dt, $read]") ->:
        exp"[$n.$dt, $read]"

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
    ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] =
    ???
}
