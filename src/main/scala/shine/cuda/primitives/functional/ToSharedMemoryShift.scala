package shine.cuda.primitives.functional

import shine.DPIA.Compilation.TranslationToImperative.acc
import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.FunctionalPrimitives.{Map, Take}
import shine.DPIA.ImperativePrimitives.MapAcc
import shine.DPIA.Phrases.{ExpPrimitive, Phrase, VisitAndRebuild}
import shine.DPIA.Semantics.OperationalSemantics.{Data, Store}
import shine.DPIA.Types.DataType._
import shine.DPIA.Types.{AccType, CommType, ExpType, _}
import shine.DPIA.{->:, Nat, Phrases, _}
import shine.OpenCL.ImperativePrimitives.{IdxDistributeAcc, OpenCLNew}
import shine.OpenCL.Sequential

import scala.xml.Elem

final case class ToSharedMemoryShift(shift: Nat,
                                     m: Nat,
                                     n: Nat,
                                     dt: DataType,
                                     array: Phrase[ExpType])
  extends ExpPrimitive {

  array :: expT(m`.`(n`.`dt), read)
  override val t: ExpType = expT(m`.`(n`.`dt), read)

  override def eval(s: Store): Data = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    ToSharedMemoryShift(fun.nat(shift), fun.nat(m), fun.nat(n), fun.data(dt), VisitAndRebuild(array, fun))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = ???

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext): Phrase[CommType] =
    OpenCLNew(AddressSpace.Local, ArrayType(m, ArrayType(n + shift, dt)),
      λ(VarType(ArrayType(m, ArrayType(n + shift, dt))))(sharedArray => {
        acc(array)(
          MapAcc(m, ArrayType(n + shift, dt), ArrayType(n, dt),
            λ(AccType(ArrayType(n + shift, dt)))(x =>
              IdxDistributeAcc(n + shift, n, 1, Sequential, dt, x)),
            sharedArray.wr)) `;`
        C(
          Map(m, ArrayType(n + shift, dt), ArrayType(n, dt), read,
            λ(ExpType(ArrayType(n + shift, dt), read))(x =>
              Take(n, shift, dt, x)),
            sharedArray.rd))
      }))


  override def xmlPrinter: Elem =
    <ToSharedMemoryShift shift={shift.toString} m={m.toString} n={n.toString} dt={dt.toString}>
      <array>
        {Phrases.xmlPrinter(array)}
      </array>
    </ToSharedMemoryShift>

  override def prettyPrint: String = s"(ToSharedMemoryShift $shift $array)"
}
