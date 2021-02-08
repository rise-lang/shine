package shine.cuda.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.acc
import shine.DPIA.DSL._
import shine.DPIA.Phrases.{ConT, ExpPrimitive, Phrase}
import shine.DPIA.Types.DataType._
import shine.DPIA.Types.{AccType, CommType, ExpType, _}
import shine.DPIA.primitives.functional.Take
import shine.DPIA.primitives.imperative.MapAcc
import shine.DPIA.{->:, Nat, _}
import shine.OpenCL.Sequential
import shine.OpenCL.primitives.imperative.{IdxDistributeAcc, New}
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class ToSharedMemoryShift(shift: Nat,
                                     m: Nat,
                                     n: Nat,
                                     dt: DataType,
                                     array: Phrase[ExpType]) extends ExpPrimitive with ConT {

  array :: expT(m`.`(n`.`dt), write)
  override val t: ExpType = expT(m`.`(n`.`dt), read)

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext): Phrase[CommType] =
     New(AddressSpace.Local, ArrayType(m, ArrayType(n + shift, dt)),
      λ(VarType(ArrayType(m, ArrayType(n + shift, dt))))(sharedArray => {
        acc(array)(
          MapAcc(m, ArrayType(n + shift, dt), ArrayType(n, dt),
            λ(AccType(ArrayType(n + shift, dt)))(x =>
              IdxDistributeAcc(n + shift, n, 1, Sequential, dt, x)),
            sharedArray.wr)) `;`
        C(
          shine.DPIA.primitives.functional.Map(m, ArrayType(n + shift, dt), ArrayType(n, dt), read,
            λ(ExpType(ArrayType(n + shift, dt), read))(x =>
              Take(n, shift, dt, x)),
            sharedArray.rd))
      }))
}
