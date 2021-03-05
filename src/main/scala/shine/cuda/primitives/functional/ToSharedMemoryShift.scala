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

/**
  * Returns a copy of a matrix in shared memory with `shift` elements spacing between two consecutive rows. <br>
  * This can be used to avoid bank conflicts when using Tensor Cores.
  * @param shift  number of elements spacing between two consecutive
  * @param m      number of rows
  * @param n      number of columnd
  * @param dt     datatype of the elemnts of the matrix
  * @param matrix matrix which should be copied to shared memory
  */
@expPrimitive
final case class ToSharedMemoryShift(shift: Nat,
                                     m: Nat,
                                     n: Nat,
                                     dt: DataType,
                                     matrix: Phrase[ExpType]) extends ExpPrimitive with ConT {

  matrix :: expT(m`.`(n`.`dt), write)
  override val t: ExpType = expT(m`.`(n`.`dt), read)

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext): Phrase[CommType] =
     New(AddressSpace.Local, ArrayType(m, ArrayType(n + shift, dt)),
      λ(VarType(ArrayType(m, ArrayType(n + shift, dt))))(sharedArray => {
        acc(matrix)(
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
