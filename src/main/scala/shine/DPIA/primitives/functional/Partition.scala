package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Partition(n: Nat,
                           m: Nat,
                           lenF: NatToNat,
                           dt: DataType,
                           array: Phrase[ExpType]
                          ) extends ExpPrimitive with ConT {
  array :: expT(n`.`dt, read)
  override val t: ExpType = expT(m`.d`{ i => lenF(i)`.`dt }, read)

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] =
    con(array)(λ(expT(n`.`dt, read))(x => C(Partition(n, m, lenF, dt, x))))
}
