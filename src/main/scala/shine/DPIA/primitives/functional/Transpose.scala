package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.imperative.TransposeAcc
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Transpose(n: Nat,
                           m: Nat,
                           dt: DataType,
                           access: AccessType,
                           array: Phrase[ExpType]
                          ) extends ExpPrimitive with FedeT {
  array :: expT(n`.`(m`.`dt), access)
  override val t: ExpType = expT(m`.`(n`.`dt), access)

  def fedeTranslation(env: Predef.Map[Identifier[ExpType], Identifier[AccType]])
                     (C: Phrase[AccType ->: AccType]): Phrase[AccType] =
    fedAcc(env)(array)(fun(AccType(C.t.inT.dataType))(o =>
      TransposeAcc(n, m, dt, C(o))))
}
