package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.imperative.TakeAcc
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class PadEmpty(n: Nat,
                          r: Nat,
                          dt: DataType,
                          array: Phrase[ExpType]
                         ) extends ExpPrimitive with FedeT {
  array :: expT(n `.` dt, write)
  override val t: ExpType = expT((n + r)`.`dt, write)

  def fedeTranslation(env: Predef.Map[Identifier[ExpType], Identifier[AccType]])
                     (C: Phrase[AccType ->: AccType]): Phrase[AccType] =
    fedAcc(env)(array)(fun(accT(C.t.inT.dataType))(o =>
      TakeAcc(n, r, dt, C(o))))
}
