package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.imperative.ScatterAcc
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Scatter(n: Nat, m: Nat, dt: DataType,
                         indices: Phrase[ExpType],
                         input: Phrase[ExpType]
                        ) extends ExpPrimitive with AccT {
  indices :: expT(n`.`idx(m), read)
  input :: expT(n`.`dt, write)
  override val t: ExpType = expT(m`.`dt, write)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] =
    con(indices)(fun(expT(m`.`idx(n), read))(y =>
      acc(input)(ScatterAcc(n, m, dt, y, A))
    ))
}
