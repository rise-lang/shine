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
final case class IterateStream(n: Nat,
                               dt1: DataType,
                               dt2: DataType,
                               f: Phrase[ExpType ->: ExpType],
                               array: Phrase[ExpType]
                              ) extends ExpPrimitive with AcceptorTranslatable {
  f :: expT(dt1, read) ->: expT(dt2, write)
  array :: expT(n`.`dt1, read)
  override val t: ExpType = expT(n`.`dt2, write)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] = {
    val fI = λ(expT(dt1, read))(x => λ(accT(dt2))(o => acc(f(x))(o)))
    val i = NatIdentifier(freshName("i"))
    str(array)(fun((i: NatIdentifier) ->:
      (expT(dt1, read) ->: (comm: CommType)) ->: (comm: CommType)
    )(next =>
      comment("iterateStream") `;`
        forNat(n, i =>
          streamNext(next, i, fun(expT(dt1, read))(x => fI(x)(A `@` i))))
    ))
  }
}
