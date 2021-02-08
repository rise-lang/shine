package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.intermediate.CircularBufferI
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class CircularBuffer(n: Nat,
                                alloc: Nat,
                                sz: Nat,
                                dt1: DataType,
                                dt2: DataType,
                                load: Phrase[ExpType ->: ExpType],
                                input: Phrase[ExpType]
                               ) extends ExpPrimitive with StreamT {
  load :: expT(dt1, read) ->: expT(dt2, write)
  input :: expT((n - 1 + sz)`.`dt1, read)
  override val t: ExpType = expT(n`.`(sz`.`dt2), read)

  def streamTranslation(C: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType])
                       (implicit context: TranslationContext): Phrase[CommType] = {
    val i = NatIdentifier(freshName("i"))
    str(input)(fun((i: NatIdentifier) ->:
      (expT(dt1, read) ->: (comm: CommType)) ->: (comm: CommType)
    )(nextIn =>
      CircularBufferI(n, sz, 1, dt1, dt2,
        fun(expT(dt1, read))(x =>
          fun(accT(dt2))(o => acc(load(x))(o))),
        nextIn, C)
    ))
  }
}
