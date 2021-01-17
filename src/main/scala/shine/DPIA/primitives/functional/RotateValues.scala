package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.intermediate.SlideSeqIValues
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class RotateValues(n: Nat,
                              sz: Nat,
                              dt: DataType,
                              write: Phrase[ExpType ->: ExpType],
                              input: Phrase[ExpType]
                             ) extends ExpPrimitive {
  write :: expT(dt, read) ->: expT(dt, shine.DPIA.Types.write)
  input :: expT((n - 1 + sz)`.`dt, read)
  override val t: ExpType = expT(n`.`(sz`.`dt), read)

  override def streamTranslation(C: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType])
                                (implicit context: TranslationContext): Phrase[CommType] = {
    val i = NatIdentifier(freshName("i"))
    str(input)(fun((i: NatIdentifier) ->:
      (expT(dt, read) ->: (comm: CommType)) ->: (comm: CommType)
    )(nextIn =>
      SlideSeqIValues(n, sz, 1, dt, dt,
        fun(expT(dt, read))(x =>
          fun(accT(dt))(o => acc(write(x))(o))),
        nextIn, C)
    ))
  }
}
