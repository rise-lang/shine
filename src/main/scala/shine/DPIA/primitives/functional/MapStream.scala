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
final case class MapStream(n: Nat,
                           dt1: DataType,
                           dt2: DataType,
                           f: Phrase[ExpType ->: ExpType],
                           array: Phrase[ExpType]
                          ) extends ExpPrimitive {
  f :: expT(dt1, read) ->: expT(dt2, write)
  array :: expT(n`.`dt1, read)
  override val t: ExpType = expT(n`.`dt2, write)

  override def streamTranslation(C: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType])
                                (implicit context: TranslationContext): Phrase[CommType] = {
    val i = NatIdentifier(freshName("i"))
    str(array)(fun((i: NatIdentifier) ->:
      (expT(dt1, read) ->: (comm: CommType)) ->: (comm: CommType)
    )(next =>
      C(nFun(i =>
        fun(expT(dt2, read) ->: (comm: CommType))(k =>
          streamNext(next, i, fun(expT(dt1, read))(x =>
            con(f(x))(k)
          ))
        ), arithexpr.arithmetic.RangeAdd(0, n, 1)))
    ))
  }
}
