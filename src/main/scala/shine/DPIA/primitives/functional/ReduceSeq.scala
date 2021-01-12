package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.primitives.intermediate.ReduceSeqI
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType}
import shine.DPIA.{->:, Nat}

final case class ReduceSeq(n: Nat,
                           dt1: DataType,
                           dt2: DataType,
                           f: Phrase[ExpType ->: ExpType ->: ExpType],
                           init: Phrase[ExpType],
                           array: Phrase[ExpType])
  extends AbstractReduce(n, dt1, dt2, f, init, array) {
  override def makeReduce = ReduceSeq

  override def makeReduceI(n: Nat,
                           dt1: DataType,
                           dt2: DataType,
                           f: Phrase[->:[ExpType, ->:[ExpType, ->:[AccType, CommType]]]],
                           init: Phrase[ExpType],
                           array: Phrase[ExpType],
                           out: Phrase[->:[ExpType, CommType]])
                          (implicit context: TranslationContext): Phrase[CommType] =
    ReduceSeqI(n, dt1, dt2, f, init, array, out)
}
