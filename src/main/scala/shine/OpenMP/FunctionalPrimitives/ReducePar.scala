package shine.OpenMP.FunctionalPrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.FunctionalPrimitives.AbstractReduce
import shine.OpenMP.IntermediatePrimitives.ReduceParI
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType}
import shine.DPIA._

//noinspection TypeAnnotation
final case class ReducePar(n: Nat,
                           dt1: DataType, dt2: DataType,
                           f: Phrase[ExpType ->: ExpType ->: ExpType],
                           init: Phrase[ExpType],
                           array: Phrase[ExpType])
  extends AbstractReduce(n, dt1, dt2, f, init, array)
{
  override def makeReduce = ReducePar

  override def makeReduceI(n: Nat,
                           dt1: DataType,
                           dt2: DataType,
                           f: Phrase[->:[ExpType, ->:[ExpType, ->:[AccType, CommType]]]],
                           init: Phrase[ExpType],
                           array: Phrase[ExpType],
                           out: Phrase[->:[ExpType, CommType]])
                          (implicit context: TranslationContext): Phrase[CommType] =
    ReduceParI(n, dt1, dt2, f, init, array, out)
}
