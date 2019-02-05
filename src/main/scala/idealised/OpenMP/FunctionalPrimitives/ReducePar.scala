package idealised.OpenMP.FunctionalPrimitives

import idealised.DPIA.FunctionalPrimitives.AbstractReduce
import idealised.OpenMP.IntermediatePrimitives.ReduceParI
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{DataType, ExpType}
import idealised.DPIA._

//noinspection TypeAnnotation
final case class ReducePar(n: Nat,
                           dt1: DataType, dt2: DataType,
                           f: Phrase[ExpType -> (ExpType -> ExpType)],
                           init: Phrase[ExpType],
                           array: Phrase[ExpType])
  extends AbstractReduce(n, dt1, dt2, f, init, array)
{
  override def makeReduce = ReducePar
  override def makeReduceI = ReduceParI
}
