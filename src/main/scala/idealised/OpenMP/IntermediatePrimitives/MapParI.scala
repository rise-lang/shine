package idealised.OpenMP.IntermediatePrimitives

import idealised.DPIA.Compilation.SubstituteImplementations
import idealised.DPIA.Compilation.SubstituteImplementations._
import idealised.DPIA.DSL._
import idealised.DPIA.IntermediatePrimitives.AbstractMapI
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.DPIA._
import idealised.OpenMP.DSL._

final case class MapParI(n: Nat,
                         dt1: DataType,
                         dt2: DataType,
                         f: Phrase[ExpType -> (AccType -> CommandType)],
                         in: Phrase[ExpType],
                         out: Phrase[AccType])
  extends AbstractMapI(n, dt1, dt2, f, in, out) {

  override def makeMapI: (Nat, DataType, DataType, Phrase[ExpType -> (AccType -> CommandType)], Phrase[ExpType], Phrase[AccType]) => MapParI = MapParI

  override def substituteImpl(env: Environment): Phrase[CommandType] = {
    parFor(n, dt2, out, i => a =>
      SubstituteImplementations(f(in `@` i)(a), env)
    )
  }

}
