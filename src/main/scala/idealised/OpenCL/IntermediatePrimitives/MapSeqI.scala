package idealised.OpenCL.IntermediatePrimitives

import idealised.DPIA.Compilation.SubstituteImplementations
import idealised.DPIA.DSL._
import idealised.DPIA.IntermediatePrimitives.AbstractMapI
import SubstituteImplementations._
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.DPIA._
import idealised.DPIA.DSL.`for`

final case class MapSeqI(n: Nat,
                         dt1: DataType,
                         dt2: DataType,
                         f: Phrase[ExpType -> (AccType -> CommandType)],
                         in: Phrase[ExpType],
                         out: Phrase[AccType])
  extends AbstractMapI(n, dt1, dt2, f, in, out) {

  override def makeMapI = MapSeqI

  override def substituteImpl(env: Environment): Phrase[CommandType] = {
    `for`(n, i =>
      SubstituteImplementations(f(in `@` i)(out `@` i), env)
    )
  }

}
