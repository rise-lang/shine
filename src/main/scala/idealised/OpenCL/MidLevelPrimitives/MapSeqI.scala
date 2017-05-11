package idealised.OpenCL.MidLevelPrimitives

import idealised.Compiling.SubstituteImplementations
import idealised.Core._
import idealised.DSL.typed._
import idealised.IntermediatePrimitives.AbstractMapI
import SubstituteImplementations._

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
