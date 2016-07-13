package OpenCL.MidLevelCombinators

import Compiling.SubstituteImplementations
import Core._
import DSL.typed._
import MidLevelCombinators.AbstractMapI
import SubstituteImplementations._

final case class MapSeqI(n: Nat,
                         dt1: DataType,
                         dt2: DataType,
                         out: Phrase[AccType],
                         f: Phrase[AccType -> (ExpType -> CommandType)],
                         in: Phrase[ExpType])
  extends AbstractMapI(n, dt1, dt2, out, f, in) {

  override def makeMapI = MapSeqI

  override def substituteImpl(env: Environment): Phrase[CommandType] = {
    `for`(n, i =>
      SubstituteImplementations(f(out `@` i)(in `@` i), env)
    )
  }

}
