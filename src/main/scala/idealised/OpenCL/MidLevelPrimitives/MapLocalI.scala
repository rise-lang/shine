package idealised.OpenCL.MidLevelPrimitives

import idealised.Compiling.SubstituteImplementations
import idealised.Core._
import idealised.DSL.typed._
import idealised.IntermediatePrimitives.AbstractMapI
import SubstituteImplementations._
import idealised.OpenCL.LowLevelPrimitives.ParForLocal

final case class MapLocalI(n: Nat,
                           dt1: DataType,
                           dt2: DataType,
                           out: Phrase[AccType],
                           f: Phrase[AccType -> (ExpType -> CommandType)],
                           in: Phrase[ExpType])
  extends AbstractMapI(n, dt1, dt2, out, f, in) {

  override def makeMapI = MapLocalI

  override def substituteImpl(env: Environment): Phrase[CommandType] = {
    ParForLocal(n, dt2, out, λ(exp"[idx($n)]")(i => λ(acc"[$dt2]")(o =>
      SubstituteImplementations(f(o)(in `@` i), env)
    )))
  }

}
