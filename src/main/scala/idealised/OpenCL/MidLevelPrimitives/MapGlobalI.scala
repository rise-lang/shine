package idealised.OpenCL.MidLevelPrimitives

import idealised.Compiling.SubstituteImplementations
import idealised.Core._
import idealised.DSL.typed._
import idealised.IntermediatePrimitives.AbstractMapI
import SubstituteImplementations._
import idealised.OpenCL.LowLevelPrimitives.ParForGlobal

final case class MapGlobalI(n: Nat,
                            dt1: DataType,
                            dt2: DataType,
                            f: Phrase[ExpType -> (AccType -> CommandType)],
                            in: Phrase[ExpType],
                            out: Phrase[AccType])
  extends AbstractMapI(n, dt1, dt2, f, in, out) {

  override def makeMapI = MapGlobalI

  override def substituteImpl(env: Environment): Phrase[CommandType] = {
    ParForGlobal(n, dt2, out, λ(exp"[idx($n)]")(i => λ(acc"[$dt2]")(o =>
      SubstituteImplementations(f(in `@` i)(o), env)
    )))
  }

}
