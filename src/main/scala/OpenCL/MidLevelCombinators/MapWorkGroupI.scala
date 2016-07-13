package OpenCL.MidLevelCombinators

import Compiling.SubstituteImplementations
import Core._
import DSL.typed._
import MidLevelCombinators.AbstractMapI
import SubstituteImplementations._
import OpenCL.Core.GlobalMemory
import OpenCL.LowLevelCombinators.ParForWorkGroup

final case class MapWorkGroupI(n: Nat,
                               dt1: DataType,
                               dt2: DataType,
                               out: Phrase[AccType],
                               f: Phrase[AccType -> (ExpType -> CommandType)],
                               in: Phrase[ExpType])
  extends AbstractMapI(n, dt1, dt2, out, f, in) {

  override def makeMapI = MapWorkGroupI

  override def substituteImpl(env: Environment): Phrase[CommandType] = {

    ParForWorkGroup(n, dt2, out, λ(exp"[idx($n)]")(i => λ(acc"[$dt2]")(o => {

      //      val access = (out `@` 0) `@` 0 // TODO: this is totally not generic ...
      //      TypeChecker(access)
      //      val identifier = ToOpenCL.acc(access, new ToOpenCL(?, ?))
      //      val addressSpace = env.addressspace(identifier.name)
      val addressSpace = GlobalMemory

      SubstituteImplementations(f(o)(in `@` i),
        env.copy(env.addressSpace.updated(o.name, addressSpace)))
    })))

  }

}
