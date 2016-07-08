package OpenCL.MidLevelCombinators

import Compiling.SubstituteImplementations
import Core._
import DSL.typed._
import MidLevelCombinators.AbstractMapI
import OpenCL.Core.GlobalMemory
import OpenCL.LowLevelCombinators.ParForWorkgroup
import apart.arithmetic.ArithExpr

case class MapWorkgroupI(n: ArithExpr,
                         dt1: DataType,
                         dt2: DataType,
                         out: Phrase[AccType],
                         f: Phrase[AccType -> (ExpType -> CommandType)],
                         in: Phrase[ExpType])
  extends AbstractMapI(n, dt1, dt2, out, f, in) {

  override def makeMapI = MapWorkgroupI

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {

    ParForWorkgroup(n, dt2, out, λ( ExpType(int) ) { i => λ( AccType(dt2) ) { o =>

//      val access = (out `@` 0) `@` 0 // TODO: this is totally not generic ...
//      TypeChecker(access)
//      val identifier = ToOpenCL.acc(access, new ToOpenCL(?, ?))
//      val addressSpace = env.addressspace(identifier.name)
      val addressSpace = GlobalMemory

      SubstituteImplementations( f(o)(in `@` i), env.copy(env.addressSpace.updated(o.name, addressSpace)) )
    } } )

  }

}
