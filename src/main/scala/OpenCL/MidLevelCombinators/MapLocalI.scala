package OpenCL.MidLevelCombinators

import Compiling.SubstituteImplementations
import Core._
import DSL.typed._
import MidLevelCombinators.AbstractMapI
import OpenCL.LowLevelCombinators.ParForLocal
import apart.arithmetic.ArithExpr

case class MapLocalI(n: ArithExpr,
                     dt1: DataType,
                     dt2: DataType,
                     out: Phrase[AccType],
                     f: Phrase[AccType -> (ExpType -> CommandType)],
                     in: Phrase[ExpType])
  extends AbstractMapI(n, dt1, dt2, out, f, in) {

  override def makeMapI = MapLocalI

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {
    ParForLocal(n, dt2, out, λ( ExpType(int) ) { i => λ( AccType(dt2) ) { o =>
      SubstituteImplementations( f(o)(in `@` i), env )
    } })
  }

}
