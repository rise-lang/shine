package CommandPatterns

import Core._
import DSL._
import Compiling.SubstituteImplementations
import apart.arithmetic.ArithExpr

case class MapSeqI(n: ArithExpr,
                   dt1: DataType,
                   dt2: DataType,
                   out: Phrase[AccType],
                   f: Phrase[AccType -> (ExpType -> CommandType)],
                   in: Phrase[ExpType])
  extends AbstractMapI(n, dt1, dt2, out, f, in) {

  override def makeMapI = MapSeqI

  override def substituteImpl(env: SubstituteImplementations.Environment): Phrase[CommandType] = {
    `for`(n, i => {
      SubstituteImplementations( f(out `@` i)(in `@` i), env )
    })
  }

}
