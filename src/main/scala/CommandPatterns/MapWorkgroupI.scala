package CommandPatterns

import Core.PhraseType._
import Core._
import DSL._
import Compiling.SubstituteImplementations
import apart.arithmetic.ArithExpr

case class MapWorkgroupI(n: ArithExpr,
                         dt1: DataType,
                         dt2: DataType,
                         out: Phrase[AccType],
                         f: Phrase[AccType -> (ExpType -> CommandType)],
                         in: Phrase[ExpType])
  extends AbstractMapI(n, dt1, dt2, out, f, in) {

  override def makeMapI = MapWorkgroupI

  override def substituteImpl: Phrase[CommandType] = {

    val elemT = out.t match { case AccType(ArrayType(_, dt)) => dt }
    ParForWorkgroup(n, out, λ( ExpType(int) ) { i => λ( AccType(elemT) ) { o =>
      SubstituteImplementations( f(o)(in `@` i) )
    } })
  }

}
