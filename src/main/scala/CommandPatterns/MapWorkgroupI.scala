package CommandPatterns

import Core.PhraseType._
import Core._
import DSL._
import Rewriting.SubstituteImplementations

case class MapWorkgroupI(out: Phrase[AccType],
                         f: Phrase[AccType -> (ExpType -> CommandType)],
                         in: Phrase[ExpType])
  extends AbstractMapI(out, f, in) {

  override def makeMapI = MapWorkgroupI

  override def substituteImpl: Phrase[CommandType] = {
    val l = length(in)
    TypeChecker(l)

    val elemT = out.t match { case AccType(ArrayType(_, dt)) => dt }
    ParForWorkgroup(l, out, λ( ExpType(int) ) { i => λ( AccType(elemT) ) { o =>
      SubstituteImplementations( f(o)(in `@` i) )
    } })
  }

}
