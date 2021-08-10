package shine.DPIA.Compilation.Passes

import shine.DPIA.DSL
import shine.DPIA.Phrases.{Identifier, Phrase, VisitAndRebuild}
import shine.DPIA.Types.{CommType, ExpType, PhraseType}
import rise.core.types.DataType._
import shine.DPIA.primitives.functional.NatAsIndex

object SimplifyNats {

  def simplify: Phrase[CommType] => Phrase[CommType] = p => {
    VisitAndRebuild(p, new VisitAndRebuild.Visitor {
      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
        p match {
          // don't touch identifiers that can be introduced by lambdas
          case _: Identifier[T] => Continue(p, this)
          case _ => p.t match {
            case _: ExpType => Stop(simplifyIndexAndNatExp(p.asInstanceOf[Phrase[ExpType]])
              .asInstanceOf[Phrase[T]])
            case _ => Continue(p, this)
          }
        }
      }
    })
  }

  def simplifyIndexAndNatExp(p: Phrase[ExpType]): Phrase[ExpType] = {
    p.t.dataType match {
      case IndexType(n) => NatAsIndex(n, DSL.mapTransientNat(p, x => x))
      case NatType => DSL.mapTransientNat(p, x => x)
      case _ => p
    }
  }
}
