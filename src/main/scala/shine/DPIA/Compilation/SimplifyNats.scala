package shine.DPIA.Compilation

import shine.DPIA.FunctionalPrimitives.NatAsIndex
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

object SimplifyNats {
  def apply(p: Phrase[CommType]): Phrase[CommType] = {
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
