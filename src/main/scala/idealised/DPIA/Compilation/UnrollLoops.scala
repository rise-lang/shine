package idealised.DPIA.Compilation

import idealised.DPIA._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA.ImperativePrimitives.{Comment, For, ForNat, Seq}
import idealised.DPIA.FunctionalPrimitives.AsIndex

object UnrollLoops {
  def apply(p: Phrase[CommType]): Phrase[CommType] = {
    val r = VisitAndRebuild(p, new VisitAndRebuild.Visitor {
      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = p match {
        case For(n, Lambda(ident: Identifier[_], body), true) =>
          Continue(unrollLoop(n, i => Phrase.substitute(AsIndex(n, Natural(i)), `for`=ident, in=body)), this)
        case ForNat(n, DepLambda(ident: NatIdentifier, body), true) =>
          Continue(unrollLoop(n, i => PhraseType.substitute(lift.arithmetic.Cst(i), `for`=ident, in=body)), this)
        case _ =>
          Continue(p, this)
      }
    })
    r
  }

  def unrollLoop(n: Nat, genBody: Int => Phrase[CommType]): Phrase[CommType] = {
    import lift.arithmetic.NotEvaluableException

    val m = try {
      n.eval
    } catch {
      case _: NotEvaluableException => throw new Exception(s"cannot evaluate $n during loop unrolling")
    }

    (0 until m).foldLeft[Phrase[CommType]](Comment(s"unrolling loop of $m"))({ case (prev, i) =>
      Seq(prev, genBody(i))
    })
  }
}
