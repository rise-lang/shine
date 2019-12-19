package idealised.DPIA.Compilation

import idealised.DPIA._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA.ImperativePrimitives.{Comment, For, ForNat, IdxAcc, Seq}
import idealised.DPIA.FunctionalPrimitives.AsIndex
import idealised.OpenCL.ImperativePrimitives.OpenCLParFor
import rise.arithmetic.Cst
import rise.arithmetic.ArithExpr.isSmaller

object UnrollLoops {
  def apply(p: Phrase[CommType]): Phrase[CommType] = {
    val r = VisitAndRebuild(p, new VisitAndRebuild.Visitor {
      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = p match {
        case For(n, Lambda(ident: Identifier[_], body), true) =>
          Continue(unrollLoop(n, init=0, step=1, i => Phrase.substitute(AsIndex(n, Natural(i)), `for`=ident, in=body)), this)
        case ForNat(n, DepLambda(ident: NatIdentifier, body), true) =>
          Continue(unrollLoop(n, init=0, step=1, i => PhraseType.substitute(i, `for`=ident, in=body)), this)
        case OpenCLParFor(n, _, out, Lambda(ident: Identifier[_], Lambda(identOut: Identifier[_], body)), init, step, true) =>
          out.t.dataType match {
            case ArrayType(_, elemType) =>
              Continue(unrollLoop(n, init, step, i => Phrase.substitute(IdxAcc(n, elemType, AsIndex(n, Natural(i)), out),
                `for`=identOut,
                Phrase.substitute(AsIndex(n, Natural(i)), `for`=ident, in=body))), this)
            case _ => throw new Exception("OpenCLParFor acceptor has to be of ArrayType.")
          }
        case _ =>
          Continue(p, this)
      }
    })
    r
  }

  def unrollLoop(n: Nat, init: Nat, step: Nat,
                 genBody: Nat => Phrase[CommType]): Phrase[CommType] = {
    import rise.arithmetic.NotEvaluableException

    val stopMax = try {
      n.max.eval
    } catch {
      case _: NotEvaluableException => throw new Exception(s"cannot evaluate ${n.max} during loop unrolling")
    }

    val startMin = try {
      init.min.eval
    } catch {
      case _: NotEvaluableException => throw new Exception(s"cannot evaluate ${init.min} during loop unrolling")
    }

    val incr = try {
      step.eval
    } catch {
      case _: NotEvaluableException => throw new Exception(s"cannot evaluate $step during loop unrolling")
    }

    val numIter = ceilDiv(stopMax - startMin, incr)

    val tmp = (0 until numIter).foldLeft[Phrase[CommType]](Comment(s"unrolling loop of $numIter"))({ case (prev, i) =>
      val index = init + Cst(i * incr)
      assert(isSmaller(index, n).contains(true)) //TODO add if-guards otherwise.
      //TODO store result of init in temporary variable
      Seq(prev, genBody(index))
    })

    tmp
  }

  def ceilDiv(a: Int, b: Int) : Int = {
    (a + b - 1)/ b
  }
}
