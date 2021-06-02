package shine.DPIA.Compilation.Passes

import arithexpr.arithmetic.ArithExpr.isSmaller
import arithexpr.arithmetic.Cst
import shine.DPIA.Nat
import shine.DPIA.Phrases.{Natural, Phrase, VisitAndRebuild}
import shine.DPIA.Types.{ArrayType, CommType, PhraseType}
import shine.DPIA.primitives.functional.NatAsIndex
import shine.DPIA.primitives.imperative._
import shine.OpenCL.primitives.imperative.ParFor

object UnrollLoops {

  def unroll: Phrase[CommType] => Phrase[CommType] = p => {
    val r = VisitAndRebuild(p, new VisitAndRebuild.Visitor {
      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] =
        p match {
          case f@For(true) =>
            f.loopBody match {
              case shine.DPIA.Phrases.Lambda(x, body) =>
                Continue(unrollLoop(f.n, init = 0, step = 1, i =>
                  Phrase.substitute(NatAsIndex(f.n, Natural(i)),
                    `for` = x, in = body)), this)
              case _ => throw new Exception("This should not happen")
            }
          case f@ForNat(true) =>
            f.loopBody match {
              case shine.DPIA.Phrases.DepLambda(kind, x, body) =>
                Continue(unrollLoop(f.n, init = 0, step = 1, i =>
                  PhraseType.substitute(i, `for` = x, in = body)), this)
              case _ => throw new Exception("This should not happen")
            }
          case pf@ParFor(_, _, true, _) =>
            pf.body match {
              case shine.DPIA.Phrases.Lambda(ident, shine.DPIA.Phrases.Lambda(identOut, body)) =>
                pf.out.t.dataType match {
                  case ArrayType(_, elemType) =>
                    Continue(unrollLoop(pf.n, pf.init, pf.step, i =>
                      Phrase.substitute(
                        IdxAcc(pf.n, elemType,
                          NatAsIndex(pf.n, Natural(i)), pf.out),
                        `for` = identOut,
                        Phrase.substitute(NatAsIndex(pf.n, Natural(i)),
                          `for` = ident, in = body))), this)
                  case _ =>
                    throw new Exception("OpenCLParFor acceptor has to be of ArrayType.")
                }
              case _ => throw new Exception("This should not happen")
            }
          case _ =>
            Continue(p, this)
        }
    })
    r
  }

  private def unrollLoop(n: Nat, init: Nat, step: Nat,
                         genBody: Nat => Phrase[CommType]): Phrase[CommType] = {
    import arithexpr.arithmetic.NotEvaluableException

    val stopMax = try {
      n.max.eval
    } catch {
      case _: NotEvaluableException =>
        throw new Exception(s"cannot evaluate ${n.max} during loop unrolling")
    }

    val startMin = try {
      init.min.eval
    } catch {
      case _: NotEvaluableException =>
        throw new Exception(s"cannot evaluate ${init.min} during loop unrolling")
    }

    val incr = try {
      step.eval
    } catch {
      case _: NotEvaluableException =>
        throw new Exception(s"cannot evaluate $step during loop unrolling")
    }

    val numIter = ceilDiv(stopMax - startMin, incr)

    val tmp = (0 until numIter).foldLeft[Phrase[CommType]](
      shine.DPIA.DSL.comment(s"unrolling loop of $numIter"))({
      case (prev, i) =>
        val index = init + Cst(i * incr)
        assert(isSmaller(index, n).contains(true)) //TODO add if-guards otherwise.
        //TODO store result of init in temporary variable
        Seq(prev, genBody(index))
    })

    tmp
  }

  private def ceilDiv(a: Int, b: Int): Int = {
    (a + b - 1) / b
  }
}
