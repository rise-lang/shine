package elevate.core

import lift.core.Expr
import elevate.core.rules.algorithmic._
import elevate.core.strategies.algorithmic._
import elevate.core.rules._
import strategies.traversal._
import scala.language.implicitConversions


package object strategies {
  def id: Strategy =
    e => Success(e)

  def seq: Strategy => Strategy => Strategy =
    f => s => e => f(e) match {
      case Success(x) => s(x)
      case f:Failure => f
    }

  def leftChoice: Strategy => Strategy => Strategy =
    f => s => e => {
      f(e) match {
        case s:Success => s
        case Failure(_) => s(e)
      }
    }

  def `try`: Strategy => Strategy =
    s => leftChoice(s)(id)

  def peek(f: Expr => Unit): Strategy =
    e => { f(e); Success(e) }

  def repeat: Strategy => Strategy =
    s => `try`(s `;` (e => repeat(s)(e)))

  def countingRepeat: (Int => Strategy) => Int => Strategy =
    s => i => `try`(s(i) `;` (e => countingRepeat(s)(i+1)(e)))

  def repeatNTimes: Int => Strategy => Strategy =
    n => s => if (n > 0) { s `;` repeatNTimes(n-1)(s) } else { id }

  def normalize: Strategy => Strategy =
    s => repeat(oncetd(s))

  def print: Strategy = print("")
  def print(msg: String): Strategy = {
    e => println(s"$msg $e"); Success(e)
  }

  def wrap: Int => (Strategy => Strategy) => Strategy => Strategy =
    i => wrapper => s => if(i <= 0) s else wrap(i-1)(wrapper)(wrapper(s))

  // normalforms
  def reductionNormalform: Strategy = normalize(betaReduction <+ etaReduction)

  def fmap: Strategy => Strategy =
    s =>
      mapFusion `;` reductionNormalform `;`
      wrap(3)(one(_))(s) `;` reductionNormalform `;`
      wrap(1)(one(_))(mapFullFission)
}
