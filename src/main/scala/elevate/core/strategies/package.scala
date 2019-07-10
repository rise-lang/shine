package elevate.core

import lift.core.Expr

import strategies.traversal._

package object strategies {
  def id: Strategy =
    e => e

  def seq: Strategy => Strategy => Strategy =
    f => s => e => s(f(e))

  def leftChoice: Strategy => Strategy => Strategy =
    f => s => e => {
      mayApply(f)(e) match {
        case Success(r) => r
        case Failure(_) => s(e)
      }
    }

  def `try`: Strategy => Strategy =
    s => leftChoice(s)(id)

  def peek(f: Expr => Unit): Strategy =
    e => { f(e); e }

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
    e => println(s"$msg $e"); e
  }
}
