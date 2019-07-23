package elevate.core.strategies

import elevate.core.{Failure, Strategy, Success}
import elevate.lift.strategies.traversal._
import lift.core.Expr


object basic {

  def id: Strategy =
    e => Success(e)

  def seq: Strategy => Strategy => Strategy =
    f => s => e => f(e).flatMapSuccess(s(_))

  def leftChoice: Strategy => Strategy => Strategy =
    f => s => e => f(e).flatMapFailure(_ => s(e))

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

  def debug: Strategy = debug("")
  def debug(msg: String): Strategy = peek(e => println(s"$msg $e"))
  def debugln(msg: String): Strategy = debug(msg + "\n")

  def print(msg: String): Strategy = peek(e => println(msg))

  def applyNTimes: Int => (Strategy => Strategy) => Strategy => Strategy =
    i => f => s => if(i <= 0) s else applyNTimes(i-1)(f)(f(s))


}
