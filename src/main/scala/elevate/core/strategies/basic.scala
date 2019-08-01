package elevate.core.strategies

import elevate.core._
import lift.core.{Expr, Program}

object basic {

  case class id[T <: Program]() extends StrategyT[T] {
    def apply(e: T) = Success(e)
  }

  case class seq[T <: Program](f: StrategyT[T], s: StrategyT[T]) extends StrategyT[T] {
    def apply(e: T): RewriteResult[T] = f(e).flatMapSuccess({ case x:T => s(x)})
  }

  case class leftChoice[T <: Program](f: StrategyT[T], s: StrategyT[T]) extends StrategyT[T] {
    def apply(e: T): RewriteResult[T] = f(e).flatMapFailure(_ => s(e))
  }

  case class `try`[T <: Program](s: StrategyT[T]) extends StrategyT[T] {
    def apply(e: T): RewriteResult[T] = leftChoice[T](s, id())(e)
  }

  case class peek[T <: Program](f: T => Unit) extends StrategyT[T] {
    def apply(e: T): RewriteResult[T] = {f(e); Success(e)}
  }

  case class repeat[T <: Program](s: StrategyT[T]) extends StrategyT[T] {
    def apply(e: T): RewriteResult[T] = `try`[T](s `;` repeat[T](s))(e)
  }

  /*
  def countingRepeat: (Int => Strategy) => Int => Strategy =
    s => i => `try`(s(i) `;` (e => countingRepeat(s)(i+1)(e)))

  def repeatNTimes: Int => Strategy => Strategy =
    n => s => if (n > 0) { s `;` repeatNTimes(n-1)(s) } else { id }
   */

  case class debug[T <: Program](msg: String) extends StrategyT[T] {
    def apply(e: T): RewriteResult[T] = peek[T](p => println(s"$msg $p"))(e)
  }

  object debug {
    def apply[T <: Program](e: T): RewriteResult[T] = debug("")(e)
  }

  case class debugln[T <: Program](msg: String) extends StrategyT[T] {
    def apply(e: T): RewriteResult[T] = debug[T](msg + "\n")(e)
  }

  case class print[T <: Program](msg: String) extends StrategyT[T] {
    def apply(e: T): RewriteResult[T] = peek[T](e => println(msg))(e)
  }

  case class applyNTimes[T <: Program](i: Int, f: (StrategyT[T] => StrategyT[T]), s: StrategyT[T]) extends StrategyT[T] {
    def apply(e: T): RewriteResult[T] = if(i <= 0) s(e) else applyNTimes[T](i-1,f,f(s))(e)
  }
}
