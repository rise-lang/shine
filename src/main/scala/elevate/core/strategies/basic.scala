package elevate.core.strategies

import elevate.core._
import lift.core.Program

object basic {

  case class id[T <: Program]() extends Strategy[T] {
    def apply(e: T) = Success(e)
  }

  case class seq[T <: Program](f: Strategy[T], s: Strategy[T]) extends Strategy[T] {
    def apply(e: T): RewriteResult[T] = f(e).flatMapSuccess({ case x:T => s(x)})
  }

  case class leftChoice[T <: Program](f: Strategy[T], s: Strategy[T]) extends Strategy[T] {
    def apply(e: T): RewriteResult[T] = f(e).flatMapFailure(_ => s(e))
  }

  case class `try`[T <: Program](s: Strategy[T]) extends Strategy[T] {
    def apply(e: T): RewriteResult[T] = leftChoice[T](s, id())(e)
  }

  def try2[T <: Program]: Strategy[T] => Strategy[T] = s => leftChoice[T](s, id())

  case class peek[T <: Program](f: T => Unit) extends Strategy[T] {
    def apply(e: T): RewriteResult[T] = {f(e); Success(e)}
  }

  case class repeat[T <: Program](s: Strategy[T]) extends Strategy[T] {
    def apply(e: T): RewriteResult[T] = `try`[T](s `;` repeat[T](s))(e)
  }

  case class countingRepeat[T <: Program](s: Int => Strategy[T], i: Int) extends Strategy[T] {
    def apply(e: T): RewriteResult[T] = (`try`(s(i) `;` countingRepeat(s, i + 1))) (e)
  }

  case class repeatNTimes[T <: Program](n: Int, s: Strategy[T]) extends Strategy[T] {
    def apply(e :T): RewriteResult[T] = if (n > 0) {(s `;` repeatNTimes(n - 1, s))(e)} else { id()(e) }
  }

  case class debug[T <: Program](msg: String) extends Strategy[T] {
    def apply(e: T): RewriteResult[T] = peek[T](p => println(s"$msg $p"))(e)
  }

  case object debug { def apply[T <: Program](e: T): RewriteResult[T] = debug("")(e) }

  case class debugln[T <: Program](msg: String) extends Strategy[T] {
    def apply(e: T): RewriteResult[T] = debug[T](msg + "\n")(e)
  }

  case class print[T <: Program](msg: String) extends Strategy[T] {
    def apply(e: T): RewriteResult[T] = peek[T](_ => println(msg))(e)
  }

  case class applyNTimes[T <: Program](i: Int, f: (Strategy[T] => Strategy[T]), s: Strategy[T]) extends Strategy[T] {
    def apply(e: T): RewriteResult[T] = if(i <= 0) s(e) else applyNTimes[T](i-1,f,f(s))(e)
  }
}
