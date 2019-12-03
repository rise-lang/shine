package elevate.core.strategies

import elevate.core._
import elevate.core.strategies.traversal.oncetd

object basic {

  // Naive Strategies

  case class id[P]() extends Strategy[P] {
    def apply(p: P) = Success(p)
    override def toString: String = "id"
  }

  case class fail[P]() extends Strategy[P] {
    def apply(p: P) = Failure(fail())
    override def toString: String = "fail"
  }

  // Basic Strategy Combinators

  case class seq[P](f: Strategy[P], s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = f(p).flatMapSuccess(s)
    override def toString: String = s"$f `;` $s"
  }

  case class leftChoice[P](f: Strategy[P], s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = f(p).flatMapFailure(_ => s(p))
    override def toString: String = s"$f <+ $s"
  }

  // Basic Strategies

  case class `try`[P](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = (s <+ id())(p)
    override def toString: String = s"try($s)"
  }

  case class repeat[P](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = `try`(s `;` repeat(s))(p)
    override def toString: String = s"repeat($s)"
  }

  case class repeatNTimes[P](n: Int, s: Strategy[P]) extends Strategy[P] {
    def apply(p :P): RewriteResult[P] = if (n > 0) {(s `;` repeatNTimes(n - 1, s))(p)} else { id()(p) }
    override def toString: String = "repeat" + n + s"times($s)"
  }

  // Normalize

  def normalize[P: Traversable]: Strategy[P] => Strategy[P] =
    s => repeat(oncetd(s))

  // Strategy Factories

  def applyNTimes[P]: Int => (Strategy[P] => Strategy[P]) => Strategy[P] => Strategy[P] =
    i => f => s => if(i <= 0) s else applyNTimes[P](i-1)(f)(f(s))

  // Debug Strategies

  case class peek[P](f: P => Unit) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = { f(p); Success(p) }
    override def toString: String = s"peek(f)"
  }

  case class debug[P](msg: String) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = peek[P](x => println(msg + "\n" + x))(p)
    override def toString: String = "debug"
  }

  case class println[P](msg: String) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = peek[P](_ => println(msg))(e)
    override def toString: String = "println"
  }
}
