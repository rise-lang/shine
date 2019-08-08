package elevate.core.strategies

import elevate.core._
import elevate.core.strategies.traversal.oncetd
import elevate.lift.strategies.traversal._
import elevate.meta.strategies.traversal._

object basic {

  case class id[P]() extends Strategy[P] {
    def apply(e: P) = Success(e)
    override def toString = s"id"
  }

  case class seq[P](f: Strategy[P], s: Strategy[P]) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = f(e).flatMapSuccess(s)
    override def toString = s"$f `;` $s"
    //override def toString = s"seq($f,$s)"
  }

  case class leftChoice[P](f: Strategy[P], s: Strategy[P]) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = f(e).flatMapFailure(_ => s(e))
    override def toString = s"leftChoice($f,$s)"
  }

  case class `try`[P](s: Strategy[P]) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = leftChoice[P](s, id())(e)
    override def toString = s"try($s)"
  }

  case class peek[P](f: P => Unit) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = {f(e); Success(e)}
    override def toString = s"peek(...)"
  }

  case class repeat[P](s: Strategy[P]) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = `try`(s `;` repeat(s))(e)
    override def toString = s"repeat($s)"
  }

  case class countingRepeat[P](s: Int => Strategy[P], i: Int) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = (`try`(s(i) `;` countingRepeat(s, i + 1))) (e)
    override def toString = s"countingRepeat(${s(i)}, $i)"
  }

  case class repeatNTimes[P](n: Int, s: Strategy[P]) extends Strategy[P] {
    def apply(e :P): RewriteResult[P] = if (n > 0) {(s `;` repeatNTimes(n - 1, s))(e)} else { id()(e) }
    override def toString = "repeat" + n + s"times($s)"
  }

  def show[P]: Strategy[P] = debug("")

  case class debug[P](msg: String) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = peek[P](p => println(s"$msg $p"))(e)
    override def toString = "debug(...)"
  }

  case class debugln[P](msg: String) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = debug[P](msg + "\n")(e)
    override def toString = "debugln(...)"
  }

  case class print[P](msg: String) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = peek[P](_ => println(msg))(e)
    override def toString = "print(...)"
  }

  def applyNTimes[P]: Int => (Strategy[P] => Strategy[P]) => Strategy[P] => Strategy[P] =
    i => f => s => if(i <= 0) s else applyNTimes[P](i-1)(f)(f(s))

  // todo generalize
  def normalize: Strategy[Lift] => Strategy[Lift] =
    s => repeat(oncetd(s))

  def normalizeElevate: Strategy[Elevate] => Strategy[Elevate] =
    s => repeat(oncetd(s))
}
