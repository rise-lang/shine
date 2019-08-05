package elevate.core.strategies

import elevate.core._

object basic {

  case class id[P]() extends Strategy[P] {
    def apply(e: P) = Success(e)
  }

  case class seq[P](f: Strategy[P], s: Strategy[P]) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = f(e).flatMapSuccess(s)
  }

  case class leftChoice[P](f: Strategy[P], s: Strategy[P]) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = f(e).flatMapFailure(_ => s(e))
  }

  case class `try`[P](s: Strategy[P]) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = leftChoice[P](s, id())(e)
  }

  def try2[P]: Strategy[P] => Strategy[P] = s => leftChoice[P](s, id())

  case class peek[P](f: P => Unit) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = {f(e); Success(e)}
  }

  case class repeat[P](s: Strategy[P]) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = `try`[P](s `;` repeat[P](s))(e)
  }

  case class countingRepeat[P](s: Int => Strategy[P], i: Int) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = (`try`(s(i) `;` countingRepeat(s, i + 1))) (e)
  }

  case class repeatNTimes[P](n: Int, s: Strategy[P]) extends Strategy[P] {
    def apply(e :P): RewriteResult[P] = if (n > 0) {(s `;` repeatNTimes(n - 1, s))(e)} else { id()(e) }
  }

  case class debug[P](msg: String) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = peek[P](p => println(s"$msg $p"))(e)
  }

  case object debug { def apply[P](e: P): RewriteResult[P] = debug("")(e) }

  case class debugln[P](msg: String) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = debug[P](msg + "\n")(e)
  }

  case class print[P](msg: String) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = peek[P](_ => println(msg))(e)
  }

  case class applyNTimes[P](i: Int, f: (Strategy[P] => Strategy[P]), s: Strategy[P]) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = if(i <= 0) s(e) else applyNTimes[P](i-1,f,f(s))(e)
  }
}
