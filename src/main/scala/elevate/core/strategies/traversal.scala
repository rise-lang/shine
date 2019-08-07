package elevate.core.strategies

import elevate.core._
import elevate.core.strategies.basic._
import lift.core.types._
import lift.core._

object traversal {

  // generic one-level traversal operators

  // applies s to all direct subexpressions
  case class all[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = implicitly[Traversable[P]].all(s)(p)
  }

  // applies s to one direct subexpression
  case class oneHandlingState[P: Traversable](carryOverState: Boolean, s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = implicitly[Traversable[P]].oneHandlingState(carryOverState)(s)(p)
  }

  object one {
    def apply[P: Traversable](s: Strategy[P]) = oneHandlingState(carryOverState = false, s)
  }

  object oneWithState {
    def apply[P: Traversable](s: Strategy[P]) = oneHandlingState(carryOverState = true, s)
  }

  // applies s to at least one direct subexpression and as many as possible
  case class some[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = implicitly[Traversable[P]].some(s)(p)
  }

  // generic traversal strategies

  case class oncetd[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = (s <+ one(oncetd(s))) (p)
    override def toString = s"oncetd($s)"
  }

  case class topdown[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = (s `;` all(topdown(s))) (p)
  }

  case class tryAll[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = (all(tryAll(`try`(s))) `;` `try`(s)) (p)
    override def toString = s"tryAll($s)"
  }

  case class bottomup[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = (all(bottomup(s)) `;` s) (p)
  }

  case class downup[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = (s `;` (all(downup(s)) `;` s)) (p)
  }

  case class downup2[P: Traversable](s1: Strategy[P], s2: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = (s1 `;` (all(downup2(s1, s2)) `;` s2)) (p)
  }

  case class oncebu[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = (one(oncebu(s)) <+ s)(p)
  }

  case class alltd[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = (s <+ all(alltd(s)))(p)
  }

  case class sometd[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = (s <+ some(sometd(s)))(p)
  }

  case class somebu[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = (some(somebu(s)) <+ s)(p)
  }

  case class position[P: Traversable](n: Int)(s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = if (n <= 0) s(p) else oneWithState(position(n - 1)(s)).apply(p)
  }

  case class skip[P: Traversable](n: Int)(s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = s(p) match {
      case Failure(a) =>
        oneWithState(skip(n)(a)).apply(p)
      case Success(r) if n <= 0 =>
        Success(r)
      case Success(_) if n > 0 =>
        oneWithState(skip(n - 1)(s)).apply(p)
    }
  }

  // todo figure out whats wrong here
  //def innermost: Strategy[Lift] => Strategy[Lift] = s => bottomup(`try`(e => (s `;` innermost(s))(e))) }
}
