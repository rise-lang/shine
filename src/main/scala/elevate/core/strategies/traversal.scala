package elevate.core.strategies

import elevate.core._
import elevate.core.strategies.basic._

object traversal {

  // generic one-level traversal strategies

  case class all[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = implicitly[Traversable[P]].all(s)(p)
  }

  case class one[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = implicitly[Traversable[P]].one(s)(p)
  }

  case class some[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = implicitly[Traversable[P]].some(s)(p)
  }

  case class oneUsingState[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = implicitly[Traversable[P]].oneUsingState(s)(p)
  }

  // generic complete traversal strategies

  case class oncetd[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = (s <+ one(oncetd(s))) (p)
  }

  case class topdown[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = (s `;` all(topdown(s))) (p)
  }

  case class tryAll[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = (all(tryAll(`try`(s))) `;` `try`(s)) (p)
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

  // counting traversal strategies describing absolute positions

  case class position[P: Traversable](n: Int)(s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = if (n <= 0) s(p) else oneUsingState(position(n - 1)(s)).apply(p)
  }

  case class skip[P: Traversable](n: Int)(s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = s(p) match {
      case Failure(a)           => oneUsingState(skip(n)(a)).apply(p)
      case Success(_) if n > 0  => oneUsingState(skip(n - 1)(s)).apply(p)
      case Success(r) if n <= 0 => Success(r)
    }
  }
}
