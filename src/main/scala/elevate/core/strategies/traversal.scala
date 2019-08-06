package elevate.core.strategies

import elevate.core._
import elevate.core.strategies.basic._
import lift.core.types._
import lift.core._

object traversal {

  // generic one-level traversal operators

  // applies s to all direct subexpressions
  case class all[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = implicitly[Traversable[P]].all(s)(e)
  }

  // applies s to one direct subexpression
  case class oneHandlingState[P: Traversable](carryOverState: Boolean, s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = implicitly[Traversable[P]].oneHandlingState(carryOverState)(s)(p)
  }

//  case class oneHandlingState(carryOverState: Boolean, s: Strategy[Lift]) extends Strategy[Lift] {
//    def apply(e: Lift): RewriteResult[Lift] = e match {
//      case Apply(f, e) => s(f) match {
//        case Success(x: Lift) => Success(Apply(x, e))
//        case Failure(state) => if (carryOverState)
//          state(e).mapSuccess(Apply(f, _)) else
//          s(e).mapSuccess(Apply(f, _))
//      }
//      case x => elevate.lift.strategies.traversal.traverseSingleSubexpression(s)(x) match {
//        case Some(r) => r
//        case None => Failure(s)
//      }
//    }
//  }

  object one {
    def apply[P: Traversable](s: Strategy[P]) = oneHandlingState(carryOverState = false, s)
  }

  object oneWithState {
    def apply[P: Traversable](s: Strategy[P]) = oneHandlingState(carryOverState = true, s)
  }

  // applies s to at least one direct subexpression and as many as possible
  case class some(s: Strategy[Lift]) extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case Apply(f, e) => (s(f), s(e)) match {
        case (Failure(_), Failure(_)) => Failure(s)
        case (x, y) => Success(Apply(x.getProgramOrElse(f), y.getProgramOrElse(e)))
      }
      case x => elevate.lift.strategies.traversal.traverseSingleSubexpression(s)(x) match {
        case Some(r) => r
        case None => Failure(s)
      }
    }
  }


  // generic traversal strategies

  case class oncetd[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = (s <+ one(oncetd(s))) (p)
  }

  case class topdown[P: Traversable](s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = (s `;` all(topdown(s))) (p)
  }

  //def tryAll2: Strategy[Lift] => Strategy[Lift] = s => ((e: Lift) => all(tryAll(`try`(s))).apply(e)) `;` `try`(s)
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

  /*
  def oncebu: Strategy[Lift] => Strategy[Lift] = s => ((e: Lift) => one(oncebu(s))(e)) <+ s

  def alltd: Strategy[Lift] => Strategy[Lift] = s => s <+ (e => all(alltd(s)).apply(e))


  def sometd: Strategy[Lift] => Strategy[Lift] = s => s <+ (e => some(sometd(s))(e))

  def somebu: Strategy[Lift] => Strategy[Lift] = s => ((e: Lift) => some(somebu(s))(e)) <+ s

   */
  case class position[P: Traversable](n: Int)(s: Strategy[P]) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = if (n <= 0) s(p) else oneWithState(position(n - 1)(s)).apply(p)
  }

  /*
  def skip(n: Int): Strategy[Lift] => Strategy[Lift] = s => e => s(e) match {
    case Failure(a) =>
      oneWithState(skip(n)(a))(e)
    case Success(r) if n <= 0 =>
      Success(r)
    case Success(_) if n > 0 =>
      oneWithState(skip(n - 1)(s))(e)
  }

   */
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
