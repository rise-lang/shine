package elevate

import _root_.lift.core._
import elevate.core.strategies.basic._

package object core {

  type Strategy[P] = P => RewriteResult[P]

  type Meta = Strategy[Elevate] // Meta = Strategies for Elevate
  type Elevate = Strategy[Lift] // Elevate = Strategies for Lift
  type Lift = Expr

  sealed trait RewriteResult[P] {
    def getProgramOrElse(e: P): P
    def get: P

    def mapSuccess(f: P => P): RewriteResult[P]
    def flatMapSuccess(f: P => RewriteResult[P]): RewriteResult[P]

    def mapFailure(f: Strategy[P] => Strategy[P]): RewriteResult[P]
    def flatMapFailure(f: Strategy[P] => RewriteResult[P]): RewriteResult[P]
  }

  case class Success[P](p: P) extends RewriteResult[P] {
    override def getProgramOrElse(u: P): P = p
    override def get: P = p

    override def mapSuccess(f: P => P): RewriteResult[P] = Success(f(p))
    override def flatMapSuccess(f: P => RewriteResult[P]): RewriteResult[P] = f(p)

    override def mapFailure(f: Strategy[P] => Strategy[P]): RewriteResult[P] = this
    override def flatMapFailure(f: Strategy[P] => RewriteResult[P]): RewriteResult[P] = this
  }

  case class Failure[P](s: Strategy[P]) extends RewriteResult[P] {
    override def getProgramOrElse(p: P): P = p
    override def get: P = throw NotApplicable(s)

    override def mapSuccess(f: P => P): RewriteResult[P] = this
    override def flatMapSuccess(f: P => RewriteResult[P]): RewriteResult[P] = this

    override def mapFailure(f: Strategy[P] => Strategy[P]): RewriteResult[P] = Failure(f(s))
    override def flatMapFailure(f: Strategy[P] => RewriteResult[P]): RewriteResult[P] = f(s)
  }

  case class NotApplicable[P](s: Strategy[P]) extends Exception

  implicit class Then[P](f: Strategy[P]) {
    def `;`(s: Strategy[P]): Strategy[P] = seq[P](f,s)
  }

  implicit class LeftChoice[P](f: Strategy[P]) {
    def <+(s: Strategy[P]): Strategy[P] = leftChoice(f,s)
  }
}
