package rise.elevate.rules

import elevate.core._
import rise.elevate.Rise
import rise.core._
import rise.core.types._

// implementing elevate.core.strategies.Traversable for Rise

object traversal {
  // Rise-specific Traversals

  case class body(s: Strategy[Rise]) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case Lambda(x, f) => s(f).mapSuccess(Lambda(x, _)(e.t))
      case DepLambda(x: NatIdentifier, f) =>
        s(f).mapSuccess(DepLambda[NatKind](x, _)(e.t))
      case DepLambda(x: DataTypeIdentifier, f) =>
        s(f).mapSuccess(DepLambda[DataKind](x, _)(e.t))
      case DepLambda(x: AddressSpaceIdentifier, f) =>
        s(f).mapSuccess(DepLambda[AddressSpaceKind](x, _)(e.t))
      case DepLambda(x: NatToNatIdentifier, f) =>
        s(f).mapSuccess(DepLambda[NatToNatKind](x, _)(e.t))
      case DepLambda(x: NatToDataIdentifier, f) =>
        s(f).mapSuccess(DepLambda[NatToDataKind](x, _)(e.t))
      case _ => Failure(s)
    }
    override def toString = s"body($s)"
  }

  case class lambdaBodyWithName(
    s: Identifier => Strategy[Rise]
  ) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case Lambda(x, f) => s(x)(f).mapSuccess(Lambda(x, _)(e.t))
      case _ => Failure(lambdaBodyWithName(s))
    }
    override def toString = s"body($s)"
  }

  case class function(s: Strategy[Rise]) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case ap @ App(f, x) => s(f).mapSuccess(App(_, x)(ap.t))
      case _ => Failure(s)
    }
    override def toString = s"function($s)"
  }

  case class argument(s: Strategy[Rise]) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case ap @ App(f, x) => s(x).mapSuccess(App(f, _)(ap.t))
      case _ => Failure(s)
    }
    override def toString = s"argument($s)"
  }

  case class argumentOf(p: Primitive, s: Strategy[Rise])
    extends Strategy[Rise] {

    def apply(e: Rise): RewriteResult[Rise] = e match {
      case ap @ App(f, x) if f == p => s(x).mapSuccess(App(f, _)(ap.t))
      case _ => Failure(s)
    }
    override def toString = s"argumentOf($p,$s)"
  }

  // Default traversal order
  object default {
    implicit object RiseTraversable extends implementation.DefaultTraversal {
      override protected def oneHandlingState: Boolean => Strategy[Rise] => Strategy[Rise] =
        carryOverState => s => {
          // (option 1) traverse to argument first
          case a @ App(f, e) => s(e) match {
            case Success(x: Rise) => Success(App(f, x)(a.t))
            case Failure(state)   => if (carryOverState)
              state(f).mapSuccess(App(_, e)(a.t)) else
              s(f).mapSuccess(App(_, e)(a.t))
          }

          // Push s further down the AST.
          // If there are no subexpressions (None),
          // we failed to apply s once => Failure
          case x => traverseSingleSubexpression(s)(x) match {
            case Some(r) => r
            case None    => Failure(s)
          }
        }
    }
  }

  object alternative {
    implicit object RiseTraversable extends implementation.DefaultTraversal  {
      override protected def oneHandlingState: Boolean => Strategy[Rise] => Strategy[Rise] =
        carryOverState => s => {
          // (option 2) traverse to function first
          case a @ App(f, e) => s(f) match {
            case Success(x: Rise) => Success(App(x, e)(a.t))
            case Failure(state)   => if (carryOverState)
              state(e).mapSuccess(App(f, _)(a.t)) else
                  s(e).mapSuccess(App(f, _)(a.t))
          }

          // Push s further down the AST.
          // If there are no subexpressions (None),
          // we failed to apply s once => Failure
          case x => traverseSingleSubexpression(s)(x) match {
            case Some(r) => r
            case None    => Failure(s)
          }
        }
    }
  }

  object implementation {
    // For Rise, the only AST node that contains multiple subexpressions is App!
    abstract class DefaultTraversal
      extends elevate.core.strategies.Traversable[Rise] {

      override def all: Strategy[Rise] => Strategy[Rise] = s => {
        case ap @ App(f, e) =>
          s(f).flatMapSuccess(a => s(e).mapSuccess(b => App(a, b)(ap.t)))

        // Push s further down the AST.
        // If there are no subexpressions (None), we're done
        case x => traverseSingleSubexpression(s)(x) match {
          case Some(r) => r
          case None => Success(x)
        }
      }

      protected def oneHandlingState: Boolean => Strategy[Rise] => Strategy[Rise]

      override def one: Strategy[Rise] => Strategy[Rise] =
        oneHandlingState(false)
      override def oneUsingState: Strategy[Rise] => Strategy[Rise] =
        oneHandlingState(true)

      override def some: Strategy[Rise] => Strategy[Rise] = s => {
        case a @ App(f, e) => (s(f), s(e)) match {
          case (Failure(_), Failure(_)) => Failure(s)
          case (x, y) =>
            Success(App(x.getProgramOrElse(f), y.getProgramOrElse(e))(a.t))
        }

        // ...same here (see oneHandlingState)
        case x => traverseSingleSubexpression(s)(x) match {
          case Some(r) => r
          case None => Failure(s)
        }
      }

      // Handle all Rise-AST nodes that contain one
      // or no subexpressions (all except App)
      type TraversalType = Strategy[Rise] => Rise => Option[RewriteResult[Rise]]
      protected def traverseSingleSubexpression: TraversalType =
        s => {
          case App(_,_) => throw new Exception("this should not happen")
          case Identifier(_) => None
          case l @ Lambda(x, e) => Some(s(e).mapSuccess(Lambda(x, _)(l.t)))
          case dl @ DepLambda(x, e) => x match {
            case n: NatIdentifier =>
              Some(s(e).mapSuccess(DepLambda[NatKind](n, _)(dl.t)))
            case ns: NatCollectionIdentifier =>
              Some(s(e).mapSuccess(DepLambda[NatCollectionKind](ns, _)(dl.t)))
            case dt: DataTypeIdentifier =>
              Some(s(e).mapSuccess(DepLambda[DataKind](dt, _)(dl.t)))
            case addr: AddressSpaceIdentifier =>
              Some(s(e).mapSuccess(DepLambda[AddressSpaceKind](addr, _)(dl.t)))
          }
          case da @ DepApp(f, x)=> x match {
            case n: Nat => Some(s(f).mapSuccess(DepApp[NatKind](_, n)(da.t)))
            case dt: DataType =>
              Some(s(f).mapSuccess(DepApp[DataKind](_, dt)(da.t)))
            case addr: AddressSpace =>
              Some(s(f).mapSuccess(DepApp[AddressSpaceKind](_, addr)(da.t)))
          }
          case Literal(_) => None
          case _: ForeignFunction => None
          case _: Primitive => None
        }
    }
  }
}
