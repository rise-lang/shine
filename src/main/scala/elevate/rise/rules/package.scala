package elevate.rise

import elevate.core.strategies.predicate._
import elevate.rise.rules.traversal._
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import lift.core._
import lift.core.types._
import lift.core.DSL._

package object rules {

  case object betaReduction extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(f, x) => lifting.liftFunExpr(f) match {
        case lifting.Reducing(lf) => Success(lf(x))
        case _                    => Failure(betaReduction)
      }
      case DepApp(f, x: Nat) => lifting.liftDepFunExpr[NatKind](f) match {
        case lifting.Reducing(lf) => Success(lf(x))
        case _                    => Failure(betaReduction)
      }
      case _                      => Failure(betaReduction)
    }
    override def toString = "betaReduction"
  }

  case object etaReduction extends Strategy[Rise]  {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case Lambda(x1, App(f, x2)) if x1 == x2 && !contains[Rise](x1).apply(f) => Success(f)
      case _                                                                  => Failure(etaReduction)
    }
    override def toString = "etaReduction"
  }

  case object etaAbstraction extends Strategy[Rise] {
    // todo check that f is funtype
    def apply(f: Rise): RewriteResult[Rise] = {//f.t match {
      //case FunType(_,_) =>
        val x = identifier(freshName("η"))
        Success(lambda(x, DSL.app(f, x)))
      //case _ => Failure(etaAbstraction)
    }
    override def toString = "etaAbstraction"
  }

  // todo: remove once all rules are type-preserving
  case object inferRise extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = Success(infer(e))
  }
}
