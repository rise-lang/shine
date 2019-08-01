package elevate.lift.strategies

import elevate.core.{Failure, RewriteResult, Strategy, StrategyT, Success}
import lift.core.{Apply, DepLambda, Expr, Lambda, Primitive}
import lift.core.primitives.map
import elevate.lift.rules.algorithmic._
import elevate.core.strategies.traversal._
import elevate.core.strategies.basic._
import elevate.lift.strategies.algorithmic._
import elevate.lift.strategies.normalForm._

object traversal {

  case class body(s: StrategyT[Expr]) extends StrategyT[Expr] {
    def apply(e: Expr): RewriteResult[Expr] = e match {
      case Lambda(x, f) => s(f).mapSuccess({ case y: Expr => Lambda(x, y) })
      case _ => Failure(s)
    }
  }

  case class function(s: StrategyT[Expr]) extends StrategyT[Expr] {
    def apply(e: Expr): RewriteResult[Expr] = e match {
      case Apply(f, e) => s(f).mapSuccess({ case y: Expr => Apply(y, e) })
      case _ => Failure(s)
    }
  }

  case class inBody(s: StrategyT[StrategyT[Expr]]) extends StrategyT[StrategyT[Expr]] {
    def apply(e: StrategyT[Expr]): RewriteResult[StrategyT[Expr]] = e match {
      case body(x: StrategyT[Expr]) => s(x).mapSuccess({case y: StrategyT[Expr] => body(y)})
      case _ => Failure(s)
    }
  }

  /*
  def argument: Strategy => Strategy =
    s => {
      case Apply(f, e) => s(e).mapSuccess({case y: Expr => Apply(f, y)})
      case _ => Failure(s)
    }

  def argumentOf(x: Primitive): Strategy => Strategy = {
    s => {
      case Apply(f, e) if f == x => s(e).mapSuccess({case y:Expr => Apply(f, y)})
      case _ => Failure(s)
    }
  }
   */

  /*
  // applying a strategy to an expression applied to a lift `map`. Example:
  // ((map λe14. (transpose ((map (map e12)) e14))) e13) // input expr
  //  (map λe14. (transpose ((map (map e12)) e14)))      // result of `function`
  //       λe14. (transpose ((map (map e12)) e14))       // result of `argument`
  //             (transpose ((map (map e12)) e14))       // result of 'body' -> here we can apply s
  def fmap: Strategy => Strategy = s => function(argumentOf(map)(body(s)))

  // fmap applied for expressions in rewrite normal form:
  // fuse -> fmap -> fission
  def fmapRNF: Strategy => Strategy =
    s => LCNF `;` mapFusion `;`
      LCNF `;` fmap(s) `;`
      LCNF `;` one(mapFullFission)

  // applying a strategy to an expression nested in one or multiple lift `map`s
  def mapped: Strategy => Strategy =
    s => s <+ (e => fmapRNF(mapped(s))(e))

  // moves along RNF-normalized expression
  // e.g., expr == ***f o ****g o *h
  // move(0)(s) == s(***f o ****g o *h)
  // move(1)(s) == s(****g o *h)
  // move(2)(s) == s(*h)
  def moveTowardsArgument: Int => Strategy => Strategy =
    i => s => applyNTimes(i)(argument(_))(s)

   */
}
