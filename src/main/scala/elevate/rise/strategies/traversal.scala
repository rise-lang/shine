package elevate.rise.strategies

import elevate.core._
import lift.core.DSL._
import elevate.rise.rules.algorithmic._
import elevate.core.strategies.traversal._
import elevate.core.strategies.basic._
import elevate.rise.Rise
import elevate.rise.rules.traversal._
import elevate.rise.strategies.algorithmic._
import elevate.rise.strategies.normalForm._

object traversal {

  // applying a strategy to an expression applied to a lift `map`. Example:
  // ((map λe14. (transpose ((map (map e12)) e14))) e13) // input expr
  //  (map λe14. (transpose ((map (map e12)) e14)))      // result of `function`
  //       λe14. (transpose ((map (map e12)) e14))       // result of `argument`
  //             (transpose ((map (map e12)) e14))       // result of 'body' -> here we can apply s
  def fmap: Strategy[Rise] => Strategy[Rise] = s => function(argumentOf(map, body(s)))

  // fmap applied for expressions in rewrite normal form:
  // fuse -> fmap -> fission
  def fmapRNF: Strategy[Rise] => Strategy[Rise] =
    s => LCNF `;` mapFusion `;`
         LCNF `;` fmap(s) `;`
         LCNF `;` one(mapFullFission)

  // applying a strategy to an expression nested in one or multiple lift `map`s
  def mapped: Strategy[Rise] => Strategy[Rise] =
    s => s <+ (e => fmapRNF(mapped(s))(e))

  // moves along RNF-normalized expression
  // e.g., expr == ***f o ****g o *h
  // move(0)(s) == s(***f o ****g o *h)
  // move(1)(s) == s(****g o *h)
  // move(2)(s) == s(*h)
  def moveTowardsArgument: Int => Strategy[Rise] => Strategy[Rise] =
    i => s => applyNTimes(i)((e: Strategy[Rise]) => argument(e))(s)
}
