package elevate.core

import elevate.core.rules.algorithmic._
import elevate.core.strategies.algorithmic._
import elevate.core.strategies.basic._
import elevate.core.strategies.normalForm._
import elevate.core.strategies.liftTraversal._
import strategies.traversal._
import scala.language.implicitConversions


package object strategies {

  def print: Strategy = print("")
  def print(msg: String): Strategy = peek(e => println(s"$msg $e"))

  def wrap: Int => (Strategy => Strategy) => Strategy => Strategy =
    i => wrapper => s => if(i <= 0) s else wrap(i-1)(wrapper)(wrapper(s))

  // applying a strategy to an expression applied to a lift `map`. Example:
  // ((map λe14. (transpose ((map (map e12)) e14))) e13) // input expr
  //  (map λe14. (transpose ((map (map e12)) e14)))      // result of `function`
  //       λe14. (transpose ((map (map e12)) e14))       // result of `argument`
  //             (transpose ((map (map e12)) e14))       // result of 'body' -> here we can apply s
  def fmap: Strategy => Strategy = s => function(argument(body(s)))

  // fmap applied for expressions in rewrite normal form:
  // fuse -> fmap -> fission
  def fmapRNF: Strategy => Strategy =
    s =>
      mapFusion `;` reductionNormalForm `;`
      fmap(s) `;` reductionNormalForm `;`
      one(mapFullFission)

  // applying a strategy to an expression nested in one or multiple lift `map`s
  def mapped: Strategy => Strategy =
    s => s <+ (e => fmapRNF(mapped(s))(e))

}
