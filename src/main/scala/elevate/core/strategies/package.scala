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

  def fmap: Strategy => Strategy =
    s =>
      mapFusion `;` reductionNormalForm `;`
      //wrap(3)(one(_))(s) `;` reductionNormalform `;`
      function(argument(body(s))) `;` reductionNormalForm `;`
      one(mapFullFission)

  // example rule used for s: **f >> T -> T >> **f
  // ((map transpose) ((map (map (map e12))) e13))       // input to fmap
  // ((map λe14. (transpose ((map (map e12)) e14))) e13) // result of mapFusion + reductionNormalform
  //  (map λe14. (transpose ((map (map e12)) e14)))      // result of `function`
  //       λe14. (transpose ((map (map e12)) e14))       // result of `argument`
  //             (transpose ((map (map e12)) e14))       // result of 'body' -> here we can apply s

  def mapped: Strategy => Strategy =
    s => s <+ (e => fmap(mapped(s))(e))

}
