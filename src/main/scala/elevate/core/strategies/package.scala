package elevate.core

import elevate.core.rules.algorithmic._
import elevate.core.strategies.algorithmic._
import elevate.core.strategies.normalforms._
import elevate.core.strategies.liftTraversal._
import strategies.traversal._
import scala.language.implicitConversions


package object strategies {

  def print: Strategy = print("")
  def print(msg: String): Strategy = {
    e => println(s"$msg $e"); Success(e)
  }

  def wrap: Int => (Strategy => Strategy) => Strategy => Strategy =
    i => wrapper => s => if(i <= 0) s else wrap(i-1)(wrapper)(wrapper(s))

  def fmap: Strategy => Strategy =
    s =>
      mapFusion `;` reductionNormalform `;`
      //wrap(3)(one(_))(s) `;` reductionNormalform `;`
      function(argument(body(s))) `;` reductionNormalform `;`
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
