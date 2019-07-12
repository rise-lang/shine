package elevate.core

import lift.core.Expr
import elevate.core.rules.algorithmic._
import elevate.core.strategies.algorithmic._
import elevate.core.strategies.normalforms._
import elevate.core.strategies.basic._
import elevate.core.rules._
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
      wrap(3)(one(_))(s) `;` reductionNormalform `;`
      wrap(1)(one(_))(mapFullFission)

  def family: Strategy => Strategy =
    s => s <+ (e => family(fmap(s))(e))

}
