package idealised.rewriting

import idealised.rewriting._
import idealised.rewriting.Elevate._
import idealised.rewriting.Strategies._

object Tiling {

//  def tileEveryDimension: Int => Strategy = n => expr => {
//    Location.findAll( e =>
//      isDefined(Rules.splitJoin(n))(e) // && e.isConcrete
//    )(expr)
//      .foldLeft(expr)( (e, loc: Location) => `try`( applyAt(Rules.splitJoin(n))(loc) )(e) )
//  }

  def tiling: Int => Strategy = n => expr => {
//    tileEveryDimension(n)(expr) `;` rewriteNormalForm
    ???
  }

}
