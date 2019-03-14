package idealised.rewriting

import idealised.rewriting.Elevate._
import idealised.rewriting.Strategies._

object Tiling {

  def tileEveryDimension: Int => Strategy = n => expr => {
    Location.findAll( e =>
      isDefined(Rules.splitJoin(n))(e) // && e.isConcrete
    )(expr)
      .foldLeft(expr)( (e, loc: Location) => `try`( applyAt(Rules.splitJoin(n))(loc) )(e) )
  }

  def rearrangeDimensions: Int => Strategy = {
    case d if d < 2 =>
      Strategies.id
    case 2 =>
      shuffleDimension(2)
    case d if d > 2 =>
      rearrangeDimensions(d - 1) `;` shuffleDimension(d)
  }

  def shuffleDimension: Int => Strategy = ???

  def tiling: Int => Strategy = n => {
    tileEveryDimension(n) `;`
      rewriteNormalForm `;`
      rearrangeDimensions(???)
  }

}
