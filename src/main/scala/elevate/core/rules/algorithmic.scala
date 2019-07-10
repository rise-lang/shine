package elevate.core.rules

import elevate.core.{NotApplicable, Strategy}
import lift.core._
import lift.core.DSL._
import lift.core.primitives.{join, map, slide, split, transpose}

object algorithmic {
  // - Notation -
  // x >> y: piping operator, x then y
  // *f: map(f)
  // T: transpose
  // S: slide/split
  // J: join

  // divide & conquer

  def splitJoin: Nat => Strategy = `*f -> S >> **f >> J`
  def `*f -> S >> **f >> J`: Nat => Strategy =
    n => {
      case Apply(`map`, f) => split(n) >> map(map(f)) >> join
      case _ => throw NotApplicable(splitJoin(n))
    }

  // fusion / fission

  def mapFusion: Strategy = `*g >> *f -> *(g >> f)`
  def `*g >> *f -> *(g >> f)`: Strategy = {
    case Apply(Apply(`map`, f), Apply(Apply(`map`, g), arg)) =>
      map(g >> f)(arg)
    case _ => throw NotApplicable(mapFusion)
  }

  // fission of the last function to be applied inside a map
  def mapLastFission: Strategy = `*(g >> .. >> f) -> *(g >> ..) >> *f`
  def `*(g >> .. >> f) -> *(g >> ..) >> *f`: Strategy = {
    // TODO? 'x' should not be used in 'f' or 'g'
    /* chain of two fission
    case Apply(`map`, Lambda(x1, Apply(f, Apply(g, x2)))) if x1 == x2 =>
      map(g) >> map(f)
      */
    case Apply(`map`, Lambda(x, Apply(f, gx))) =>
      Apply(`map`, Lambda(x, gx)) >> map(f)
    case _ => throw NotApplicable(mapLastFission)
  }

  // identities

  def createTransposePair: Strategy = ` -> T >> T`
  def ` -> T >> T`: Strategy = x => x |> transpose |> transpose

  def removeTransposePair: Strategy = `T >> T -> `
  def `T >> T -> `: Strategy = {
    case Apply(`transpose`, Apply(`transpose`, x)) => x
    case _ => throw NotApplicable(removeTransposePair)
  }
}
