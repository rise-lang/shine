package elevate.lift.rules

import elevate.core.{Failure, Strategy, Success}
import elevate.lift.strategies.predicate._
import lift.core._
import lift.core.DSL._
import lift.core.primitives.{id, join, map, split, transpose}


//noinspection MutatorLikeMethodIsParameterless
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
      case Apply(`map`, f) => Success(split(n) >> map(map(f)) >> join)
      case _ => Failure(splitJoin(n))
    }

  // fusion / fission

  def mapFusion: Strategy = `*g >> *f -> *(g >> f)`
  def `*g >> *f -> *(g >> f)`: Strategy = {
    case Apply(Apply(`map`, f), Apply(Apply(`map`, g), arg)) =>
      Success(map(g >> f)(arg))
    case _ => Failure(mapFusion)
  }

  // fission of the last function to be applied inside a map
  def mapLastFission: Strategy = `*(g >> .. >> f) -> *(g >> ..) >> *f`
  def `*(g >> .. >> f) -> *(g >> ..) >> *f`: Strategy = {
    // TODO: why gx != Identifier?
    case Apply(`map`, Lambda(x, Apply(f, gx))) if !contains(x)(f) && !isIdentifier(gx) =>
      Success(Apply(`map`, Lambda(x, gx)) >> map(f))
    case _ => Failure(mapLastFission)
  }

  // identities

  def idAfter: Strategy = ` -> id`
  def ` -> id`: Strategy = x => Success(x |> id)

  def liftId: Strategy = `id -> *id`
  def `id -> *id`: Strategy = {
    case Apply(`id`, arg) => Success(Apply(map(id), arg))
    case _ => Failure(liftId)
  }

  def createTransposePair: Strategy = `id -> T >> T`
  def `id -> T >> T`: Strategy = {
    case Apply(`id`, arg) => Success(Apply(transpose >> transpose, arg))
    case _ => Failure(createTransposePair)
  }

  def `_-> T >> T`: Strategy = idAfter `;` createTransposePair

  def removeTransposePair: Strategy = `T >> T -> `
  def `T >> T -> `: Strategy = {
    case Apply(`transpose`, Apply(`transpose`, x)) => Success(x)
    case _ => Failure(removeTransposePair)
  }
}
