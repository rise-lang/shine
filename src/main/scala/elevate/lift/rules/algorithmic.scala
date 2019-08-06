package elevate.lift.rules

import elevate.core.strategies.basic.seq
import elevate.core._
import elevate.core.strategies.predicate._
import elevate.lift.strategies.predicate._
import elevate.core.strategies.traversal._
import elevate.lift.strategies.traversal._
import lift.core._
import lift.core.DSL._
import lift.core.primitives.{id, join, map, split, transpose}


object algorithmic {
  // - Notation -
  // x >> y: piping operator, x then y
  // *f: map(f)
  // T: transpose
  // S: slide/split
  // J: join


  // divide & conquer
  def  splitJoin(n: Nat): Strategy[Lift] = `*f -> S >> **f >> J`(n: Nat)
  case class `*f -> S >> **f >> J`(n: Nat) extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case Apply(`map`, f) => Success(split(n) >> map(map(f)) >> join)
      case _ => Failure(splitJoin(n))
    }
    override def toString = s"splitJoin($n)"
  }

  // fusion / fission
  def mapFusion: Strategy[Lift] = `*g >> *f -> *(g >> f)`
  case object `*g >> *f -> *(g >> f)` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case Apply(Apply(`map`, f), Apply(Apply(`map`, g), arg)) =>
        Success(map(g >> f)(arg))
      case _ => Failure(mapFusion)
    }
    override def toString = s"mapFusion"
  }

  // fission of the last function to be applied inside a map
  def mapLastFission: Strategy[Lift] = `*(g >> .. >> f) -> *(g >> ..) >> *f`
  case object `*(g >> .. >> f) -> *(g >> ..) >> *f` extends Strategy[Lift] {
    // TODO: why gx != Identifier?
    def apply(e: Lift): RewriteResult[Lift] = e match {
        // todo fix constraint again
      case Apply(`map`, Lambda(x, Apply(f, gx))) if !contains[Lift](x).apply(f) && !isIdentifier(gx) =>
        Success(Apply(`map`, Lambda(x, gx)) >> map(f))
      case _ => Failure(mapLastFission)
    }
    override def toString = s"mapLastFission"
  }


  // identities

  def idAfter: Strategy[Lift] = ` -> id`
  case object ` -> id` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = Success(e |> id)
  }

  def liftId: Strategy[Lift] = `id -> *id`
  case object `id -> *id` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case Apply(`id`, arg) => Success(Apply(map(id), arg))
      case _ => Failure(liftId)
    }
  }

  def createTransposePair: Strategy[Lift] = `id -> T >> T`
  case object `id -> T >> T` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case Apply(`id`, arg) => Success(Apply(transpose >> transpose, arg))
      case _ => Failure(createTransposePair)
    }
  }

  def `_-> T >> T`: Strategy[Lift] = idAfter `;` createTransposePair

  def removeTransposePair: Strategy[Lift] = `T >> T -> `
  case object `T >> T -> ` extends Strategy[Lift]  {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case Apply(`transpose`, Apply(`transpose`, x)) => Success(x)
      case _ => Failure(removeTransposePair)
    }
  }
}
