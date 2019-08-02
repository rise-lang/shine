package elevate.lift.rules

import elevate.core.strategies.basic.seq
import elevate.core.{Elevate, Failure, Lift, RewriteResult, Rule, Strategy, Success}
import elevate.lift.strategies.predicate._
import elevate.lift.strategies.traversal._
import elevate.lift.strategies.traversal.body
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
  case object splitJoin { def apply(n: Nat) = `*f -> S >> **f >> J`(n) }
  case class `*f -> S >> **f >> J`(n: Nat) extends Strategy[Expr] {
    def apply(e: Expr): RewriteResult[Expr] = e match {
      case Apply(`map`, f) => Success(split(n) >> map(map(f)) >> join)
      case _ => Failure(splitJoin(n))
    }
    override def toString = s"splitJoin($n)"
  }

  // fusion / fission
  case object mapFusion extends Rule[Lift] { def apply(e: Lift) = `*g >> *f -> *(g >> f)`(e) }
  case object `*g >> *f -> *(g >> f)` extends Rule[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case Apply(Apply(`map`, f), Apply(Apply(`map`, g), arg)) =>
        Success(map(g >> f)(arg))
      case _ => Failure(mapFusion)
    }
    override def toString = s"mapFusion"
  }

  // fission of the last function to be applied inside a map
  case object mapLastFission extends Rule[Lift] { def apply(e: Lift) = `*(g >> .. >> f) -> *(g >> ..) >> *f`(e) }
  case object `*(g >> .. >> f) -> *(g >> ..) >> *f` extends Rule[Lift] {
    // TODO: why gx != Identifier?
    def apply(e: Lift): RewriteResult[Lift] = e match {
        // todo fix constraint again
      case Apply(`map`, Lambda(x, Apply(f, gx))) if !contains(x)(f) && !isIdentifier(gx) =>
        Success(Apply(`map`, Lambda(x, gx)) >> map(f))
      case _ => Failure(mapLastFission)
    }
    override def toString = s"mapLastFission"
  }


  // identities

  /*
  def idAfter: Strategy = ` -> id`
  def ` -> id`: Strategy = {case x:Expr => Success[Expr](x |> id)}

  def liftId: Strategy = `id -> *id`
  def `id -> *id`: Strategy = {
    case Apply(`id`, arg) => Success(Apply(map(id), arg))
  }

  def createTransposePair: Strategy = `id -> T >> T`
  def `id -> T >> T`: Strategy = {
    case Apply(`id`, arg) => Success(Apply(transpose >> transpose, arg))
  }

  def `_-> T >> T`: Strategy = idAfter `;` createTransposePair

  def removeTransposePair: Strategy = `T >> T -> `
  def `T >> T -> `: Strategy = {
    case Apply(`transpose`, Apply(`transpose`, x)) => Success(x)
    case _ => Failure(removeTransposePair)
  }
   */
}
