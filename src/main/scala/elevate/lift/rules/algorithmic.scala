package elevate.lift.rules

import elevate.core.strategies.basic.seq
import elevate.core.{Failure, RewriteResult, Strategy, StrategyT, Success}
import elevate.lift.strategies.predicate._
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


  // like this we can avoid using parenthesis
  object rules {
    def mapFusion: StrategyT[Expr] = `*g >> *f -> *(g >> f)`()
    def splitJoin(n : Nat): StrategyT[Expr] = `*f -> S >> **f >> J`(n)
    def mapLastFission: StrategyT[Expr] = `*(g >> .. >> f) -> *(g >> ..) >> *f`()
    def bodyFission: StrategyT[StrategyT[Expr]] = bodyFissionObject()
  }

  import rules._

  // divide & conquer
  case class `*f -> S >> **f >> J`(n: Nat) extends StrategyT[Expr] {
    def apply(e: Expr): RewriteResult[Expr] = e match {
      case Apply(`map`, f) => Success(split(n) >> map(map(f)) >> join)
      case _ => Failure(splitJoin(n))
    }
  }

  // fusion / fission
  case class `*g >> *f -> *(g >> f)`() extends StrategyT[Expr] {
    def apply(e: Expr): RewriteResult[Expr] = e match {
      case Apply(Apply(`map`, f), Apply(Apply(`map`, g), arg)) =>
        Success(map(g >> f)(arg))
      case _ => Failure(mapFusion)
    }
  }

  // fission of the last function to be applied inside a map
  case class `*(g >> .. >> f) -> *(g >> ..) >> *f`() extends StrategyT[Expr] {
    // TODO: why gx != Identifier?
    def apply(e: Expr): RewriteResult[Expr] = e match {
        // todo fix constraint again
      case Apply(`map`, Lambda(x, Apply(f, gx))) => //if !contains(x)(f) && !isIdentifier(gx) =>
        Success(Apply(`map`, Lambda(x, gx)) >> map(f))
      case _ => Failure(mapLastFission)
    }
  }

  case class bodyFissionObject() extends StrategyT[StrategyT[Expr]] {
    def apply(e: StrategyT[Expr]): RewriteResult[StrategyT[Expr]] = e match {
      case body(seq(f,s)) => Success(seq(body(f),body(s)))
      case x => Failure(bodyFissionObject())
    }
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
