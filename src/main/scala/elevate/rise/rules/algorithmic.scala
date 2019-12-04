package elevate.rise.rules

import elevate.core._
import elevate.core.strategies.predicate._
import elevate.rise.strategies.predicate._
import elevate.rise.rules.traversal._
import elevate.rise._
import lift.core._
import lift.core.DSL._
import lift.core.primitives._
import lift.core.types._

//noinspection MutatorLikeMethodIsParameterless
object algorithmic {

  // - Notation -
  // x >> y: piping operator, x then y
  // *f: map(f)
  // T: transpose
  // S: slide/split
  // J: join

  // divide & conquer

  def  splitJoin(n: Nat): Strategy[Rise] = `*f -> S >> **f >> J`(n: Nat)
  case class `*f -> S >> **f >> J`(n: Nat) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(Map(), f) => Success(split(n) >> map(map(f)) >> join)
      case _             => Failure(splitJoin(n))
    }
    override def toString = s"splitJoin($n)"
  }

  // fusion / fission

  def mapFusion: Strategy[Rise] = `*g >> *f -> *(g >> f)`
  case object `*g >> *f -> *(g >> f)` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(App(Map(), f), App(App(Map(), g), arg)) => Success(map(g >> f)(arg))
      case _                                           => Failure(mapFusion)
    }
    override def toString = s"mapFusion"
  }

  // fission of the last function to be applied inside a map
  def mapLastFission: Strategy[Rise] = `*(g >> .. >> f) -> *(g >> ..) >> *f`
  case object `*(g >> .. >> f) -> *(g >> ..) >> *f` extends Strategy[Rise] {
    // this is an example where we don't want to fission if gx == Identifier:
    // (map λe4. (((((zip: (K.float -> (K.float -> K.(float, float)))) (e3: K.float)): (K.float -> K.(float, float))) (e4: K.float)): K.(float, float)))
    // gx == (e4: K.float)
    // in this case we would return some form of map(id):
    // ((map λe4. (e4: K.float)) e743))
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(Map(), Lambda(x, App(f, gx))) if !contains[Rise](x).apply(f) && !isIdentifier(gx) =>
        Success(DSL.app(map, lambda(x, gx)) >> map(f))
      case _ => Failure(mapLastFission)
    }
    override def toString = s"mapLastFission"
  }

  // identities

  def idAfter: Strategy[Rise] = ` -> id`
  case object ` -> id` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = Success(e |> id)
    override def toString = "idAfter"
  }

  def liftId: Strategy[Rise] = `id -> *id`
  case object `id -> *id` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(Id(), arg) => Success(DSL.app(map(id), arg))
      case _              => Failure(liftId)
    }
    override def toString = "liftId"
  }

  def createTransposePair: Strategy[Rise] = `id -> T >> T`
  case object `id -> T >> T` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(Id(), arg) => Success(DSL.app(transpose >> transpose, arg))
      case _              => Failure(createTransposePair)
    }
    override def toString = "createTransposePair"
  }

  def `_-> T >> T`: Strategy[Rise] = idAfter `;` createTransposePair

  def removeTransposePair: Strategy[Rise] = `T >> T -> `
  case object `T >> T -> ` extends Strategy[Rise]  {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(Transpose(), App(Transpose(), x)) => Success(x)
      case _                                     => Failure(removeTransposePair)
    }
    override def toString = "createTransposePair"
  }

  // slideSeq fusion
  import lift.OpenCL.primitives._
  import lift.OpenCL.DSL._

  def slideSeqFusion: Strategy[Rise] = `slideSeq(f) >> map(g) -> slideSeq(f >> g)`
  def `slideSeq(f) >> map(g) -> slideSeq(f >> g)`: Strategy[Rise] = {
    case App(App(Map(), g), App(App(App(DepApp(DepApp(SlideSeq(rot), sz: Nat), sp: Nat), wr), f), e)) =>
      Success(slideSeq(rot)(sz)(sp)(wr)(f >> g)(e))
    case App(App(Map(), g), App(App(App(DepApp(DepApp(DepApp(OclSlideSeq(rot), a: AddressSpace), sz: Nat), sp: Nat), wr), f), e)) =>
      Success(oclSlideSeq(rot)(a)(sz)(sp)(wr)(f >> g)(e))
    case _ => Failure(slideSeqFusion)
  }
}
