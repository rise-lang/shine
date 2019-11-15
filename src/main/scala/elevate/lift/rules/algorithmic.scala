package elevate.lift.rules

import elevate.core.strategies.basic.seq
import elevate.core._
import elevate.core.strategies.predicate._
import elevate.lift.strategies.predicate._
import elevate.core.strategies.traversal._
import elevate.lift.strategies.traversal._
import elevate.lift._
import lift.core._
import lift.core.DSL._
import lift.core.primitives._
import lift.core.types._
import elevate.lift.extractors._

//noinspection MutatorLikeMethodIsParameterless
object algorithmic {
  // - Notation -
  // x >> y: piping operator, x then y
  // *f: map(f)
  // T: transpose
  // S: slide/split
  // J: join

  // lift reduce
  def liftReduce: Strategy[Lift] = `map(reduce(...)) -> reduce(map(...))`
  case object `map(reduce(...)) -> reduce(map(...))` extends Strategy[Lift] {

   def apply(e: Lift): RewriteResult[Lift] = e match {

      case App(Map(), Lambda(mapVar, App(App(App(rx @ (Reduce() | ReduceSeq()), op),
           init :: (dt: DataType)), reduceArg))) :: FunType(ArrayType(size, ArrayType(_,_)), _) =>

        def reduceMap(zippedMapArg : (Expr, Expr) => Expr, reduceArg: Expr): RewriteResult[Lift] = {
          Success(
            rx(fun((acc, y) => // y :: 16.n793.(float,float), acc:: 16.32.(float)
              map(fun(x => DSL.app(DSL.app(op, fst(x)), snd(x)))) $ zippedMapArg(acc, y)
            ))(generate(fun(IndexType(size) ->: dt)(_ => init))) o reduceArg
          )
        }

        reduceArg match {
          // simple case (see test "lift reduce")
          // for some reason mapVar might be untyped, hence simply trying `==` fails
          case x if x == mapVar =>
            reduceMap(
              (acc, y) => zip(acc, y),
              transpose
            )
          // zipped input (see test "MM to MM-LoopMKN")
          case App(App(Zip(), u), v) =>
            val notToBeTransposed = if (mapVar == u) v else u
            reduceMap(
              zippedMapArg = (acc, y) => zip(acc, map(fun(bs => pair(bs, fst(y)))) $ snd(y)),
              reduceArg = zip(notToBeTransposed) o transpose
            )
            // input is tile1.tile2.dim.(float,float)
            // dim needs to be reduced -> we need dim.tile1.tile2.(float,float)
            // todo what's the general case? How to (re-)order dimensions here?
          case _ =>
            val result = reduceMap(
              (acc, y) => zip(acc, y),
              transpose o map(transpose)
            )
            result
        }

      case _ => Failure(liftReduce)
    }
    override def toString = "liftReduce"
  }

  // divide & conquer
  def  splitJoin(n: Nat): Strategy[Lift] = `*f -> S >> **f >> J`(n: Nat)
  case class `*f -> S >> **f >> J`(n: Nat) extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case App(Map(), f) => Success(split(n) >> map(map(f)) >> join)
      case _ => Failure(splitJoin(n))
    }
    override def toString = s"splitJoin($n)"
  }

  // fusion / fission
  def mapFusion: Strategy[Lift] = `*g >> *f -> *(g >> f)`
  case object `*g >> *f -> *(g >> f)` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case App(App(Map(), f), App(App(Map(), g), arg)) =>
        Success(map(g >> f)(arg))
      case _ => Failure(mapFusion)
    }
    override def toString = s"mapFusion"
  }

  // fission of the last function to be applied inside a map
  def mapLastFission: Strategy[Lift] = `*(g >> .. >> f) -> *(g >> ..) >> *f`
  case object `*(g >> .. >> f) -> *(g >> ..) >> *f` extends Strategy[Lift] {
    // this is an example where we don't want to fission if gx == Identifier:
    // (map λe4. (((((zip: (K.float -> (K.float -> K.(float, float)))) (e3: K.float)): (K.float -> K.(float, float))) (e4: K.float)): K.(float, float)))
    // gx == (e4: K.float)
    // in this case we would return some form of map(id):
    // ((map λe4. (e4: K.float)) e743))
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case y @ App(Map(), Lambda(x, App(f, gx))) if !contains[Lift](x).apply(f) && !isIdentifier(gx) =>
        Success(DSL.app(map, lambda(x, gx)) >> map(f))
      case _ => Failure(mapLastFission)
    }
    override def toString = s"mapLastFission"
  }

  // identities

  def idAfter: Strategy[Lift] = ` -> id`
  case object ` -> id` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = Success(e |> id)
    override def toString = "idAfter"
  }

  def liftId: Strategy[Lift] = `id -> *id`
  case object `id -> *id` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case App(Id(), arg) => Success(DSL.app(map(id), arg))
      case _ => Failure(liftId)
    }
    override def toString = "liftId"
  }

  def createTransposePair: Strategy[Lift] = `id -> T >> T`
  case object `id -> T >> T` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case App(Id(), arg) => Success(DSL.app(transpose >> transpose, arg))
      case _ => Failure(createTransposePair)
    }
    override def toString = "createTransposePair"
  }

  def `_-> T >> T`: Strategy[Lift] = idAfter `;` createTransposePair

  def removeTransposePair: Strategy[Lift] = `T >> T -> `
  case object `T >> T -> ` extends Strategy[Lift]  {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case App(Transpose(), App(Transpose(), x)) => Success(x)
      case _ => Failure(removeTransposePair)
    }
    override def toString = "createTransposePair"
  }

  // slideSeq fusion
  import lift.OpenCL.primitives._
  import lift.OpenCL.DSL._

  def slideSeqFusion: Strategy[Lift] = `slideSeq(f) >> map(g) -> slideSeq(f >> g)`
  def `slideSeq(f) >> map(g) -> slideSeq(f >> g)`: Strategy[Lift] = {
    case App(App(Map(), g), App(App(App(DepApp(DepApp(SlideSeq(rot), sz: Nat), sp: Nat), wr), f), e)) =>
      Success(slideSeq(rot)(sz)(sp)(wr)(f >> g)(e))
    case App(App(Map(), g), App(App(App(DepApp(DepApp(DepApp(OclSlideSeq(rot), a: AddressSpace), sz: Nat), sp: Nat), wr), f), e)) =>
      Success(oclSlideSeq(rot)(a)(sz)(sp)(wr)(f >> g)(e))
    case _ => Failure(slideSeqFusion)
  }
}
