package elevate.lift.rules

import elevate.core.strategies.basic.seq
import elevate.core._
import elevate.core.strategies.predicate._
import elevate.lift.strategies.predicate._
import elevate.core.strategies.traversal._
import elevate.lift.strategies.traversal._
import elevate.lift._
import elevate.lift.extractors._
import lift.core._
import lift.core.DSL._
import lift.core.primitives._
import lift.core.types.{ArrayType, DataType, FunType, IndexType}


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

      case _apply(_apply(_map(), _lambda(mapVar, _apply(
           _apply(_apply(_reduce(), op), init :: (dt:DataType)), reduceArg))),
           input@(_ :: ArrayType(size, ArrayType(_,_)))) =>

    def reduceMap(zippedMapArg : (Expr, Expr) => Expr, reduceArg: Expr): RewriteResult[Lift] = {
          Success(
            reduce(fun((y, acc) => // y :: 16.n793.(float,float), acc:: 16.32.(float)
              map(fun(x => Apply(Apply(op, fst(x)), snd(x)))) $ zippedMapArg(y, acc)
            ))(generate(fun(IndexType(size) ->: dt)(_ => init))) $ reduceArg
          )
        }

        reduceArg match {
          // simple case (see test "lift reduce")
          // for some reason mapVar might be untyped, hence simply trying `==` fails
          case x if sameButOneMayBeTyped(x, mapVar) =>
            reduceMap(
              (y, acc) => zip(y, acc),
              transpose(input)
            )
          // zipped input (see test "MM to MM-LoopMKN")
          case _apply(_apply(_zip(), u), v) =>
            val notToBeTransposed = if (sameButOneMayBeTyped(mapVar, u)) v else u
            reduceMap(
              zippedMapArg = (y, acc) => zip(map(fun(bs => pair(bs, fst(y)))) $ snd(y), acc),
              reduceArg = zip(notToBeTransposed, transpose(input))
            )
            // input is tile1.tile2.dim.(float,float)
            // dim needs to be reduced -> we need dim.tile1.tile2.(float,float)
            // todo what's the general case? How to (re-)order dimensions here?
          case _ =>
            val result = reduceMap(
              (y, acc) => zip(y, acc),
              (transpose o map(transpose)) $ input
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
      case _apply(_map(), f) => Success(split(n) >> map(map(f)) >> join)
      case _ => Failure(splitJoin(n))
    }
    override def toString = s"splitJoin($n)"
  }

  // fusion / fission
  def mapFusion: Strategy[Lift] = `*g >> *f -> *(g >> f)`
  case object `*g >> *f -> *(g >> f)` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case _apply(_apply(_map(), f), _apply(_apply(_map(), g), arg)) =>
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
      case y@_apply(_map(), _lambda(x, _apply(f, gx))) if !contains[Lift](x).apply(f) && !isIdentifier(gx) =>
        Success(Apply(`map`, Lambda(x, gx)) >> map(f))
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
      case _apply(`id`, arg) => Success(Apply(map(id), arg))
      case _ => Failure(liftId)
    }
    override def toString = "liftId"
  }

  def createTransposePair: Strategy[Lift] = `id -> T >> T`
  case object `id -> T >> T` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case _apply(`id`, arg) => Success(Apply(transpose >> transpose, arg))
      case _ => Failure(createTransposePair)
    }
    override def toString = "createTransposePair"
  }

  def `_-> T >> T`: Strategy[Lift] = idAfter `;` createTransposePair

  def removeTransposePair: Strategy[Lift] = `T >> T -> `
  case object `T >> T -> ` extends Strategy[Lift]  {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case _apply(_transpose(), _apply(_transpose(), x)) => Success(x)
      case _ => Failure(removeTransposePair)
    }
    override def toString = "createTransposePair"
  }

  case object untype extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case TypedExpr(e, _) => Success(e)
      case _ => Failure(untype)
    }
  }
}
