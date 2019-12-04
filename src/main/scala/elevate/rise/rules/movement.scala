package elevate.rise.rules

import elevate.core.strategies.predicate._
import elevate.rise.rules.traversal._
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import elevate.rise._
import lift.core._
import lift.core.primitives._
import lift.core.DSL._
import lift.core.types.{ArrayType, DataType, FunType, IndexType}

// Describing possible movements between pairs of rise primitives (potentially nested in maps)

// todo: should all rules expect LCNF-normalized expressions as input?
object movement {

  // - Notation -
  // x >> y: piping operator, x then y
  // *f: map(f)
  // T: transpose
  // S: slide/split
  // J: join

  // transpose

  def mapMapFBeforeTranspose: Strategy[Rise] = `**f >> T -> T >> **f`
  case object `**f >> T -> T >> **f` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
        Transpose(),
        App(App(Map(), App(Map(), f)), y)) =>
          Success(y |> transpose |> map(map(f)))
      // LCNF
      case App(Transpose(),
      App(
      mapMapF @ App(Map(), Lambda(n7, App(
                App(Map(), Lambda(n6, App(
      f, n61))), n71))),
      arg
      )
      ) if contains[Rise](n6).apply(n61) && contains[Rise](n7).apply(n71) =>
        Success(arg |> transpose |> mapMapF)
      case _ => Failure(mapMapFBeforeTranspose)
    }
    override def toString = "mapMapFBeforeTranspose"
  }

  def transposeBeforeMapMapF: Strategy[Rise] = `T >> **f -> **f >> T`
  case object `T >> **f -> **f >> T` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      App(Map() , App(Map(), f)),
      App(Transpose() , y)) =>
        Success(y |> map(map(f)) |> transpose)
      case _ => Failure(transposeBeforeMapMapF)
    }
    override def toString = "transposeBeforeMapMapF"
  }

  // split/slide

  private def isSplitOrSlide(s: Expr): Boolean = s match {
    case DepApp(DepApp(Slide(), _: Nat), _: Nat) => true
    case DepApp(Split(), _: Nat)                 => true
    case _                                       => false
  }

  def slideBeforeMapMapF: Strategy[Rise] = `S >> **f -> *f >> S`
  case object `S >> **f -> *f >> S` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      App(Map(), App(Map(), f)),
      App(s, y)) if isSplitOrSlide(s) =>
        Success(y |> map(f) |> s)
      case _ => Failure(slideBeforeMapMapF)
    }
    override def toString = "slideBeforeMapMapF"
  }

  def mapFBeforeSlide: Strategy[Rise] = `*f >> S -> S >> **f`
  case object `*f >> S -> S >> **f` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      s,
      App(App(Map(), f), y)) if isSplitOrSlide(s) =>
        Success(y |> s |> map(map(f)))
      case _ => Failure(mapFBeforeSlide)
    }
    override def toString = "mapFBeforeSlide"
  }

  // join

  def joinBeforeMapF: Strategy[Rise] = `J >> *f -> **f >> J`
  case object `J >> *f -> **f >> J` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      App(Map(), f),
      App(Join(), y)
      ) =>
        Success(y |> map(map(f)) >> join)
      case _ => Failure(joinBeforeMapF)
    }
    override def toString = "joinBeforeMapF"
  }

  def mapMapFBeforeJoin: Strategy[Rise] = `**f >> J -> J >> *f`
  case object `**f >> J -> J >> *f` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      Join(),
      App(App(map, App(Map(), f)), y)
      ) =>
        Success(y |> join |> map(f))
      case _ => Failure(mapMapFBeforeJoin)
    }
    override def toString = "mapMapFBeforeJoin"
  }

  // special-cases
  // slide + transpose

  def transposeBeforeSlide: Strategy[Rise] = `T >> S -> *S >> T >> *T`
  case object `T >> S -> *S >> T >> *T` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      s,
      App(Transpose(), y)
      ) if isSplitOrSlide(s) =>
        Success(y |> map(s) |> transpose >> map(transpose))
      case _ => Failure(transposeBeforeSlide)
    }
    override def toString = "transposeBeforeSlide"
  }

  def transposeBeforeMapSlide: Strategy[Rise] = `T >> *S -> S >> *T >> T`
  case object `T >> *S -> S >> *T >> T` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      App(Map(), s),
      App(Transpose(), y)
      ) if isSplitOrSlide(s) =>
        Success(y |> s |> map(transpose) |> transpose)
      case _ => Failure(transposeBeforeMapSlide)
    }
    override def toString = "transposeBeforeMapSlide"
  }

  def mapSlideBeforeTranspose: Strategy[Rise] = `*S >> T -> T >> S >> *T`
  case object `*S >> T -> T >> S >> *T` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      Transpose(),
      App(App(Map(), s), y)
      ) if isSplitOrSlide(s) =>
        Success(y |> transpose >> s >> map(transpose))
      case _ => Failure(mapSlideBeforeTranspose)
    }
    override def toString = "mapSlideBeforeTranspose"
  }

  // transpose + join

  def joinBeforeTranspose: Strategy[Rise] = `J >> T -> *T >> T >> *J`
  case object `J >> T -> *T >> T >> *J` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      Transpose(),
      App(Join(), y)
      ) =>
        Success(y |> map(transpose) |> transpose |> map(join))
      case _ => Failure(joinBeforeTranspose)
    }
    override def toString = "joinBeforeTranspose"
  }

  def transposeBeforeMapJoin: Strategy[Rise] = `T >> *J -> *T >> J >> T`
  case object `T >> *J -> *T >> J >> T` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      App(Map(), Join()),
      App(Transpose(), y)
      ) =>
        Success(y |> map(transpose) |> join |> transpose)
      case _ => Failure(transposeBeforeMapJoin)
    }
    override def toString = "transposeBeforeMapJoin"
  }

  def mapTransposeBeforeJoin: Strategy[Rise] = `*T >> J -> T >> *J >> T`
  case object `*T >> J -> T >> *J >> T` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      Join(),
      App(App(Map(), Transpose()), y)
      ) =>
        Success(y |> transpose |> map(join) |> transpose)
      case _ => Failure(mapTransposeBeforeJoin)
    }
    override def toString = "mapTransposeBeforeJoin"
  }

  def mapJoinBeforeTranspose: Strategy[Rise] = `*J >> T -> T >> *T >> J`
  case object `*J >> T -> T >> *T >> J` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      Transpose(),
      App(App(Map(), Join()), y)
      ) =>
        Success(y |> transpose |> map(transpose) |> join)
      case _ => Failure(mapJoinBeforeTranspose)
    }
    override def toString = "mapJoinBeforeTranspose"
  }

  // join + join

  def joinBeforeJoin: Strategy[Rise] = `J >> J -> *J >> J`
  case object `J >> J -> *J >> J` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      Join(),
      App(Join(), y)
      ) =>
        Success(y |> map(join) >> join)
      case _ => Failure(joinBeforeJoin)
    }
    override def toString = "joinBeforeJoin"
  }

  def mapJoinBeforeJoin: Strategy[Rise] = `*J >> J -> J >> J`
  case object `*J >> J -> J >> J` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      Join(),
      App(App(Map(), Join()), y)
      ) =>
        Success(y |> join |> join)
      case _ => Failure(mapJoinBeforeJoin)
    }
    override def toString = "mapJoinBeforeJoin"
  }

  // split + slide

  def slideBeforeSplit: Strategy[Rise] = `slide(n)(s) >> split(k) -> slide(k+n-s)(k) >> map(slide(n)(s))`
  case object `slide(n)(s) >> split(k) -> slide(k+n-s)(k) >> map(slide(n)(s))` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      DepApp(Split(), k: Nat),
      App(DepApp(DepApp(Slide(), n: Nat), s: Nat), y)
      ) =>
        Success(y |> slide(k + n - s)(k) |> map(slide(n)(s)))
      case _ => Failure(slideBeforeSplit)
    }
    override def toString = "slideBeforeSplit"
  }


  // nested map + reduce

  // todo simplify
  case object liftReduce extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {

      case App(Map(), Lambda(mapVar, App(App(App(rx @ (Reduce() | ReduceSeq()), op),
      init :: (dt: DataType)), reduceArg))) :: FunType(ArrayType(size, ArrayType(_,_)), _) =>

        def reduceMap(zippedMapArg : (Expr, Expr) => Expr, reduceArgFun: Expr): RewriteResult[Rise] = {
          Success(
            rx(fun((acc, y) =>
              map(fun(x => DSL.app(DSL.app(op, fst(x)), snd(x)))) $ zippedMapArg(acc, y)
            ))(generate(fun(IndexType(size) ->: dt)(_ => init))) o reduceArgFun
          )
        }

        reduceArg match {
          // simple case (see test "lift reduce")
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
              reduceArgFun = zip(notToBeTransposed) o transpose
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
}
