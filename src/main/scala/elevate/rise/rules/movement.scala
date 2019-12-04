package elevate.rise.rules

import elevate.core.strategies.predicate._
import elevate.rise.strategies.traversal._
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import elevate.rise.Lift
import lift.core._
import lift.core.primitives._
import lift.core.DSL._

// Describing possible movements between pairs of primitives

object movement {
  // - Notation -
  // x >> y: piping operator, x then y
  // *f: map(f)
  // T: transpose
  // S: slide/split
  // J: join

  // transpose

  def mapMapFBeforeTranspose: Strategy[Lift] = `**f >> T -> T >> **f`
  case object `**f >> T -> T >> **f` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
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
      ) if contains[Lift](n6).apply(n61) && contains[Lift](n7).apply(n71) =>
        Success(arg |> transpose |> mapMapF)
      case _ => Failure(mapMapFBeforeTranspose)
    }
    override def toString = "mapMapFBeforeTranspose"
  }

  def transposeBeforeMapMapF: Strategy[Lift] = `T >> **f -> **f >> T`
  case object `T >> **f -> **f >> T` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
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
    case DepApp(Split(), _: Nat) => true
    case _ => false
  }

  def slideBeforeMapMapF: Strategy[Lift] = `S >> **f -> *f >> S`
  case object `S >> **f -> *f >> S` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case App(
      App(Map(), App(Map(), f)),
      App(s, y)) if isSplitOrSlide(s) =>
        Success(y |> map(f) |> s)
      case _ => Failure(slideBeforeMapMapF)
    }
    override def toString = "slideBeforeMapMapF"
  }

  def mapFBeforeSlide: Strategy[Lift] = `*f >> S -> S >> **f`
  case object `*f >> S -> S >> **f` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case App(
      s,
      App(App(Map(), f), y)) if isSplitOrSlide(s) =>
        Success(y |> s |> map(map(f)))
      case _ => Failure(mapFBeforeSlide)
    }
    override def toString = "mapFBeforeSlide"
  }

  // join

  def joinBeforeMapF: Strategy[Lift] = `J >> *f -> **f >> J`
  case object `J >> *f -> **f >> J` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case App(
      App(Map(), f),
      App(Join(), y)
      ) =>
        Success(y |> map(map(f)) >> join)
      case _ => Failure(joinBeforeMapF)
    }
    override def toString = "joinBeforeMapF"
  }

  def mapMapFBeforeJoin: Strategy[Lift] = `**f >> J -> J >> *f`
  case object `**f >> J -> J >> *f` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
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

  def transposeBeforeSlide: Strategy[Lift] = `T >> S -> *S >> T >> *T`
  case object `T >> S -> *S >> T >> *T` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case App(
      s,
      App(Transpose(), y)
      ) if isSplitOrSlide(s) =>
        Success(y |> map(s) |> transpose >> map(transpose))
      case _ => Failure(transposeBeforeSlide)
    }
    override def toString = "transposeBeforeSlide"
  }

  def transposeBeforeMapSlide: Strategy[Lift] = `T >> *S -> S >> *T >> T`
  case object `T >> *S -> S >> *T >> T` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case App(
      App(Map(), s),
      App(Transpose(), y)
      ) if isSplitOrSlide(s) =>
        Success(y |> s |> map(transpose) |> transpose)
      case _ => Failure(transposeBeforeMapSlide)
    }
    override def toString = "transposeBeforeMapSlide"
  }

  def mapSlideBeforeTranspose: Strategy[Lift] = `*S >> T -> T >> S >> *T`
  case object `*S >> T -> T >> S >> *T` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
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

  def joinBeforeTranspose: Strategy[Lift] = `J >> T -> *T >> T >> *J`
  case object `J >> T -> *T >> T >> *J` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case App(
      Transpose(),
      App(Join(), y)
      ) =>
        Success(y |> map(transpose) |> transpose |> map(join))
      case _ => Failure(joinBeforeTranspose)
    }
    override def toString = "joinBeforeTranspose"
  }

  def transposeBeforeMapJoin: Strategy[Lift] = `T >> *J -> *T >> J >> T`
  case object `T >> *J -> *T >> J >> T` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case App(
      App(Map(), Join()),
      App(Transpose(), y)
      ) =>
        Success(y |> map(transpose) |> join |> transpose)
      case _ => Failure(transposeBeforeMapJoin)
    }
    override def toString = "transposeBeforeMapJoin"
  }

  def mapTransposeBeforeJoin: Strategy[Lift] = `*T >> J -> T >> *J >> T`
  case object `*T >> J -> T >> *J >> T` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case App(
      Join(),
      App(App(Map(), Transpose()), y)
      ) =>
        Success(y |> transpose |> map(join) |> transpose)
      case _ => Failure(mapTransposeBeforeJoin)
    }
    override def toString = "mapTransposeBeforeJoin"
  }

  def mapJoinBeforeTranspose: Strategy[Lift] = `*J >> T -> T >> *T >> J`
  case object `*J >> T -> T >> *T >> J` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
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

  def joinBeforeJoin: Strategy[Lift] = `J >> J -> *J >> J`
  case object `J >> J -> *J >> J` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case App(
      Join(),
      App(Join(), y)
      ) =>
        Success(y |> map(join) >> join)
      case _ => Failure(joinBeforeJoin)
    }
    override def toString = "joinBeforeJoin"
  }

  def mapJoinBeforeJoin: Strategy[Lift] = `*J >> J -> J >> J`
  case object `*J >> J -> J >> J` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
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

  def slideBeforeSplit: Strategy[Lift] = `slide(n)(s) >> split(k) -> slide(k+n-s)(k) >> map(slide(n)(s))`
  case object `slide(n)(s) >> split(k) -> slide(k+n-s)(k) >> map(slide(n)(s))` extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case App(
      DepApp(Split(), k: Nat),
      App(DepApp(DepApp(Slide(), n: Nat), s: Nat), y)
      ) =>
        Success(y |> slide(k + n - s)(k) |> map(slide(n)(s)))
      case _ => Failure(slideBeforeSplit)
    }
    override def toString = "slideBeforeSplit"
  }
}
