package elevate.lift.rules

import elevate.core.strategies.predicate._
import elevate.core.{Failure, Lift, NotApplicable, Strategy, Success}
import elevate.lift.strategies.predicate._
import lift.core.{Apply, DepApply, Expr, Lambda, Nat, Primitive}
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
  def `**f >> T -> T >> **f`: Strategy[Lift] = {
    case Apply(
    `transpose`,
    Apply(Apply(`map`, Apply(`map`, f)), y)) =>
      Success(y |> transpose |> map(map(f)))
    // LCNF
    case Apply(
      `transpose`,
      Apply(
        Apply(`map`, Lambda(n7, Apply(
          Apply(`map`, Lambda(n6, Apply(
            f, n61))), n71))),
        arg
      )
    ) if contains(n6)(n61) && contains(n7)(n71) =>
      Success(arg |> transpose |> map(map(f)))
    case _ => Failure(mapMapFBeforeTranspose)
  }

  def transposeBeforeMapMapF: Strategy[Lift] = `T >> **f -> **f >> T`
  def `T >> **f -> **f >> T`: Strategy[Lift] = {
    case Apply(
    Apply(`map`, Apply(`map`, f)),
    Apply(`transpose`, y)) =>
      Success(y |> map(map(f)) |> transpose)
    case _ => Failure(transposeBeforeMapMapF)
  }

  // split/slide

  private def isSplitOrSlide(s: Expr): Boolean = s match {
    case DepApply(DepApply(`slide`, _: Nat), _: Nat) => true
    case DepApply(`split`, _: Nat) => true
    case _ => false
  }

  def slideBeforeMapMapF: Strategy[Lift] = `S >> **f -> *f >> S`
  def `S >> **f -> *f >> S`: Strategy[Lift] = {
    case Apply(
    Apply(`map`, Apply(`map`, f)),
    Apply(s, y)) if isSplitOrSlide(s) =>
      Success(y |> map(f) |> s)
    case _ => Failure(slideBeforeMapMapF)
  }

  def mapFBeforeSlide: Strategy[Lift] = `*f >> S -> S >> **f`
  def `*f >> S -> S >> **f`: Strategy[Lift] = {
    case Apply(
    s,
    Apply(Apply(`map`, f), y)) if isSplitOrSlide(s) =>
      Success(y |> s |> map(map(f)))
    case _ => Failure(mapFBeforeSlide)
  }

  // join

  def joinBeforeMapF: Strategy[Lift] = `J >> *f -> **f >> J`
  def `J >> *f -> **f >> J`: Strategy[Lift] = {
    case Apply(
    Apply(`map`, f),
    Apply(`join`, y)
    ) =>
      Success(y |> map(map(f)) >> join)
    case _ => Failure(joinBeforeMapF)
  }

  def mapMapFBeforeJoin: Strategy[Lift] = `**f >> J -> J >> *f`
  def `**f >> J -> J >> *f`: Strategy[Lift] = {
    case Apply(
    `join`,
    Apply(Apply(map, Apply(`map`, f)), y)
    ) =>
      Success(y |> join |> map(f))
    case _ => Failure(mapMapFBeforeJoin)
  }

  // special-cases
  // slide + transpose

  def transposeBeforeSlide: Strategy[Lift] = `T >> S -> *S >> T >> *T`
  def `T >> S -> *S >> T >> *T`: Strategy[Lift] = {
    case Apply(
    s,
    Apply(`transpose`, y)
    ) if isSplitOrSlide(s) =>
      Success(y |> map(s) |> transpose >> map(transpose))
    case _ => Failure(transposeBeforeSlide)
  }

  def transposeBeforeMapSlide: Strategy[Lift] = `T >> *S -> S >> *T >> T`
  def `T >> *S -> S >> *T >> T`: Strategy[Lift] = {
    case Apply(
    Apply(`map`, s),
    Apply(`transpose`, y)
    ) if isSplitOrSlide(s) =>
      Success(y |> s |> map(transpose) |> transpose)
    case _ => Failure(transposeBeforeMapSlide)
  }

  def mapSlideBeforeTranspose: Strategy[Lift] = `*S >> T -> T >> S >> *T`
  def `*S >> T -> T >> S >> *T`: Strategy[Lift] = {
    case Apply(
    `transpose`,
    Apply(Apply(`map`, s), y)
    ) if isSplitOrSlide(s) =>
      Success(y |> transpose >> s >> map(transpose))
    case _ => Failure(mapSlideBeforeTranspose)
  }

  // transpose + join

  def joinBeforeTranspose: Strategy[Lift] = `J >> T -> *T >> T >> *J`
  def `J >> T -> *T >> T >> *J`: Strategy[Lift] = {
    case Apply(
    `transpose`,
    Apply(`join`, y)
    ) =>
      Success(y |> map(transpose) |> transpose |> map(join))
    case _ => Failure(joinBeforeTranspose)
  }

  def transposeBeforeMapJoin: Strategy[Lift] = `T >> *J -> *T >> J >> T`
  def `T >> *J -> *T >> J >> T`: Strategy[Lift] = {
    case Apply(
    Apply(`map`, `join`),
    Apply(`transpose`, y)
    ) =>
      Success(y |> map(transpose) |> join |> transpose)
    case _ => Failure(transposeBeforeMapJoin)
  }

  def mapTransposeBeforeJoin: Strategy[Lift] = `*T >> J -> T >> *J >> T`
  def `*T >> J -> T >> *J >> T`: Strategy[Lift] = {
    case Apply(
    `join`,
    Apply(Apply(`map`, `transpose`), y)
    ) =>
      Success(y |> transpose |> map(join) |> transpose)
    case _ => Failure(mapTransposeBeforeJoin)
  }

  def mapJoinBeforeTranspose: Strategy[Lift] = `*J >> T -> T >> *T >> J`
  def `*J >> T -> T >> *T >> J`: Strategy[Lift] = {
    case Apply(
    `transpose`,
    Apply(Apply(`map`, `join`), y)
    ) =>
      Success(y |> transpose |> map(transpose) |> join)
    case _ => Failure(mapJoinBeforeTranspose)
  }

  // join + join

  def joinBeforeJoin: Strategy[Lift] = `J >> J -> *J >> J`
  def `J >> J -> *J >> J`: Strategy[Lift] = {
    case Apply(
    `join`,
    Apply(`join`, y)
    ) =>
      Success(y |> map(join) >> join)
    case _ => Failure(joinBeforeJoin)
  }

  def mapJoinBeforeJoin: Strategy[Lift] = `*J >> J -> J >> J`
  def `*J >> J -> J >> J`: Strategy[Lift] = {
    case Apply(
    `join`,
    Apply(Apply(`map`, `join`), y)
    ) =>
      Success(y |> join |> join)
    case _ => Failure(mapJoinBeforeJoin)
  }

  // split + slide

  def slideBeforeSplit: Strategy[Lift] = `slide(n)(s) >> split(k) -> slide(k+n-s)(k) >> map(slide(n)(s))`
  def `slide(n)(s) >> split(k) -> slide(k+n-s)(k) >> map(slide(n)(s))`: Strategy[Lift] = {
    case Apply(
    DepApply(`split`, k: Nat),
    Apply(DepApply(DepApply(`slide`, n: Nat), s: Nat), y)
    ) =>
      Success(y |> slide(k+n-s)(k) |> map(slide(n)(s)))
    case _ => Failure(slideBeforeSplit)
  }
}
