package elevate.lift.rules

import elevate.core.{Failure, NotApplicable, Strategy, Success}
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

  def mapMapFBeforeTranspose: Strategy = `**f >> T -> T >> **f`
  def `**f >> T -> T >> **f`: Strategy = {
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
    ) if n7 == n71 && n6 == n61
     => Success(arg |> transpose |> map(map(f)))
    case _ => Failure(mapMapFBeforeTranspose)
  }

  def transposeBeforeMapMapF: Strategy = `T >> **f -> **f >> T`
  def `T >> **f -> **f >> T`: Strategy = {
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

  def slideBeforeMapMapF: Strategy = `S >> **f -> *f >> S`
  def `S >> **f -> *f >> S`: Strategy = {
    case Apply(
    Apply(`map`, Apply(`map`, f)),
    Apply(s, y)) if isSplitOrSlide(s) =>
      Success(y |> map(f) |> s)
    case _ => Failure(slideBeforeMapMapF)
  }

  def mapFBeforeSlide: Strategy = `*f >> S -> S >> **f`
  def `*f >> S -> S >> **f`: Strategy = {
    case Apply(
    s,
    Apply(Apply(`map`, f), y)) if isSplitOrSlide(s) =>
      Success(y |> s |> map(map(f)))
    case _ => Failure(mapFBeforeSlide)
  }

  // join

  def joinBeforeMapF: Strategy = `J >> *f -> **f >> J`
  def `J >> *f -> **f >> J`: Strategy = {
    case Apply(
    Apply(`map`, f),
    Apply(`join`, y)
    ) =>
      Success(y |> map(map(f)) >> join)
    case _ => Failure(joinBeforeMapF)
  }

  def mapMapFBeforeJoin: Strategy = `**f >> J -> J >> *f`
  def `**f >> J -> J >> *f`: Strategy = {
    case Apply(
    `join`,
    Apply(Apply(map, Apply(`map`, f)), y)
    ) =>
      Success(y |> join |> map(f))
    case _ => Failure(mapMapFBeforeJoin)
  }

  // special-cases
  // slide + transpose

  def transposeBeforeSlide: Strategy = `T >> S -> *S >> T >> *T`
  def `T >> S -> *S >> T >> *T`: Strategy = {
    case Apply(
    s,
    Apply(`transpose`, y)
    ) if isSplitOrSlide(s) =>
      Success(y |> map(s) |> transpose >> map(transpose))
    case _ => Failure(transposeBeforeSlide)
  }

  def transposeBeforeMapSlide: Strategy = `T >> *S -> S >> *T >> T`
  def `T >> *S -> S >> *T >> T`: Strategy = {
    case Apply(
    Apply(`map`, s),
    Apply(`transpose`, y)
    ) if isSplitOrSlide(s) =>
      Success(y |> s |> map(transpose) |> transpose)
    case _ => Failure(transposeBeforeMapSlide)
  }

  def mapSlideBeforeTranspose: Strategy = `*S >> T -> T >> S >> *T`
  def `*S >> T -> T >> S >> *T`: Strategy = {
    case Apply(
    `transpose`,
    Apply(Apply(`map`, s), y)
    ) if isSplitOrSlide(s) =>
      Success(y |> transpose >> s >> map(transpose))
    case _ => Failure(mapSlideBeforeTranspose)
  }

  // transpose + join

  def joinBeforeTranspose: Strategy = `J >> T -> *T >> T >> *J`
  def `J >> T -> *T >> T >> *J`: Strategy = {
    case Apply(
    `transpose`,
    Apply(`join`, y)
    ) =>
      Success(y |> map(transpose) |> transpose |> map(join))
    case _ => Failure(joinBeforeTranspose)
  }

  def transposeBeforeMapJoin: Strategy = `T >> *J -> *T >> J >> T`
  def `T >> *J -> *T >> J >> T`: Strategy = {
    case Apply(
    Apply(`map`, `join`),
    Apply(`transpose`, y)
    ) =>
      Success(y |> map(transpose) |> join |> transpose)
    case _ => Failure(transposeBeforeMapJoin)
  }

  def mapTransposeBeforeJoin: Strategy = `*T >> J -> T >> *J >> T`
  def `*T >> J -> T >> *J >> T`: Strategy = {
    case Apply(
    `join`,
    Apply(Apply(`map`, `transpose`), y)
    ) =>
      Success(y |> transpose |> map(join) |> transpose)
    case _ => Failure(mapTransposeBeforeJoin)
  }

  def mapJoinBeforeTranspose: Strategy = `*J >> T -> T >> *T >> J`
  def `*J >> T -> T >> *T >> J`: Strategy = {
    case Apply(
    `transpose`,
    Apply(Apply(`map`, `join`), y)
    ) =>
      Success(y |> transpose |> map(transpose) |> join)
    case _ => Failure(mapJoinBeforeTranspose)
  }

  // join + join

  def joinBeforeJoin: Strategy = `J >> J -> *J >> J`
  def `J >> J -> *J >> J`: Strategy = {
    case Apply(
    `join`,
    Apply(`join`, y)
    ) =>
      Success(y |> map(join) >> join)
    case _ => Failure(joinBeforeJoin)
  }

  def mapJoinBeforeJoin: Strategy = `*J >> J -> J >> J`
  def `*J >> J -> J >> J`: Strategy = {
    case Apply(
    `join`,
    Apply(Apply(`map`, `join`), y)
    ) =>
      Success(y |> join |> join)
    case _ => Failure(mapJoinBeforeJoin)
  }

  // split + slide

  def slideBeforeSplit: Strategy = `slide(n)(s) >> split(k) -> slide(k+n-s)(k) >> map(slide(n)(s))`
  def `slide(n)(s) >> split(k) -> slide(k+n-s)(k) >> map(slide(n)(s))`: Strategy = {
    case Apply(
    DepApply(`split`, k: Nat),
    Apply(DepApply(DepApply(`slide`, n: Nat), s: Nat), y)
    ) =>
      Success(y |> slide(k+n-s)(k) |> map(slide(n)(s)))
    case _ => Failure(slideBeforeSplit)
  }
}
