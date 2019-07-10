package elevate.core.rules

import elevate.core.{NotApplicable, Strategy}
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

  def transposeOverMapMapF: Strategy = `**f >> T -> T >> **f`
  def `**f >> T -> T >> **f`: Strategy = {
    case Apply(
    `transpose`,
    Apply(Apply(`map`, Apply(`map`, f)), y)) =>
      y |> transpose |> map(map(f))
    case _ => throw NotApplicable(transposeOverMapMapF)
  }

  def mapMapFOverTranspose: Strategy = `T >> **f -> **f >> T`
  def `T >> **f -> **f >> T`: Strategy = {
    case Apply(
    Apply(`map`, Apply(`map`, f)),
    Apply(`transpose`, y)) =>
      y |> map(map(f)) |> transpose
    case _ => throw NotApplicable(mapMapFOverTranspose)
  }

  // split/slide

  private def isSplitOrSlide(s: Expr): Boolean = s match {
    case DepApply(DepApply(`slide`, _: Nat), _: Nat) => true
    case DepApply(`split`, _: Nat) => true
    case _ => false
  }

  def mapMapFOverSlide: Strategy = `S >> **f -> *f >> S`
  def `S >> **f -> *f >> S`: Strategy = {
    case Apply(
    Apply(`map`, Apply(`map`, f)),
    Apply(s, y)) if isSplitOrSlide(s) =>
      y |> map(f) |> s
    case _ => throw NotApplicable(mapMapFOverSlide)
  }

  def slideOverMapF: Strategy = `*f >> S -> S >> **f`
  def `*f >> S -> S >> **f`: Strategy = {
    case Apply(
    s,
    Apply(Apply(`map`, f), y)) if isSplitOrSlide(s) =>
      y |> s |> map(map(f))
    case _ => throw NotApplicable(slideOverMapF)
  }

  // join

  def mapFOverJoin: Strategy = `J >> *f -> **f >> J`
  def `J >> *f -> **f >> J`: Strategy = {
    case Apply(
    Apply(`map`, f),
    Apply(`join`, y)
    ) =>
      y |> map(map(f)) >> join
    case _ => throw NotApplicable(mapFOverJoin)
  }

  def joinOverMapMapF: Strategy = `**f >> J -> J >> *f`
  def `**f >> J -> J >> *f`: Strategy = {
    case Apply(
    `join`,
    Apply(Apply(map, Apply(`map`, f)), y)
    ) =>
      y |> join |> map(f)
    case _ => throw NotApplicable(joinOverMapMapF)
  }

  // special-cases
  // slide + transpose

  def slideOverTranspose: Strategy = `T >> S -> *S >> T >> *T`
  def `T >> S -> *S >> T >> *T`: Strategy = {
    case Apply(
    s,
    Apply(`transpose`, y)
    ) if isSplitOrSlide(s) =>
      y |> map(s) |> transpose >> map(transpose)
    case _ => throw NotApplicable(slideOverTranspose)
  }

  def mapSlideOverTranspose: Strategy = `T >> *S -> S >> *T >> T`
  def `T >> *S -> S >> *T >> T`: Strategy = {
    case Apply(
    Apply(`map`, s),
    Apply(`transpose`, y)
    ) if isSplitOrSlide(s) =>
      y |> s |> map(transpose) |> transpose
    case _ => throw NotApplicable(mapSlideOverTranspose)
  }

  def transposeOverMapSlide: Strategy = `*S >> T -> T >> S >> *T`
  def `*S >> T -> T >> S >> *T`: Strategy = {
    case Apply(
    `transpose`,
    Apply(Apply(`map`, s), y)
    ) if isSplitOrSlide(s) =>
      y |> transpose >> s >> map(transpose)
    case _ => throw NotApplicable(transposeOverMapSlide)
  }

  // transpose + join

  def transposeOverJoin: Strategy = `J >> T -> *T >> T >> *J`
  def `J >> T -> *T >> T >> *J`: Strategy = {
    case Apply(
    `transpose`,
    Apply(`join`, y)
    ) =>
      y |> map(transpose) |> transpose |> map(join)
    case _ => throw NotApplicable(transposeOverJoin)
  }

  def mapJoinOverTranspose: Strategy = `T >> *J -> *T >> J >> T`
  def `T >> *J -> *T >> J >> T`: Strategy = {
    case Apply(
    Apply(`map`, `join`),
    Apply(`transpose`, y)
    ) =>
      y |> map(transpose) |> join |> transpose
    case _ => throw NotApplicable(mapJoinOverTranspose)
  }

  def joinOverMapTranspose: Strategy = `*T >> J -> T >> *J >> T`
  def `*T >> J -> T >> *J >> T`: Strategy = {
    case Apply(
    `join`,
    Apply(Apply(`map`, `transpose`), y)
    ) =>
      y |> transpose |> map(join) |> transpose
    case _ => throw NotApplicable(joinOverMapTranspose)
  }

  def transposeOverMapJoin: Strategy = `*J >> T -> T >> *T >> J`
  def `*J >> T -> T >> *T >> J`: Strategy = {
    case Apply(
    `transpose`,
    Apply(Apply(`map`, `join`), y)
    ) =>
      y |> transpose |> map(transpose) |> join
    case _ => throw NotApplicable(transposeOverMapJoin)
  }

  // join + join

  def joinOverJoin: Strategy = `J >> J -> *J >> J`
  def `J >> J -> *J >> J`: Strategy = {
    case Apply(
    `join`,
    Apply(`join`, y)
    ) =>
      y |> map(join) >> join
    case _ => throw NotApplicable(joinOverJoin)
  }

  def joinOverMapJoin: Strategy = `*J >> J -> J >> J`
  def `*J >> J -> J >> J`: Strategy = {
    case Apply(
    `join`,
    Apply(Apply(`map`, `join`), y)
    ) =>
      y |> join |> join
    case _ => throw NotApplicable(joinOverMapJoin)
  }

  // split + slide

  def slideOverSplit: Strategy = `slide(n)(s) >> split(k) -> slide(k+n-s)(k) >> map(slide(n)(s))`
  def `slide(n)(s) >> split(k) -> slide(k+n-s)(k) >> map(slide(n)(s))`: Strategy = {
    case Apply(
    DepApply(`split`, k: Nat),
    Apply(DepApply(DepApply(`slide`, n: Nat), s: Nat), y)
    ) =>
      y |> slide(k+n-s)(k) |> map(slide(n)(s))
    case _ => throw NotApplicable(slideOverSplit)
  }
}
