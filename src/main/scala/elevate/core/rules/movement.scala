package elevate.core.rules

import elevate.core.Strategy
import lift.core.{Apply, DepApply, Expr, Lambda, Nat, Primitive}
import lift.core.primitives._
import lift.core.DSL._

/// Describing possible movements between pairs of primitives //////////////////////////////////////////////////////////

object movement {
  // - Notation -
  // x >> y: piping operator, x then y
  // *f: map(f)
  // T: transpose
  // S: slide/split
  // J: join

  /// transpose ////////////////////////////////////////////////////////////////////////////////////////////////////////

  def `**f >> T -> T >> **f`: Strategy = {
      case Apply(
        `transpose`,
        Apply(Apply(`map`, Apply(`map`, f)), y)) =>
        y |> transpose |> map(map(f))
  }

  def `T >> **f -> **f >> T`: Strategy = {
    case Apply(
      Apply(`map`, Apply(`map`, f)),
      Apply(`transpose`, y)) =>
      y |> map(map(f)) |> transpose
  }

  /// split/slide //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def isSplitOrSlide(s: Expr): Boolean = s match {
    case DepApply(DepApply(`slide`, _: Nat), _: Nat) => true
    case DepApply(`split`, _: Nat) => true
    case _ => false
  }

  def `S >> **f -> *f >> S`: Strategy = {
    case Apply(
      Apply(`map`, Apply(`map`, f)),
      Apply(s, y)) if isSplitOrSlide(s) =>
      y |> map(f) |> s
  }

  def `*f >> S -> S >> **f`: Strategy = {
    case Apply(
      s,
      Apply(Apply(`map`, f), y)) if isSplitOrSlide(s) =>
      y |> s |> map(map(f))
  }

}
