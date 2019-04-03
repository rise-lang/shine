package elevate.core.rules

import elevate.core.Strategy
import lift.core._
import lift.core.DSL._
import lift.core.primitives.{map, slide, split, join, transpose}

object algorithmic {
  // - Notation -
  // x >> y: piping operator, x then y
  // *f: map(f)
  // T: transpose
  // S: slide/split
  // J: join

  def splitJoin: Nat => Strategy =
    n => {
      case Apply(`map`, f) => split(n) >> map(map(f)) >> join
    }

  // *g >> *f -> *(g >> f)
  def mapFusion: Strategy = {
    case Apply(Apply(`map`, f), Apply(Apply(`map`, g), arg)) =>
      map(g >> f)(arg)
  }

  // *(g >> f) -> *g >> *f
  def mapFission: Strategy = {
    // TODO? 'x' should not be used in 'f' or 'g'
    case Apply(`map`, Lambda(x1, Apply(f, Apply(g, x2)))) if x1 == x2 =>
      map(g) >> map(f)
  }

  def `**f >> T -> T >> **f`: Strategy = {
    case Apply(`transpose`, Apply(Apply(`map`, Lambda(x1, Apply(Apply(`map`, f), x2))), y)) if x1 == x2 =>
      y |> transpose |> map(map(f))
  }

  def `*f >> S -> S >> **f`: Strategy = {
    case Apply(NatApply(NatApply(`slide`, sz), sp), Apply(Apply(`map`, f), x)) =>
      x |> slide(sz)(sp) |> map(map(f))
    case Apply(NatApply(`split`, n), Apply(Apply(`map`, f), x)) =>
      x |> split(n) |> map(map(f))
  }

  def `S >> **f -> *f >> S`: Strategy = {
    case Apply(Apply(`map`, Apply(`map`, f)), Apply(NatApply(NatApply(`slide`, sz), sp), x)) =>
      x |> map(f) |> slide(sz)(sp)
    case Apply(Apply(`map`, Apply(`map`, f)), Apply(NatApply(`split`, n), x)) =>
      x |> map(f) |> split(n)
  }

  def `*S >> T -> T >> S >> *T`: Strategy = {
    case Apply(`transpose`, Apply(Apply(`map`, NatApply(NatApply(`slide`, sz), sp)), y)) =>
      y |> transpose |> slide(sz)(sp) |> map(transpose)
    case Apply(`transpose`, Apply(Apply(`map`, NatApply(`split`, n)), y)) =>
      y |> transpose |> split(n) |> map(transpose)
  }

  def `S >> *T -> T >> *S >> T`: Strategy = {
    case Apply(Apply(`map`, Lambda(x1, Apply(`transpose`, x2))), Apply(NatApply(NatApply(`slide`, sz), sp), y)) if x1 == x2 =>
      y |> transpose |> map(slide(sz)(sp)) |> transpose
  }

  def ` -> T >> T`: Strategy = x => x |> transpose |> transpose
  def `T >> T -> `: Strategy = {
    case Apply(`transpose`, Apply(`transpose`, x)) => x
  }
}
