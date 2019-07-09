package elevate.core.rules

import elevate.core.{NotApplicable, Strategy}
import lift.core._
import lift.core.DSL._
import lift.core.primitives.{join, map, slide, split, transpose}

object algorithmic {
  // - Notation -
  // x >> y: piping operator, x then y
  // *f: map(f)
  // T: transpose
  // S: slide/split
  // J: join

  def splitJoin: Nat => Strategy =
    n => Rule({
      case Apply(`map`, f) => split(n) >> map(map(f)) >> join
    })

  // *g >> *f -> *(g >> f)
  def mapFusion: Strategy = Rule({
    case Apply(Apply(`map`, f), Apply(Apply(`map`, g), arg)) =>
      map(g >> f)(arg)
    case _ => throw NotApplicable(mapFusion)
  })

  // fission of the last function to be applied inside a map
  // *(g >> .. >> f) -> *(g >> ..) >> *f
  def mapLastFission: Strategy = Rule({
    // TODO? 'x' should not be used in 'f' or 'g'
    /* chain of two fission
    case Apply(`map`, Lambda(x1, Apply(f, Apply(g, x2)))) if x1 == x2 =>
      map(g) >> map(f)
      */
    case Apply(`map`, Lambda(x, Apply(f, gx))) =>
      Apply(`map`, Lambda(x, gx)) >> map(f)
  })

  def `**f >> T -> T >> **f`: Strategy = Rule({
    case Apply(`transpose`, Apply(Apply(`map`, Lambda(x1, Apply(Apply(`map`, f), x2))), y)) if x1 == x2 =>
      y |> transpose |> map(map(f))
  })

  def `*f >> S -> S >> **f`: Strategy = Rule({
    case Apply(DepApply(DepApply(`slide`, sz: Nat), sp: Nat), Apply(Apply(`map`, f), x)) =>
      x |> slide(sz)(sp) |> map(map(f))
    case Apply(DepApply(`split`, n: Nat), Apply(Apply(`map`, f), x)) =>
      x |> split(n) |> map(map(f))
  })

  def `S >> **f -> *f >> S`: Strategy = Rule({
    case Apply(Apply(`map`, Apply(`map`, f)), Apply(DepApply(DepApply(`slide`, sz: Nat), sp: Nat), x)) =>
      x |> map(f) |> slide(sz)(sp)
    case Apply(Apply(`map`, Apply(`map`, f)), Apply(DepApply(`split`, n: Nat), x)) =>
      x |> map(f) |> split(n)
  })

  def `*S >> T -> T >> S >> *T`: Strategy = Rule({
    case Apply(`transpose`, Apply(Apply(`map`, DepApply(DepApply(`slide`, sz: Nat), sp: Nat)), y)) =>
      y |> transpose |> slide(sz)(sp) |> map(transpose)
    case Apply(`transpose`, Apply(Apply(`map`, DepApply(`split`, n: Nat)), y)) =>
      y |> transpose |> split(n) |> map(transpose)
  })

  def `S >> *T -> T >> *S >> T`: Strategy = Rule({
    case Apply(Apply(`map`, Lambda(x1, Apply(`transpose`, x2))),
          Apply(DepApply(DepApply(`slide`, sz: Nat), sp: Nat), y)) if x1 == x2 =>
      y |> transpose |> map(slide(sz)(sp)) |> transpose
  })

  def ` -> T >> T`: Strategy = x => x |> transpose |> transpose
  def `T >> T -> `: Strategy = Rule({
    case Apply(`transpose`, Apply(`transpose`, x)) => x
  })
}
