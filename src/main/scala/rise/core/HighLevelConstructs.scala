package rise.core

import rise.core.TypeLevelDSL._
import rise.core.primitives._
import rise.core.TypedDSL._
import rise.core.types._

object HighLevelConstructs {
  def slide2D(sz: Nat, st: Nat): ToBeTyped[Expr] = slide2D(sz, st, sz, st)
  def slide2D(szOuter: Nat, stOuter: Nat, szInner: Nat, stInner: Nat): ToBeTyped[Expr] =
    map(slide(szInner)(stInner)) >> slide(szOuter)(stOuter) >> map(transpose)

  val unslide2D: ToBeTyped[Expr] =
    map(transpose >> map(join)) >> join

  def slide3D(sz: Nat, st: Nat): ToBeTyped[Expr] =
    map(slide2D(sz, st)) >> slide(sz)(st) >> map(transpose >> map(transpose))

  val reorderWithStride: ToBeTyped[Expr] = {
    depFun((s: Nat) => {
      val f =
        impl{ n: Nat =>
          fun(IndexType(n))(i =>
            natAsIndex(n)(
              (indexAsNat(i) / (n /^ s)) +
                ((s: ToBeTyped[Expr]) * (indexAsNat(i) % (n /^ s)))
            )
          )
        }
      reorder(f)(f)
    })
  }

  def padClamp2D(b: Nat): ToBeTyped[Expr] = padClamp2D(b, b, b, b)
  def padClamp2D(l: Nat, r: Nat): ToBeTyped[Expr] = padClamp2D(l, r, l, r)
  def padClamp2D(lOuter: Nat, rOuter: Nat, lInner: Nat, rInner: Nat): ToBeTyped[Expr] =
    map(padClamp(lInner)(rInner)) >> padClamp(lOuter)(rOuter)

  def padCst2D(n: Nat): ToBeTyped[Expr] = padCst2D(n, n)
  def padCst2D(n: Nat, b: Nat): ToBeTyped[Expr] =
    fun(x => padCst(n)(b)(generate(fun(_ => x))) >> map(padCst(n)(b)(x)))

  def zipND(n: Int): ToBeTyped[Expr] = {
    def rec(n: Int, a: ToBeTyped[Expr], b: ToBeTyped[Expr]): ToBeTyped[Expr] =
      if (n == 1) {
        zip(a)(b)
      } else if (n > 1) {
        zip(a)(b) |> map(fun(p2 => rec(n - 1, fst(p2), snd(p2))))
      } else {
        pair(a)(b)
      }

    fun(a => fun(b => rec(n, a, b)))
  }

  def dropLast: ToBeTyped[Expr] = depFun((n: Nat) => impl{ m: Nat => impl{ dt: DataType =>
    take(m) :: ((m + n) `.` dt) ->: (m `.` dt) }}
  )

  // TODO: Investigate. this might be wrong
  val partition2D: ToBeTyped[Expr] = {
    import arithexpr.arithmetic.SteppedCase
    depFun((outerSize: Nat) =>
      depFun((innerSize: Nat) =>
        map(
          partition(3)(
            n2nFun(m => SteppedCase(m, Seq(outerSize, innerSize, outerSize)))
          )
        ) >> partition(3)(
          n2nFun(m => SteppedCase(m, Seq(outerSize, innerSize, outerSize)))
        )
      )
    )
  }

  def slideVectors(n: Nat): ToBeTyped[Expr] =
    slide(n)(1) >> join >> asVector(n)
}
