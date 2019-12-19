package rise.core

import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.types._
import rise.core.primitives._

object HighLevelConstructs {
  def slide2D(sz: Nat, st: Nat): Expr = slide2D(sz, st, sz, st)
  def slide2D(szOuter: Nat, stOuter: Nat,
              szInner: Nat, stInner: Nat): Expr =
    map(slide(szInner)(stInner)) >> slide(szOuter)(stOuter) >> map(transpose)

  val unslide2D: Expr =
    map(transpose >> map(join)) >> join

  def slide3D(sz: Nat, st: Nat): Expr =
    map(slide2D(sz, st)) >> slide(sz)(st) >> map(transpose >> map(transpose))

  val reorderWithStride: Expr = {
    nFun(s => {
      val f =
        implN(n =>
          fun(IndexType(n))(i => natAsIndex(n)(
            (indexAsNat(i) / (n /^ s)) + ((s: Expr) * (indexAsNat(i) % (n /^ s)))
          )))
      reorder(f)(f)
    })
  }

  def padClamp2D(b: Nat): Expr = padClamp2D(b, b, b, b)
  def padClamp2D(l: Nat, r: Nat): Expr = padClamp2D(l, r, l, r)
  def padClamp2D(lOuter: Nat, rOuter: Nat,
                 lInner: Nat, rInner: Nat): Expr =
    map(padClamp(lInner)(rInner)) >> padClamp(lOuter)(rOuter)

  def padCst2D(n: Nat): Expr = padCst2D(n, n)
  def padCst2D(n: Nat, b: Nat): Expr =
    fun(x => padCst(n)(b)(generate(fun(_ => x))) >> map(padCst(n)(b)(x)))

  def zipND(n: Int): Expr = {
    def rec(n: Int, a: Expr, b: Expr): Expr =
      if (n == 1) {
        zip(a)(b)
      } else if (n > 1) {
        zip(a)(b) |> map(fun(p2 => rec(n - 1, fst(p2), snd(p2))))
      } else {
        pair(a)(b)
      }

    fun(a => fun(b => rec(n, a, b)))
  }

  // TODO: Investigate. this might be wrong
  val partition2D: Expr = {
    import rise.arithmetic.SteppedCase
    nFun(outerSize => nFun(innerSize =>
      map(
        partition(3)(n2nFun(m => SteppedCase(m, Seq(outerSize, innerSize, outerSize))))
      ) >> partition(3)(n2nFun(m => SteppedCase(m, Seq(outerSize, innerSize, outerSize))))
    ))
  }
}
