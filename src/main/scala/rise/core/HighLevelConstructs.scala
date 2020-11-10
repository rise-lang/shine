package rise.core

import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.types._

object HighLevelConstructs {
  def slide2D(sz: Nat, st: Nat): Expr = slide2D(sz, st, sz, st)
  def slide2D(szOuter: Nat, stOuter: Nat, szInner: Nat, stInner: Nat): Expr =
    map(slide(szInner)(stInner)) >> slide(szOuter)(stOuter) >> map(transpose)

  val unslide2D: Expr =
    map(transpose >> map(join)) >> join

  // Halide::TailStrategy::ShiftInwards:
  // Prevent evaluation beyond the original extent by shifting the tail case inwards,
  // re-evaluating some points near the end.
  // WARNING: data-races will happen if processing tiles in parallel
  def tileShiftInwards(tileSize: Nat): Expr =
    impl{ n: Nat => impl{ haloSize: Nat =>
    impl{ s: DataType => impl{ t: DataType =>
    fun(processTiles =>
    fun(input => {
      val tiles = (n + tileSize - 1) / tileSize
      generate(fun(tile =>
        (input :: ((n + haloSize) `.` s)) |>
        gather(generate(fun(i => {
          val endRegularTile = lidx(n / tileSize, tiles)
          val startRegular = indexAsNat(tile) * tileSize
          val startShifted = n - tileSize
          val start = select(tile < endRegularTile, startRegular, startShifted)
          natAsIndex(n + haloSize)(start + indexAsNat(i))
        })))
      )) |>
      (processTiles ::
        (tiles `.` (tileSize + haloSize) `.` s) ->: (tiles `.` tileSize `.` t)) >>
      join >>
      scatter(generate(fun(i => {
        val tile = indexAsNat(i) / tileSize
        val endRegularTile = n / tileSize
        val regularPos = indexAsNat(i)
        val recomputed = tileSize - (n % tileSize)
        val shiftedPos = indexAsNat(i) - recomputed
        val pos = select(tile < endRegularTile, regularPos, shiftedPos)
        natAsIndex(n)(pos)
      })))
    } :: (n `.` t)))}}}}

  def tileEpilogue(tileSize: Nat): Expr =
    impl{ n: Nat => impl{ haloSize: Nat =>
    impl{ s: DataType => impl{ t: DataType =>
    fun(f => fun(g => fun(a =>
      (concat(
        (a :: ((n + haloSize) `.` s)) |>
        take((n / tileSize) * tileSize + haloSize) |>
        slide(tileSize + haloSize)(tileSize) |>
        impl{ sza: Nat => f ::
          (sza`.`(tileSize + haloSize)`.`s) ->: (sza`.`tileSize`.`t) } |>
        join
      )(
        (a :: ((n + haloSize) `.` s)) |>
        drop((n / tileSize) * tileSize) |>
        slide(n % tileSize + haloSize)(n % tileSize) |>
        impl{ szb: Nat => g ::
          (szb`.`(n % tileSize + haloSize)`.`s) ->: (szb`.`(n % tileSize)`.`t) } |>
        join
      )) :: (n`.`t)
    )))}}}}

  def slide3D(sz: Nat, st: Nat): Expr =
    map(slide2D(sz, st)) >> slide(sz)(st) >> map(transpose >> map(transpose))

  val reorderWithStride: Expr = {
    nFun(s => {
      val f =
        impl{ n: Nat =>
          fun(IndexType(n))(i =>
            natAsIndex(n)(
              (indexAsNat(i) / (n /^ s)) +
                ((s: Expr) * (indexAsNat(i) % (n /^ s)))
            )
          )
        }
      reorder(f)(f)
    })
  }

  def padClamp2D(b: Nat): Expr = padClamp2D(b, b, b, b)
  def padClamp2D(l: Nat, r: Nat): Expr = padClamp2D(l, r, l, r)
  def padClamp2D(lOuter: Nat, rOuter: Nat, lInner: Nat, rInner: Nat): Expr =
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

  def dropLast: Expr = nFun(n => impl{ m: Nat => impl{ dt: DataType =>
    take(m) :: ((m + n) `.` dt) ->: (m `.` dt) }}
  )

  // TODO: Investigate. this might be wrong
  val partition2D: Expr = {
    import arithexpr.arithmetic.SteppedCase
    nFun(outerSize =>
      nFun(innerSize =>
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

  def slideVectors(n: Nat): Expr =
    slide(n)(1) >> join >> asVector(n)
}
