package rise.core.DSL

import rise.core.DSL.Type._
import rise.core.Expr
import rise.core.primitives._
import rise.core.types._

object HighLevelConstructs {
  def slide2D(sz: Nat, st: Nat): ToBeTyped[Expr] = slide2D(sz, st, sz, st)

  def slide2D(szOuter: Nat, stOuter: Nat, szInner: Nat, stInner: Nat): ToBeTyped[Expr] =
    map(slide(szInner)(stInner)) >> slide(szOuter)(stOuter) >> map(transpose)

  val unslide2D: ToBeTyped[Expr] =
    map(transpose >> map(join)) >> join

  // Halide::TailStrategy::ShiftInwards:
  // Prevent evaluation beyond the original extent by shifting the tail case inwards,
  // re-evaluating some points near the end.
  // WARNING: data-races will happen if processing tiles in parallel
  def tileShiftInwards(tileSize: Nat): ToBeTyped[Expr] =
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
          val startShifted = l(n - tileSize)
          val start = select(tile < endRegularTile)(startRegular)(startShifted)
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
        val pos = select(tile < endRegularTile)(regularPos)(shiftedPos)
        natAsIndex(n)(pos)
      })))
    } :: (n `.` t)))}}}}

  def tileEpilogue(tileSize: Nat): ToBeTyped[Expr] =
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

  def slide3D(sz: Nat, st: Nat): ToBeTyped[Expr] =
    map(slide2D(sz, st)) >> slide(sz)(st) >> map(transpose >> map(transpose))

  val reorderWithStride: ToBeTyped[Expr] = {
    depFun((s: Nat) => {
      impl { n: Nat =>
        def f = n2nFun(n)(i => (i / (n /^ s)) + (s * (i % (n /^ s))))
        reorder(n)(f)(f)
      }
    })
  }

  // NOTE: this looks wrong to me, but comes from original Lift
  def scatterShift: ToBeTyped[Expr] = depFun((s: Nat) =>
    impl { n: Nat => impl { dt: DataType =>
      scatter(generate(fun(i =>
        natAsIndex(n)((indexAsNat(i) + l(s)) - l(n)*(indexAsNat(i) / l(n-1)))
      ))) :: (n`.`dt) ->: (n`.`dt)
    }}
  )

  def padClamp2D(b: Nat): ToBeTyped[Expr] = padClamp2D(b, b, b, b)

  def padClamp2D(l: Nat, r: Nat): ToBeTyped[Expr] = padClamp2D(l, r, l, r)

  def padClamp2D(lOuter: Nat, rOuter: Nat, lInner: Nat, rInner: Nat): ToBeTyped[Expr] =
    padClampND(Seq((lOuter, rOuter), (lInner, rInner)))

  def padClamp3D(b: Nat): ToBeTyped[Expr] = padClampND(Seq((b, b), (b, b), (b, b)))

  def padClampND(lrs: Seq[(Nat, Nat)]): ToBeTyped[Expr] =
    lrs match {
      case Nil => fun(x => x)
      case (l, r) +: Nil => padClamp(l)(r)
      case (l, r) +: inner => map(padClampND(inner)) >> padClamp(l)(r)
    }

  def padCst2D(n: Nat): ToBeTyped[Expr] = padCst2D(n, n)

  def padCst2D(l: Nat, r: Nat): ToBeTyped[Expr] = padCst2D(l, r, l, r)

  def padCst2D(lOuter: Nat, rOuter: Nat, lInner: Nat, rInner: Nat): ToBeTyped[Expr] =
    fun(x => padCst(lOuter)(rOuter)(generate(fun(_ => x))) >> map(padCst(lInner)(rInner)(x)))

  def zipND(n: Int): ToBeTyped[Expr] = {
    def rec(n: Int, a: ToBeTyped[Expr], b: ToBeTyped[Expr]): ToBeTyped[Expr] =
      if (n == 1) {
        zip(a)(b)
      } else if (n > 1) {
        zip(a)(b) |> map(fun(p2 => rec(n - 1, fst(p2), snd(p2))))
      } else {
        makePair(a)(b)
      }

    fun(a => fun(b => rec(n, a, b)))
  }

  def mapND(n: Int): ToBeTyped[Expr] = {
    def rec(n: Int, e: ToBeTyped[Expr]): ToBeTyped[Expr] =
      if (n > 0) {
        map(rec(n - 1, e))
      } else {
        e
      }

    fun(e => rec(n, e))
  }

  def dropLast: ToBeTyped[Expr] = depFun((n: Nat) => impl { m: Nat =>
    impl { dt: DataType =>
      take(m) :: ((m + n) `.` dt) ->: (m `.` dt)
    }
  }
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
