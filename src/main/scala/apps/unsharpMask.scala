package apps

import rise.core.DSL.HighLevelConstructs._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.primitives._
import rise.core.types._

// in Halide: https://github.com/halide/Halide/blob/e8acdea/apps/unsharp
// in PolyMage: https://bitbucket.org/udayb/polymage/src/e28327c/sandbox/apps/python/img_proc/unsharp_mask
// FIXME: PolyMage's algorithm is different
object unsharpMask {
  private val C2D = separableConvolution2D
  private val dot = C2D.dot

  val exp: ToBeTyped[Expr] = foreignFun("exp", f32 ->: f32)

  private def larr_f32(s: Seq[Float]): ToBeTyped[Expr] = {
    larr(s.map(semantics.FloatData))
  }

  val kPi = 3.14159265358979310000f;
  val kernel: ToBeTyped[Expr] =
    fun(f32)(sigma =>
    generate(fun(x_ => {
      val x = cast(x_) :: f32
      exp(-x * x / (l(2.0f) * sigma * sigma)) / (l(Math.sqrt(2.0f * kPi).toFloat) * sigma)
    }))
    )

  val gray: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    (3`.`h`.`w`.`f32) ->: (h`.`w`.`f32)
  )(input => input |>
    transpose >> map(transpose) >>
    map(map(dot(larr_f32(Seq(0.299f, 0.587f, 0.114f)))))
  ))

  val blurNbh: ToBeTyped[Expr] =
    fun(4`.`f32)(kernel =>
    fun((7`.`f32) ->: f32)(neighborhood => {
      def k(i: Int) = kernel `@` lidx(i, 4)
      def nbh(i: Int) = neighborhood `@` lidx(i, 7)
      k(0) * nbh(3) +
      k(1) * (nbh(2) + nbh(4)) +
      k(2) * (nbh(1) + nbh(5)) +
      k(3) * (nbh(0) + nbh(6))
    }))

  val blur: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    (4`.`f32) ->: ((h+6)`.`(w+6)`.`f32) ->: (h`.`w`.`f32)
  )((kernel, input) => input |>
    slide2D(7, 1) >>
    map(map( // TODO? rewriting could separate the blur
      // blurY
      map(blurNbh(kernel)) >>
      // blurX
      blurNbh(kernel)
    ))
  ))

  val sharpen: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32)
  )((gray, blur) =>
    zipND(2)(gray, blur) |>
    map(map(fun(p =>
      l(2.0f) * fst(p) - snd(p)
    )))
  ))

  val ratio: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32)
  )((a, b) =>
    zipND(2)(a, b) |>
    map(map(fun(p => fst(p) / snd(p))))
  ))

  val mul = depFun((h: Nat, w: Nat) => fun(
    (h`.`w`.`f32) ->: (3`.`h`.`w`.`f32) ->: (3`.`h`.`w`.`f32)
  )((a, b) =>
    zipND(2)(a, b |> transpose |> map(transpose)) |>
    map(map(fun(p =>
      map(fun(x => fst(p) * x))(snd(p))
    ))) |> map(transpose) |> transpose
  ))

  def nModFun(m: Nat, f: Nat => ToBeTyped[Expr]): ToBeTyped[Expr] = {
    import arithexpr.arithmetic._
    depFun(RangeAdd(0, PosInf, m), f)
  }

  def unsharp(wMod: Int): ToBeTyped[Expr] =
    depFun((h: Nat) => nModFun(wMod, w => fun(
      f32 ->: (3`.`h`.`w`.`f32) ->: (3`.`h`.`w`.`f32)
    )((sigma, input) =>
      gray(h)(w)(input) |> fun(g =>
      kernel(sigma) |> fun(k =>
      blur(h)(w)(k)(g |> padClamp2D(3)) |> fun(b =>
      sharpen(h)(w)(g)(b) |> fun(s =>
      ratio(h)(w)(s)(g) |> fun(r =>
      mul(h)(w)(r)(input)
      )))))
    )))

  private val id = fun(x => x)

  object omp { // and plain C
    import rise.openMP.primitives._

    val unsharpNaivePar: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
      f32 ->: (3`.`h`.`w`.`f32) ->: (3`.`h`.`w`.`f32)
    )((sigma, input) =>
      kernel(sigma) |> mapSeq(id) |> toMem |> letf(fun(k =>
      gray(h)(w)(input) |> mapPar(mapSeq(id)) |> toMem |> letf(fun(g =>
      blur(h)(w)(k)(g |> padClamp2D(3)) |> fun(b =>
      sharpen(h)(w)(g)(b) |> fun(s =>
      ratio(h)(w)(s)(g) |> mapPar(mapSeq(id)) |> toMem |> letf(fun(r =>
      mul(h)(w)(r)(input) |> mapSeq(mapPar(mapSeq(id)))
      ))))))))
    ))
  }
}
