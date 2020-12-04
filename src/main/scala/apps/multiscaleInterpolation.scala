package apps

import rise.core.DSL.HighLevelConstructs._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.primitives._
import rise.core.types._

// in Halide: https://github.com/halide/Halide/blob/e8acdea/apps/interpolate
// in PolyMage: https://bitbucket.org/udayb/polymage/src/e28327c/sandbox/apps/python/img_proc/interpolate
object multiscaleInterpolation {
  private val C2D = separableConvolution2D
  private val dot = C2D.dot

  val downsample: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    (4`.`((2*h)+1)`.`((2*w)+1)`.`f32) ->: (4`.`(h+1)`.`(w+1)`.`f32)
  )(input =>
    input |> map( // 4.
      padClamp2D(1) >> // TODO: should this be a clamp?
      map(slide(3)(2)) >> slide(3)(2) >> // H.3.W.3.
      map(transpose) >> // H.W.3.3.
      map(map( // TODO? rewriting could separate the blur
        transpose >>
        map(dot(C2D.binomialWeightsH)) >>
        dot(C2D.binomialWeightsV)
      ))
    )
  ))

  val avgWeights: ToBeTyped[Expr] = C2D.weights1d(1.0f / 2.0f, Seq(
    1, 1
  ))

  val upsample: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    (4`.`(h+1)`.`(w+1)`.`f32) ->: (4`.`((2*h)+1)`.`((2*w)+1)`.`f32)
  )(input =>
    input |> map( // 4.
      map(map(fun(x =>
        generate(fun(_ => generate(fun(_ => x)))) :: (2`.`2`.`f32)
      ))) >> // H+1.W+1.2.2.
      map(join >> transpose) >> join >> // 2H+2.2W+2.
      slide2D(2, 1) >>
      map(map(
        transpose >>
        map(dot(avgWeights)) >>
        dot(avgWeights)
      ))
    )
  ))

  val normalize: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    (4`.`h`.`w`.`f32) ->: (4`.`h`.`w`.`f32)
  )(input =>
    input |> transpose |> map(transpose) |> // H.W.4.
    map(map(fun { p =>
      generate(fun(i => p `@` i / p `@` lidx(3, 4))) :: (4`.`f32)
    })) |> map(transpose) |> transpose
  ))

  def nModFun(m: Nat, f: Nat => ToBeTyped[Expr]): ToBeTyped[Expr] = {
    import arithexpr.arithmetic._
    depFun(RangeAdd(0, PosInf, m), f)
  }

  def multiscaleInterpolation(levels: Int, wMod: Int): ToBeTyped[Expr] =
    depFun((h: Nat) => nModFun(wMod, w => fun(
      (4`.`h`.`w`.`f32) ->: (4`.`h`.`w`.`f32)
    )(input => {
      val alpha = lidx(3, 4)
      val start = generate(fun(i =>
        select(i < alpha)(
          zipND(2)(input `@` i, input `@` alpha) |>
          map(map(fun(p => fst(p) * snd(p))))
        )(input `@` alpha)
      )) |> map(padClamp2D(1))
      val downsampled = (1 to levels).foldRight[ToBeTyped[Expr]](start){ (_, prev) =>
        impl { hp: Nat => impl { wp: Nat => downsample(hp)(wp)(prev) }}
      }
      val upsampled = (1 to levels).foldRight[ToBeTyped[Expr]](downsampled){ case (_, prev) =>
        impl { hp: Nat => impl { wp: Nat => upsample(hp)(wp)(prev) }}
      }
      upsampled |>
      map(drop(1) >> dropLast(1)) >>
      map(map(drop(1) >> dropLast(1))) >>
      normalize(h)(w)
    })))

  private val id = fun(x => x)

  object omp { // and plain C
    import rise.openMP.primitives._

    def multiscaleInterpolationNaivePar(levels: Int): ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
      (4`.`h`.`w`.`f32) ->: (4`.`h`.`w`.`f32)
    )(input => {
      val alpha = lidx(3, 4)
      val start = generate(fun(i =>
        select(i < alpha)(
          zipND(2)(input `@` i, input `@` alpha) |>
          map(map(fun(p => fst(p) * snd(p))))
        )(input `@` alpha)
      )) |> map(padClamp2D(1))
      val downsampled = (1 to levels).foldRight[ToBeTyped[Expr]](start) { (_, prev) =>
        impl { hp: Nat => impl { wp: Nat =>
          downsample(hp)(wp)(prev) |> mapSeq(mapPar(mapSeq(id))) |> toMem
        }}
      }
      val upsampled = (1 to levels).foldRight[ToBeTyped[Expr]](downsampled) { case (_, prev) =>
        impl { hp: Nat => impl { wp: Nat =>
          upsample(hp)(wp)(prev) |> mapSeq(mapPar(mapSeq(id))) |> toMem
        }}
      }
      upsampled |>
      map(drop(1) >> dropLast(1)) >>
      map(map(drop(1) >> dropLast(1))) >>
      normalize(h)(w) >> mapSeq(mapPar(mapSeq(id)))
    }))
  }

}
