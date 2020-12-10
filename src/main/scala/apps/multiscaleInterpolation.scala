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

  def interpolate(levels: Int, wMod: Int): ToBeTyped[Expr] =
    depFun((h: Nat) => nModFun(wMod, w => fun(
      (4`.`h`.`w`.`f32) ->: (4`.`h`.`w`.`f32)
    )(input => {
      val alpha = lidx(3, 4)
      val start = map(padClamp2D(0, 1))(input) |> fun(clamped =>
        generate(fun(i =>
          select(i < alpha)(
            zipND(2)(clamped `@` i, clamped `@` alpha) |>
              map(map(fun(p => fst(p) * snd(p))))
          )(clamped `@` alpha)
        ))
      )
      val downsampled = (1 until levels).foldRight[ToBeTyped[Expr]](start){ (_, prev) =>
        impl { hp: Nat => impl { wp: Nat => downsample(hp)(wp)(prev) }}
      }
      val upsampled = (1 until levels).foldRight[ToBeTyped[Expr]](downsampled){ case (_, prev) =>
        impl { hp: Nat => impl { wp: Nat => upsample(hp)(wp)(prev) }}
      }
      upsampled |>
      map(dropLast(1)) >>
      map(map(dropLast(1))) >>
      normalize(h)(w)
    })))

  private val id = fun(x => x)

  object omp { // and plain C
    import rise.openMP.primitives._

    def interpolateNaivePar(levels: Int): ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
      (4`.`h`.`w`.`f32) ->: (4`.`h`.`w`.`f32)
    )(input => {
      val alpha = lidx(3, 4)
      val start = map(padClamp2D(0, 1))(input) |> fun(clamped =>
          generate(fun(i =>
          select(i < alpha)(
            zipND(2)(clamped `@` i, clamped `@` alpha) |>
            map(map(fun(p => fst(p) * snd(p))))
          )(clamped `@` alpha)
        ))
      )
      val downsampled = (1 until levels).foldLeft[ToBeTyped[Expr]](start) { (prev, i) =>
        val factor = (2: Nat).pow(i)
        downsample(h /^ factor)(w /^ factor)(prev) |> mapSeq(mapPar(mapSeq(id))) |> toMem
      }
      val upsampled = (1 until levels).foldRight[ToBeTyped[Expr]](downsampled) { case (i, prev) =>
        val factor = (2: Nat).pow(i)
        upsample(h /^ factor)(w /^ factor)(prev) |> mapSeq(mapPar(mapSeq(id))) |> toMem
      }
      upsampled |>
      map(dropLast(1)) >>
      map(map(dropLast(1))) >>
      normalize(h)(w) >> mapSeq(mapPar(mapSeq(id)))
    }))
  }

}
