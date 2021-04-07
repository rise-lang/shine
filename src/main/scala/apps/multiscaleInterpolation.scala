package apps

import localLaplacian.buildPyramid
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
    (4`.`(h+1)`.`(w+1)`.`f32) ->: (4`.`((h/2)+1)`.`((w/2)+1)`.`f32)
  )(input =>
    input |> map( // 4.
      // TODO? Halide only clamps the middle pyramid level, PolyMage pads with 0s
      padClamp2D(
        0, 2*(1 + h/2 - h/^2), // 1 - h % 2
        0, 2*(1 + w/2 - w/^2)  // 1 - w % 2
      ) >>
      map(slide(3)(2)) >> slide(3)(2) >> // H.3.W.3.
      map(transpose) >> // H.W.3.3.
      map(map( // TODO? rewriting could separate the blur
        map(dot(C2D.binomialWeightsH)) >>
        dot(C2D.binomialWeightsV)
      ))
    )
  ))

  val avgWeights: ToBeTyped[Expr] = C2D.weights1d(1.0f / 2.0f, Seq(
    1, 1
  ))

  val upsample: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    (4`.`((h/2)+1)`.`((w/2)+1)`.`f32) ->: (4`.`(h+1)`.`(w+1)`.`f32)
  )(input =>
    input |> map( // 4.
      map(map(fun(x =>
        generate(fun(_ => generate(fun(_ => x)))) :: (2`.`2`.`f32)
      ))) >> // H+1.W+1.2.2.
      map(join >> transpose) >> join >> // 2H+2.2W+2.
      // TODO? Halide does not pad for upsampling, PolyMage pads with 0s
      padClamp2D(
        0, h - 2*(h/2), // h % 2
        0, w - 2*(w/2)  // w % 2
      ) >>
      slide2D(2, 1) >>
      map(map( // TODO? rewriting could separate the blur
        map(dot(avgWeights)) >>
        dot(avgWeights)
      ))
    )
  ))

  val interpolate_level: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    (4`.`h`.`w`.`f32) ->: (4`.`h`.`w`.`f32) ->: (4`.`h`.`w`.`f32)
  )((downsampled, upsampled) =>
      zipND(3)(downsampled, upsampled) |>
      transpose |> map(transpose) |>
      map(map(fun { p => // 4.(f32 x f32)
        val alpha = lf32(1.0f) - fst(p `@` lidx(3, 4))
        map(fun(x =>
          fst(x) + alpha * snd(x)
        ))(p)
      })) |>
      map(transpose) |> transpose
  ))

  val normalize: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    (4`.`h`.`w`.`f32) ->: (3`.`h`.`w`.`f32)
  )(input =>
    input |> transpose |> map(transpose) |> // H.W.4.
    map(map(fun { p =>
      generate(fun(i => p `@` cast(i) / p `@` lidx(3, 4))) :: (3`.`f32)
    })) |> map(transpose) |> transpose
  ))

  def nModFun(m: Nat, f: Nat => ToBeTyped[Expr]): ToBeTyped[Expr] = {
    import arithexpr.arithmetic._
    depFun(RangeAdd(0, PosInf, m), f)
  }

  def interpolate(levels: Int, wMod: Int): ToBeTyped[Expr] =
    depFun((h: Nat) => nModFun(wMod, w => fun(
      (4`.`h`.`w`.`f32) ->: (3`.`h`.`w`.`f32)
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
      buildPyramid(0, levels-1, start, { (i, prev) =>
        val factor = (2: Nat).pow(i - 1)
        downsample(h / factor)(w / factor)(prev)
      }, downsampled =>
      buildPyramid(levels-1, 0, downsampled(levels-1), { (i, prev) =>
        val factor = (2: Nat).pow(i)
        upsample(h / factor)(w / factor)(prev) |> fun(u =>
        interpolate_level(h / factor + 1)(w / factor + 1)(downsampled(i), u))
      }, interpolated =>
      interpolated(0) |>
      map(dropLast(1)) >>
      map(map(dropLast(1))) >>
      normalize(h)(w)
    ))})))

  private val id = fun(x => x)

  object omp { // and plain C
    import rise.openMP.primitives._

    def interpolateNaivePar(levels: Int): ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
      (4`.`h`.`w`.`f32) ->: (3`.`h`.`w`.`f32)
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
      buildPyramid(0, levels-1, start, { (i, prev) =>
        val factor = (2: Nat).pow(i - 1)
        downsample(h / factor)(w / factor)(prev) |> mapSeq(mapPar(mapSeq(id))) |> toMem
      }, downsampled =>
      buildPyramid(levels-1, 0, downsampled(levels-1), { (i, prev) =>
        val factor = (2: Nat).pow(i)
        upsample(h / factor)(w / factor)(prev) |> fun(u =>
        interpolate_level(h / factor + 1)(w / factor + 1)(downsampled(i), u) |>
        mapSeq(mapPar(mapSeq(id))) |> toMem)
      }, interpolated =>
      interpolated(0) |>
      map(dropLast(1)) >>
      map(map(dropLast(1))) >>
      normalize(h)(w) >> mapSeq(mapPar(mapSeq(id)))
    ))}))
  }

}
