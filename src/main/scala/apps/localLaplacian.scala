package apps

import rise.core.DSL.HighLevelConstructs._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.{DSL, _}
import rise.core.primitives._
import rise.core.types._

// in Halide: https://github.com/halide/Halide/blob/e8acdea/apps/local_laplacian
// in PolyMage: https://bitbucket.org/udayb/polymage/src/e28327c/sandbox/apps/python/img_proc/local_laplacian
// TODO: check that PolyMage's algorithm is the same
object localLaplacian {
  private val C2D = separableConvolution2D
  private val dot = C2D.dot

  val exp: ToBeTyped[Expr] = foreignFun("exp", f32 ->: f32)
  def clamp(dt: DataType): ToBeTyped[Expr] =
    foreignFun(s"clamp_${dt}", dt ->: dt ->: dt ->: dt)

  val downsampleWeights: ToBeTyped[Expr] = C2D.weights1d(1.0f / 8.0f, Seq(
    1, 3, 3, 1
  ))
  val downsample2D: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    (((2*h)+3)`.`((2*w)+3)`.`f32) ->: ((h+3)`.`(w+3)`.`f32)
  )(input => input |>
    // 2x (2, 1)
    padClamp2D(3, 2) >> // TODO: should this be a clamp?
    map(slide(4)(2)) >> slide(4)(2) >> // H.4.W.4.
    map(transpose) >> // H.W.4.4.
    map(map( // TODO? rewriting could separate the blur
      transpose >>
      map(dot(downsampleWeights)) >>
      dot(downsampleWeights)
    ))
  ))

  val downsample3D: ToBeTyped[Expr] = depFun((h: Nat, w: Nat, k: Nat) => fun(
    (((2*h)+3)`.`((2*w)+3)`.`k`.`f32) ->: ((h+3)`.`(w+3)`.`k`.`f32)
  )(input =>
    input |> map(transpose) |> transpose |> map(
      downsample2D(h)(w)
    ) |> transpose |> map(transpose)
  ))

  val upsampleWeights: ToBeTyped[Expr] = C2D.weights1d(1.0f / 4.0f, Seq(
    1, 3
  ))

  // TODO: should be a bilinear filter
  val upsample2D: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    ((h+1)`.`(w+1)`.`f32) ->: (((2*h)+1)`.`((2*w)+1)`.`f32)
  )(input =>
    input |>
    map(map(fun(x =>
      generate(fun(_ => generate(fun(_ => x)))) :: (2`.`2`.`f32)
    ))) >> // H+1.W+1.2.2.
    map(join >> transpose) >> join >> // 2H+2.2W+2.
    slide2D(2, 1) >>
    map(map(
      transpose >>
      map(dot(upsampleWeights)) >>
      dot(upsampleWeights)
    ))
  ))

  val upsample3D: ToBeTyped[Expr] = depFun((h: Nat, w: Nat, k: Nat) => fun(
    ((h+1)`.`(w+1)`.`k`.`f32) ->: (((2*h)+1)`.`((2*w)+1)`.`k`.`f32)
  )(input =>
    input |> map(transpose) |> transpose |> map(
      upsample2D(h)(w)
    ) |> transpose |> map(transpose)
  ))

  private def larr_f32(s: Seq[Float]): ToBeTyped[Expr] = {
    larr(s.map(semantics.FloatData))
  }

  val floating: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    (3`.`h`.`w`.`u16) ->: (3`.`h`.`w`.`f32)
  )(input => input |>
    map(map(map(fun(p =>
      cast(p) / l(65535.0f)
    ))))
  ))

  val output_to_u16: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    (3`.`h`.`w`.`f32) ->: (3`.`h`.`w`.`u16)
  )(input => input |>
    map(map(map(fun(p =>
      cast(clamp(f32)(p, l(0.0f), l(1.0f)) * l(65535.0f))
    ))))
  ))

  val gray: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    (3`.`h`.`w`.`f32) ->: (h`.`w`.`f32)
  )(input => input |>
    transpose >> map(transpose) >>
    map(map(dot(larr_f32(Seq(0.299f, 0.587f, 0.114f)))))
  ))

  val remap: ToBeTyped[Expr] = fun(f32)(alpha =>
    generate(fun { x =>
      val fx = (cast(x) :: f32) / l(256.0f)
      alpha * fx * exp(-fx * fx / l(2.0f))
    })
  )

  val lookup: ToBeTyped[Expr] = depFun((levels: Nat, h: Nat, w: Nat) => fun(
    f32 ->: ((levels*256)`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`levels`.`f32)
  )((beta, remap, grey) =>
    grey |> map(map(fun(p =>
      generate(fun { k =>
        val maxLevel = l(levels) - l(1: Nat)
        def toF(e: ToBeTyped[Expr]) = cast(e) :: f32
        val level = toF(k) * l(1.0f) / toF(maxLevel)
        val idx = clamp(int)(
          cast(p * toF(maxLevel) * l(256.0f)),
          l(0), (cast(maxLevel) :: int) * l(256))
        val ridx = natAsIndex(levels*256)(cast(
          idx - l(256) * (cast(k) :: int)))
        beta * (p - level) + level + (remap `@` ridx)
      })
    )))
  ))

  def sub3D: ToBeTyped[Expr] = impl { m: Nat => impl { n: Nat => impl { o: Nat => fun(
    (m`.`n`.`o`.`f32) ->: (m`.`n`.`o`.`f32) ->: (m`.`n`.`o`.`f32)
  )((a, b) =>
    zipND(3)(a, b) |> map(map(map(fun(p => fst(p) - snd(p)))))
  )}}}

  def laplacianOutput: ToBeTyped[Expr] =
    impl { h: Nat => impl { w: Nat => impl { levels: Nat => fun(
      (h`.`w`.`f32) ->: (h`.`w`.`levels`.`f32) ->: (h`.`w`.`f32)
    )((inGPyramid, lPyramid) =>
      zipND(2)(inGPyramid, lPyramid) |> map(map(fun { p => // h.w.
        val level = fst(p) * (cast(l(levels) - l(1: Nat)) :: f32)
        val li = clamp(int)(cast(level), l(0), cast(l(levels) - l(2: Nat)))
        val lf = level - (cast(li) :: f32)
        l(1.0f) - lf * (snd(p) `@` cast(li)) + lf * (snd(p) `@` cast(li + l(1)))
      }))
    )}}}

  def add2D: ToBeTyped[Expr] = impl { m: Nat => impl { n: Nat => fun(
    (m`.`n`.`f32) ->: (m`.`n`.`f32) ->: (m`.`n`.`f32)
  )((a, b) =>
    zipND(2)(a, b) |> map(map(fun(p => fst(p) + snd(p))))
  )}}

  def color: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
    (h`.`w`.`f32) ->: (3`.`h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (3`.`h`.`w`.`f32)
  )((outGPyramid, floating, gray) =>
    zipND(2)(outGPyramid, zipND(2)(floating |> transpose |> map(transpose), gray)) |>
    map(map(fun { p =>
      val outG = fst(p)
      val gray = snd(snd(p))
      fst(snd(p)) |> map(fun { floating =>
        val eps = l(0.01f)
        outG * (floating + eps) / (gray + eps)
      })
    })) |> map(transpose) |> transpose
  ))

  def buildPyramid(
    start: Int, stop: Int,
    init: ToBeTyped[Expr],
    nextLevel: (Int, ToBeTyped[Expr]) => ToBeTyped[Expr],
    k: Seq[ToBeTyped[Expr]] => ToBeTyped[Expr]
  ): ToBeTyped[Expr] = {
    val step = if (start < stop) 1 else -1
    buildPyramidRec(start, stop, step, Seq(init),
      (i, p) => nextLevel(i, p.last),
      p => k(if (start < stop) p else p.reverse))
  }

  private def buildPyramidRec(
    last: Int, stop: Int, step: Int,
    pyramid: Seq[ToBeTyped[Expr]],
    nextLevel: (Int, Seq[ToBeTyped[Expr]]) => ToBeTyped[Expr],
    k: Seq[ToBeTyped[Expr]] => ToBeTyped[Expr]
  ): ToBeTyped[Expr] =
    if (last != stop) {
      val current = last + step
      // TODO: don't always want letf?
      nextLevel(current, pyramid) |> letf(fun(nl =>
        buildPyramidRec(current, stop, step, pyramid :+ nl, nextLevel, k)
      ))
    } else {
      k(pyramid)
    }

  def nModFun(m: Nat, f: Nat => ToBeTyped[Expr]): ToBeTyped[Expr] = {
    import arithexpr.arithmetic._
    depFun(RangeAdd(0, PosInf, m), f)
  }

  def dropLast2D(n: Nat): ToBeTyped[Expr] = dropLast(n) >> map(dropLast(n))

  // pyramidLevels from 1 to 20
  def localLaplacian(pyramidLevels: Int = 8): ToBeTyped[Expr] =
    depFun((levels: Nat, h: Nat, w: Nat) => fun(
      f32 ->: f32 ->: (3`.`h`.`w`.`u16) ->: (3`.`h`.`w`.`u16)
    )((alpha, beta, input) => {
      map(padClamp2D(0, 3))(input) |> fun(p =>
      floating(h+3)(w+3)(p) |> fun(f =>
      gray(h+3)(w+3)(f) |> fun(g =>
      remap(alpha) |> fun(r =>
      lookup(levels)(h+3)(w+3)(beta)(r)(g) |> fun(l =>
      // Make the processed Gaussian pyramid
      buildPyramid(0, pyramidLevels - 1, l, (_, last) =>
        impl { hp: Nat => impl { wp: Nat => downsample3D(hp)(wp)(levels)(last) }},
        gPyramid =>
      // Get its laplacian pyramid
      buildPyramid(pyramidLevels - 1, 0, gPyramid.last |> dropLast2D(2), (i, _) =>
        impl { hp: Nat => impl { wp: Nat =>
          sub3D(gPyramid(i) |> dropLast2D(2),
            upsample3D(hp)(wp)(levels)(gPyramid(i + 1) |> dropLast2D(2)))
        }},
        lPyramid =>
      // Make the Gaussian pyramid of the input
      buildPyramid(0, pyramidLevels - 1, g, (_, last) =>
        impl { hp: Nat => impl { wp: Nat => downsample2D(hp)(wp)(last) }},
        inGPyramid =>
      // Make the laplacian pyramid of the output
      buildPyramidRec(-1, pyramidLevels - 1, 1, Seq(), (i, _) =>
        laplacianOutput(inGPyramid(i) |> dropLast2D(2), lPyramid(i)),
        outLPyramid =>
      // Make the gaussian pyramid of the output
      buildPyramid(pyramidLevels - 1, 0, outLPyramid.last, (i, last) =>
        impl { hp: Nat => impl { wp: Nat =>
          add2D(upsample2D(hp)(wp)(last), outLPyramid(i))
        }},
        outGPyramid =>
        color(h)(w)(outGPyramid(0) |> dropLast2D(1))(f |> map(dropLast2D(3)))(g |> dropLast2D(3)) |>
        output_to_u16(h)(w)
      )
      )))))))))
    }))

  private val id = fun(x => x)

  object omp { // and plain C
    import rise.openMP.primitives._

    def localLaplacianNaivePar(pyramidLevels: Int = 8): ToBeTyped[Expr] =
      depFun((levels: Nat, h: Nat, w: Nat) => fun(
        f32 ->: f32 ->: (3`.`h`.`w`.`u16) ->: (3`.`h`.`w`.`u16)
      )((alpha, beta, input) => {
        remap(alpha) |> mapPar(id) |> store(r =>
        map(padClamp2D(0, 3))(input) |> fun(p =>
        floating(h+3)(w+3)(p) |> fun(f =>
        gray(h+3)(w+3)(f) |> mapPar(mapSeq(id)) |> store(g =>
        lookup(levels)(h+3)(w+3)(beta)(r)(g) |> mapPar(mapSeq(mapSeq(id))) |> store(l =>
        // Make the processed Gaussian pyramid
        buildPyramid(0, pyramidLevels - 1, l, (_, last) =>
          impl { hp: Nat => impl { wp: Nat =>
            downsample3D(hp)(wp)(levels)(last) |> mapPar(mapSeq(mapSeq(id))) |> toMem// |> letf
          }},
          gPyramid =>
        // Get its laplacian pyramid
        buildPyramid(pyramidLevels - 1, 0, gPyramid.last |> dropLast2D(2), (i, _) =>
          impl { hp: Nat => impl { wp: Nat =>
            sub3D(gPyramid(i) |> dropLast2D(2),
              upsample3D(hp)(wp)(levels)(gPyramid(i + 1) |> dropLast2D(2))) |>
            mapPar(mapSeq(mapSeq(id))) |> toMem// |> letf
          }},
          lPyramid =>
        // Make the Gaussian pyramid of the input
        buildPyramid(0, pyramidLevels - 1, g, (_, last) =>
          impl { hp: Nat => impl { wp: Nat =>
            downsample2D(hp)(wp)(last) |> mapPar(mapSeq(id)) |> toMem// |> letf
          }},
          inGPyramid =>
        // Make the laplacian pyramid of the output
        buildPyramidRec(-1, pyramidLevels - 1, 1, Seq(), (i, _) =>
          laplacianOutput(inGPyramid(i) |> dropLast2D(2), lPyramid(i)) |>
          mapPar(mapSeq(id)) |> toMem,// |> letf,
          outLPyramid =>
        // Make the gaussian pyramid of the output
        buildPyramid(pyramidLevels - 1, 0, outLPyramid.last, (i, last) =>
          impl { hp: Nat => impl { wp: Nat =>
            add2D(upsample2D(hp)(wp)(last), outLPyramid(i)) |>
            mapPar(mapSeq(id)) |> toMem// |> letf
          }},
          outGPyramid =>
          color(h)(w)(
            outGPyramid(0) |> dropLast2D(1))(
            f |> map(dropLast2D(3)))(
            g |> dropLast2D(3)) |>
          output_to_u16(h)(w) |>
          mapSeq(mapPar(mapSeq(id)))
        )
        )))))))))
      }))
  }
}
