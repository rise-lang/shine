package apps

import rise.core._
import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.types._
import rise.core.HighLevelConstructs.{slide2D, zipND}

object harrisCornerDetectionHalide {
  private val C2D = separableConvolution2D
  private val mulT = C2D.mulT
  private val dot = C2D.dot

  private def larr_f32(s: Seq[Float]): Expr = {
    larr(s.map(semantics.FloatData))
  }

  val gray: Expr = nFun(h => nFun(w => fun(
    (3`.`h`.`w`.`f32) ->: (h`.`w`.`f32)
  )(input => input |>
    transpose >> map(transpose) >>
    map(map(dot(larr_f32(Seq(0.299f, 0.587f, 0.114f)))))
  )))

  val sobelXWeights2d: Expr = C2D.weights2d(1.0f / 12.0f, Seq(
    Seq(-1, 0, +1),
    Seq(-2, 0, +2),
    Seq(-1, 0, +1)
  ))
  val sobelXWeightsV: Expr = C2D.weights1d(1.0f / 4.0f, Seq(
    1, 2, 1
  ))
  val sobelXWeightsH: Expr = C2D.weights1d(1.0f / 4.0f, Seq(
    -1, 0, +1
  ))

  val sobelYWeights2d: Expr = C2D.weights2d(1.0f / 12.0f, Seq(
    Seq(-1, -2, -1),
    Seq( 0,  0,  0),
    Seq( 1,  2,  1)
  ))
  val sobelYWeightsV: Expr = sobelXWeightsH
  val sobelYWeightsH: Expr = sobelXWeightsV

  val conv3x3: Expr = fun(3`.`3`.`f32)(weights =>
    nFun(h => nFun(w => fun(
      ((h+2)`.`(w+2)`.`f32) ->: (h`.`w`.`f32)
    )(input => input |>
      slide2D(3, 1) >>
      map(map(fun(nbh => dot(join(weights))(join(nbh)))))
    )))
  )
  val sobelX: Expr = conv3x3(sobelXWeights2d)
  val sobelY: Expr = conv3x3(sobelYWeights2d)

  val mul = nFun(h => nFun(w => fun(
    (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32)
  )((a, b) =>
    zipND(2)(a, b) |> map(map(mulT))
  )))

  val sum: Expr = reduce(add)(l(0.0f))
  val sum3x3: Expr = nFun(h => nFun(w => fun(
    ((h+2)`.`(w+2)`.`f32) ->: (h`.`w`.`f32)
  )(input => input |>
    slide2D(3, 1) >>
    map(map(fun(nbh => sum(join(nbh)))))
  )))

  // halide: det + trace + output
  val coarsityElem = fun(sxx => fun(sxy => fun(syy => fun(kappa => {
    val det = sxx * syy - sxy * sxy
    val trace = sxx + syy
    det - kappa * trace * trace
  }))))
  val coarsity = nFun(h => nFun(w => fun(
    (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32) ->: (h`.`w`.`f32)
  )((sxx, sxy, syy) =>
    zipND(2)(sxx, zipND(2)(sxy, syy)) |> map(map(fun { s =>
      val sxx = fst(s)
      val sxy = fst(snd(s))
      val syy = snd(snd(s))
      coarsityElem(sxx)(sxy)(syy)(l(0.04f))
    }))
  )))

  val harris: Expr = nFun(h => nFun(w => fun(
    (3`.`(h+4)`.`(w+4)`.`f32) ->: (h`.`w`.`f32)
  )(input =>
    gray(h+4)(w+4)(input) |> fun(g =>
    sobelX(h+2)(w+2)(g) |> fun(ix =>
    sobelY(h+2)(w+2)(g) |> fun(iy =>
    mul(h+2)(w+2)(ix)(ix) |> fun(ixx =>
    mul(h+2)(w+2)(ix)(iy) |> fun(ixy =>
    mul(h+2)(w+2)(iy)(iy) |> fun(iyy =>
    sum3x3(h)(w)(ixx) |> fun(sxx =>
    sum3x3(h)(w)(ixy) |> fun(sxy =>
    sum3x3(h)(w)(iyy) |> fun(syy =>
    coarsity(h)(w)(sxx)(sxy)(syy)
    )))))))))
  )))

  private val id = fun(x => x)
  private val write1DSeq = mapSeq(id)
  private val write2DSeq = mapSeq(write1DSeq)

  val harrisSeqWrite: Expr = nFun(h => nFun(w => fun(
    (3`.`(h+4)`.`(w+4)`.`f32) ->: (h`.`w`.`f32)
  )(input =>
    gray(h+4)(w+4)(input) |> write2DSeq |> let(fun(g =>
    sobelX(h+2)(w+2)(g) |> write2DSeq |> let(fun(ix =>
    sobelY(h+2)(w+2)(g) |> write2DSeq |> let(fun(iy =>
    mul(h+2)(w+2)(ix)(ix) |> write2DSeq |> let(fun(ixx =>
    mul(h+2)(w+2)(ix)(iy) |> write2DSeq |> let(fun(ixy =>
    mul(h+2)(w+2)(iy)(iy) |> write2DSeq |> let(fun(iyy =>
    sum3x3(h)(w)(ixx) |> write2DSeq |> let(fun(sxx =>
    sum3x3(h)(w)(ixy) |> write2DSeq |> let(fun(sxy =>
    sum3x3(h)(w)(iyy) |> write2DSeq |> let(fun(syy =>
    coarsity(h)(w)(sxx)(sxy)(syy) |> write2DSeq
    ))))))))))))))))))
  )))

//  import rise.OpenMP.DSL._
  import rise.core.primitives.SlideSeq.{Indices => RotateIndices}

  private def lineBuffer(n: Nat): Expr =
    slideSeq(RotateIndices)(n)(1)(write1DSeq)

  val harrisBuffered = nFun(h => nFun(w => fun(
    (3`.`(h+4)`.`(w+4)`.`f32) ->: (h`.`w`.`f32)
  )(input => input |>
    transpose >> map(transpose) >>
    map(map(dot(larr_f32(Seq(0.299f, 0.587f, 0.114f))))) >>
    lineBuffer(3) >>
    mapStream(
      map(slide(3)(1)) >> transpose >>
      map(fun(nbh => pair(
        dot(join(sobelXWeights2d))(join(nbh)),
        dot(join(sobelYWeights2d))(join(nbh))
      )))
    ) >>
    lineBuffer(3) >>
    iterateStream(fun(ixiy =>
      ixiy |> map(map(fun(p => fst(p) * fst(p)))) |> fun(ixx =>
      ixiy |> map(map(fun(p => fst(p) * snd(p)))) |> fun(ixy =>
      ixiy |> map(map(fun(p => snd(p) * snd(p)))) |> fun(iyy =>
      slide2D(3, 1)(ixx) |> map(map(fun(nbh => sum(join(nbh))))) |>
      fun(sxx =>
      slide2D(3, 1)(ixy) |> map(map(fun(nbh => sum(join(nbh))))) |>
      fun(sxy =>
      slide2D(3, 1)(iyy) |> map(map(fun(nbh => sum(join(nbh))))) |>
      fun(syy =>
        zipND(2)(sxx, zipND(2)(sxy, syy)) |> map(map(fun { s =>
          val sxx = fst(s)
          val sxy = fst(snd(s))
          val syy = snd(snd(s))
          coarsityElem(sxx)(sxy)(syy)(l(0.04f))
        }))
      )))))) >> write2DSeq
    )) >> join
  )))

  private val sumVec = reduce(add)(vectorFromScalar(l(0.0f)))
  private val dotWeightsVec = fun(weights => fun(input =>
    zip(map(vectorFromScalar, weights), input) |> map(mulT) |> sumVec
  ))

  val harrisBufferedVecUnaligned = nFun(h => nFun(w => fun(
    (3`.`(h+4)`.`(w+4)`.`f32) ->: (h`.`w`.`f32)
  )(input => input |>
    // FIXME? we use the aligned primitive to generate an unaligned vload
    map(map(asVectorAligned(4))) >>
    transpose >> map(transpose) >>
    map(map(dotWeightsVec(larr_f32(Seq(0.299f, 0.587f, 0.114f))))) >>
    map(asScalar) >>
    lineBuffer(3) >>
    mapStream(
      map(slide(3)(1)) >> transpose >> // TODO: asVector
      map(map(C2D.shuffle) >> fun(nbh => pair(
        dotWeightsVec(join(sobelXWeights2d))(join(nbh)),
        dotWeightsVec(join(sobelYWeights2d))(join(nbh))
      )))
    ) >>
    lineBuffer(3) >>
    iterateStream(fun(ixiy =>
      ixiy |> map(map(fun(p => fst(p) * fst(p)))) |> fun(ixx =>
      ixiy |> map(map(fun(p => fst(p) * snd(p)))) |> fun(ixy =>
      ixiy |> map(map(fun(p => snd(p) * snd(p)))) |> fun(iyy =>
      slide2D(3, 1)(ixx) |> map(map(map(C2D.shuffle) >> fun(nbh => sumVec(join(nbh))))) |>
      fun(sxx =>
      slide2D(3, 1)(ixy) |> map(map(map(C2D.shuffle) >> fun(nbh => sumVec(join(nbh))))) |>
      fun(sxy =>
      slide2D(3, 1)(iyy) |> map(map(map(C2D.shuffle) >> fun(nbh => sumVec(join(nbh))))) |>
      fun(syy =>
        zipND(2)(sxx, zipND(2)(sxy, syy)) |> map(map(fun { s =>
          val sxx = fst(s)
          val sxy = fst(snd(s))
          val syy = snd(snd(s))
          coarsityElem(sxx)(sxy)(syy)(vectorFromScalar(l(0.04f)))
        }))
      )))))) >> write2DSeq
    )) >> join >> map(asScalar)
  )))

  // TODO: padding, tail strategy or something to keep same in/out?
  val harrisBufferedVecAligned = nFun(h => nFun(w => fun(
    (3`.`(h+4)`.`(w+4)`.`f32) ->: (h`.`w`.`f32)
  )(input => input |>
    map(map(asVectorAligned(4))) >>
    transpose >> map(transpose) >>
    map(map(dotWeightsVec(larr_f32(Seq(0.299f, 0.587f, 0.114f))))) >>
    lineBuffer(3) >>
    mapStream(
      map(slide(3)(1)) >> transpose >> // W.3.3.4.f
      map(map(C2D.shuffle) >> fun(nbh => pair(
        dotWeightsVec(join(sobelXWeights2d))(join(nbh)),
        dotWeightsVec(join(sobelYWeights2d))(join(nbh))
      )))
    ) >>
    lineBuffer(3) >>
    iterateStream(fun(ixiy =>
      ixiy |> map(map(fun(p => fst(p) * fst(p)))) |> fun(ixx =>
      ixiy |> map(map(fun(p => fst(p) * snd(p)))) |> fun(ixy =>
      ixiy |> map(map(fun(p => snd(p) * snd(p)))) |> fun(iyy =>
      slide2D(3, 1)(ixx) |> map(map(map(C2D.shuffle) >> fun(nbh => sumVec(join(nbh))))) |>
      fun(sxx =>
      slide2D(3, 1)(ixy) |> map(map(map(C2D.shuffle) >> fun(nbh => sumVec(join(nbh))))) |>
      fun(sxy =>
      slide2D(3, 1)(iyy) |> map(map(map(C2D.shuffle) >> fun(nbh => sumVec(join(nbh))))) |>
      fun(syy =>
        zipND(2)(sxx, zipND(2)(sxy, syy)) |> map(map(fun { s =>
          val sxx = fst(s)
          val sxy = fst(snd(s))
          val syy = snd(snd(s))
          coarsityElem(sxx)(sxy)(syy)(vectorFromScalar(l(0.04f)))
        }))
      )))))) >> write2DSeq
    )) >> join >> map(asScalar)
  )))
}
