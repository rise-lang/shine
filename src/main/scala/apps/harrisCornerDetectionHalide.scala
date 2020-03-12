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

  val gray: Expr = nFun(h => nFun(w => fun(
    (3`.`h`.`w`.`f32) ->: (h`.`w`.`f32)
  )(input => input |>
    transpose >> map(transpose) >>
    map(map(dot(larr(Seq(0.299.f, 0.587f, 0.114f)))))
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
    )(input =>
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
  val coarsityScalar = fun(sxx => fun(sxy => fun(syy => fun(kappa => {
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
      coarsityScalar(sxx)(sxy)(syy)(l(0.04f))
    }))
  )))
}
