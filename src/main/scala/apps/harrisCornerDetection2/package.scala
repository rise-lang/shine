package apps

import rise.core._
import rise.core.DSL._
import rise.core.primitives._
import rise.core.types._
import rise.openCL.primitives._
import rise.core.DSL.HighLevelConstructs.zipND

/** This version of Harris follows from the following paper:
  * https://dl.acm.org/doi/abs/10.1145/2568058.2568067
  *
  * Compared to Halide's version:
  * - it starts from grayscale images instead of color images
  * - it uses a binomial filter instead of a box filter
  *
  * The algorithm is simplified:
  * - there is no padding and the output is smaller than the input.
  */
package object harrisCornerDetection2 {
  val num_threads = 4
  val vecw = 8
  val bd_h = 16
  val bd_w = 32
  val tile_x = 32
  val tile_y = 8

  val hFrom = (n: Int) =>
    arithexpr.arithmetic.RangeAdd(n, arithexpr.arithmetic.PosInf, 8)
  val wFrom = (n: Int) =>
    arithexpr.arithmetic.RangeAdd(12, arithexpr.arithmetic.PosInf, 32)

  val id: ToBeTyped[Expr] = fun(x => x)
  val mulT: ToBeTyped[Expr] = fun(x => fst(x) * snd(x))
  val zip2D: ToBeTyped[Expr] = zipND(2)
  val dotSeqU: ToBeTyped[Expr] = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> oclReduceSeqUnroll(AddressSpace.Private)(add)(lf32(0.0f))
  ))
  val dotSeqUWV: ToBeTyped[Expr] = fun(weights => fun(vectors =>
    zip(map(vectorFromScalar)(weights))(vectors) |>
      map(mulT) |> oclReduceSeqUnroll(AddressSpace.Private)(add)(vectorFromScalar(lf32(0.0f)))
  ))

  val shuffle =
    asScalar >> drop(vecw-1) >> take(vecw+2) >> slide(vecw)(1) >> join >> asVector(vecw)

  val binomialWeights2d = apps.separableConvolution2D.binomialWeights2d
  val binomialWeightsH = apps.separableConvolution2D.binomialWeightsH
  val binomialWeightsV = apps.separableConvolution2D.binomialWeightsV

  val sobelXWeights2d = apps.separableConvolution2D.sobelXWeights2d
  val sobelXWeightsH = apps.separableConvolution2D.sobelXWeightsH
  val sobelXWeightsV = apps.separableConvolution2D.sobelXWeightsV

  val sobelYWeights2d = apps.separableConvolution2D.sobelYWeights2d
  val sobelYWeightsH = apps.separableConvolution2D.sobelYWeightsH
  val sobelYWeightsV = apps.separableConvolution2D.sobelYWeightsV
}
