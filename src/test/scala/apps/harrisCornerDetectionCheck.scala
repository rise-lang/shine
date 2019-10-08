package apps

import harrisCornerDetection._
import util._

import scala.reflect.ClassTag

class harrisCornerDetectionCheck extends test_util.TestsWithExecutor {
  private def zip2D[A, B](as: Array[Array[A]], bs: Array[Array[B]]): Array[Array[(A, B)]] =
    as.zip(bs).map { case (a, b) => a.zip(b) }
  private def computePointwiseGold[A: ClassTag, B: ClassTag](inputs: Array[Array[A]], f: A => B): Array[Array[B]] =
    inputs.map(r => r.map(f))

  private def computeGold(h: Int, w: Int,
                          input: Array[Array[Float]],
                          kappa: Float): Array[Array[Float]] = {
    val ix = separableConvolution2DCheck.computeGold(h, w, input, separableConvolution2D.sobelXWeights2d)
    val iy = separableConvolution2DCheck.computeGold(h, w, input, separableConvolution2D.sobelYWeights2d)
    val ixx = computePointwiseGold(ix, { x: Float => x * x })
    val ixy = computePointwiseGold[(Float, Float), Float](zip2D(ix, iy), { case (a, b) => a * b })
    val iyy = computePointwiseGold(iy, { x: Float => x * x })
    val sxx = separableConvolution2DCheck.computeGold(h, w, ixx, separableConvolution2D.binomialWeights2d)
    val sxy = separableConvolution2DCheck.computeGold(h, w, ixy, separableConvolution2D.binomialWeights2d)
    val syy = separableConvolution2DCheck.computeGold(h, w, iyy, separableConvolution2D.binomialWeights2d)
    computePointwiseGold[(Float, (Float, Float)), Float](zip2D(sxx, zip2D(sxy, syy)), { case (sxx, (sxy, syy)) =>
        val det = sxx * syy - sxy * sxy
        val trace = sxx + syy
        det - kappa * trace * trace
    })
  }

  private val H = 20
  private val W = 80
  private val kappa = 1.2f
  private val threshold = 1.4f

  test("harris compiles to C code") {
    val random = new scala.util.Random()
    val input = Array.fill(H, W)(random.nextFloat)
    val gold = computeGold(H, W, input, kappa).flatten
    val (res, ts) = genOCLKernels().run(input, kappa)
    util.assertSame(gold, res, "output is different from gold")
    var totalT = TimeSpan.inMilliseconds(0.0f)
    ts.foreach { case (n, t) =>
      println(s"$n: $t")
      totalT = totalT + t
    }
    println(s"total: $totalT")
  }
}
