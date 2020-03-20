package benchmarks.halide

import apps.harrisCornerDetectionHalide.ocl._
import apps.harrisCornerDetectionHalideRewrite.{ocl => rewrite}
import benchmarks.core._
import shine.OpenCL._
import util._

object harrisCornerDetection {
  val v = 4
  val Hi = 1536
  val Wi = 2560
  val H = Hi - 4
  val W = (Wi / v) - 4

  assert(Wi % v == 0)

  private type F = ScalaFunction `(`
    Int `,` Int `,` Array[Array[Array[Float]]] `)=>` Array[Float]
  private def toFunction(e: rise.core.Expr, s: (LocalSize, GlobalSize))
  : F#T => (F#R, TimeSpan[Time.ms]) =
  {
    val lowered = rewrite.unrollDots(rise.core.types.infer(e))
    val kernel = gen.OpenCLKernel(lowered)
    val f = kernel.as[F]
    f(s._1, s._2)
  }

  def main(args: Array[String]): Unit = {
    val random = new scala.util.Random()
    val input = Array.fill(3, Hi, Wi)(random.nextFloat)

    val s1 = (LocalSize(1), GlobalSize(1))
    val s32 = (LocalSize(1), GlobalSize(H / 32))
    // assert(H % 32 == 0) FIXME

    val kernelB = toFunction(harrisBuffered, s1)
    val kernelVU = toFunction(harrisVecUnaligned, s1)
    val kernelBVU = toFunction(harrisBufferedVecUnaligned, s1)
    val kernelBVUSP = toFunction(harrisBufferedVecUnalignedSplitPar, s32)

    withExecutor {
      val samples = 10
      val stats = Seq(
        ("buffered",
          benchmark(samples, kernelB(H `,` W `,` input)._2)),
        ("vec (u)",
          benchmark(samples, kernelVU(H `,` W `,` input)._2)),
        ("buffered, vec (u)",
          benchmark(samples, kernelBVU(H `,` W `,` input)._2)),
        ("buffered, vec (u), split par (32)",
          benchmark(samples, kernelBVUSP(H `,` W `,` input)._2)),
      )
      println(s"runtime over $samples runs")
      stats.foreach { case (name, stat) => println(s"$name: $stat") }
    }
  }
}
