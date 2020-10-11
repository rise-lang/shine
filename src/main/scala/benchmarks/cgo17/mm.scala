package benchmarks.cgo17

import apps.mm._
import benchmarks.core._
import shine.OpenCL._
import shine.DPIA
import util._

object mm {
  def withSize(N: Int, M: Int, O: Int, sampleCount: Int): Unit = {
    val rand = new scala.util.Random()
    val At = Array.fill(O, N)(rand.nextFloat() * 10)
    val B = Array.fill(O, M)(rand.nextFloat() * 10)


    val localSize = LocalSize((32, 8))
    def globalSize = GlobalSize((M/4, N/8))
    def globalSizeGen(phrase: DPIA.Phrases.Phrase[_ <: DPIA.Types.PhraseType]) = {
      val t = phrase.t.asInstanceOf[DPIA.`(nat)->:`[DPIA.`(nat)->:`[
        DPIA.Types.ExpType
      ]]]
      val n = t.x
      val m = t.t.x
      GlobalSize((m /^ 4, n /^ 8))
    }
    // FIXME: input sizes should remain variable in globalSize during codegen
    val amdKernel = Kernel.forgetSizes(gen.OpenCLKernel(
      phrase => (localSize, globalSizeGen(phrase))
    )(amd, "KERNEL"))
    val nvidiaKernel = Kernel.forgetSizes(gen.OpenCLKernel(
      phrase => (localSize, globalSizeGen(phrase))
    )(nvidia, "KERNEL"))

    val stats = Seq(
      ("original AMD", benchmark(sampleCount, runOriginal("CGO17_MMAMD.cl",
        localSize, globalSize, At, B)._2)),
      ("dpia AMD", benchmark(sampleCount, runKernel(amdKernel,
        localSize, globalSize, At, B)._2)),
      ("original NVIDIA", benchmark(sampleCount, runOriginal("CGO17_MMNVIDIA.cl",
        localSize, globalSize, At, B)._2)),
      ("dpia NVIDIA", benchmark(sampleCount, runKernel(nvidiaKernel,
        localSize, globalSize, At, B)._2))
    )
    println(s"runtime over $sampleCount runs for size ${(N, M, O)}")
    stats.foreach { case (name, stat) => println(s"$name: $stat") }
  }

  def main(args: Array[String]): Unit = withExecutor {
    withSize(1024, 1024, 1024, 8)
    withSize(4096, 4096, 4096, 2)
  }
}
