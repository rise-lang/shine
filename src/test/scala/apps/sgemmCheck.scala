package apps

import sgemm._
import util.gen
import shine.OpenCL.{GlobalSize, LocalSize}
import rise.core.types._

//noinspection TypeAnnotation
class sgemmCheck extends shine.test_util.TestsWithExecutor {

  val epsilon = 1.0f

  def randomSgemmGold(M: Int, N: Int, K: Int):
    (Array[Array[Float]], Array[Array[Float]], Array[Array[Float]], Float, Float, Array[Float]) = {
    import scala.util.Random

    val random = new Random()
    val A = Array.fill(M, K)((random.nextInt(10) + 1).toFloat)
    val B = Array.fill(K, N)((random.nextInt(10) + 1).toFloat)
    val C = Array.fill(M, N)((random.nextInt(10) + 1).toFloat)
    val alpha = (random.nextInt(10) + 1).toFloat
    val beta = (random.nextInt(10) + 1).toFloat

    val gold = computeSGEMMGold(A, B, C, alpha, beta)

    (A, B, C, alpha, beta, gold.flatten)
  }

  test("Sequential gemm type inference works") {
    infer(c.sequential)
  }

  test("Sequential gemm compiles to syntactically correct C") {
    gen.CProgram(c.sequential)
  }

  test("mali gemm type inference works") {
    infer(mali_GEMM)
  }

  test("mali gemm compiles to syntactically correct kernel") {
    gen.OpenCLKernel(mali_GEMM)
  }

  test("Kepler best type inference works") {
    infer(keplerBest)
  }

  test("Kepler best compiles to syntactically correct kernel") {
    gen.OpenCLKernel(LocalSize((32,8,1)), GlobalSize((256, 128, 1)))(keplerBest, "KERNEL")
  }

  test("OpenCL sequential gemm versions produce the expected result") {
    val M = 512
    val N = 256
    val K = 64

    val (aMat, bMat, cMat, alpha, beta, goldMat) = randomSgemmGold(M, N, K)

    val (flatOutput, _ ) = runSgemmKernel(
      gen.OpenCLKernel(LocalSize(1), GlobalSize(1))(sequential, "KERNEL"),
      aMat, bMat, cMat, alpha, beta, M, N, K)

    assert(flatOutput sameElements goldMat)
  }

  test("OpenCL mali_gemm version produces the expected result") {
    val M = 512
    val N = 256
    val K = 64

    val (aMat, bMat, cMat, alpha, beta, goldMat) = randomSgemmGold(M, N, K)

    val runs = Seq(
      "dpia" -> runSgemmKernel(
        gen.OpenCLKernel(LocalSize((2, 2)), GlobalSize((N/2, N/2)))(mali_GEMM, "KERNEL"),
        aMat, bMat.transpose, cMat, alpha, beta, M, N, K),
      "original" -> runOriginalSgemm("maliSgemm.cl", LocalSize((2, 2)), GlobalSize((M/2, N/2)),
        aMat, bMat.transpose, cMat, alpha, beta, M, N, K, isMaliKernel = true))

    runs.foreach(r => {
      util.assertSame(r._2._1, goldMat, s"${r._1} is different from gold")
      println(s"${r._1} time: ${r._2._2}")
    })
  }


  test("OpenCL keplerBest version produces the expected result") {
    val M = 512
    val N = 256
    val K = 64

    val (aMat, bMat, cMat, alpha, beta, goldMat) = randomSgemmGold(M, N, K)

    val runs = Seq(
      "dpia" -> runSgemmKernel(
        gen.OpenCLKernel(LocalSize((32,8,1)), GlobalSize((256, 128, 1)))(keplerBest, "KERNEL"),
        aMat.transpose, bMat, cMat, alpha, beta, M, N, K),
      //TODO Original code injects work-item sizes as well as input size dimensions.
      // How are input dimensions injected and why? Variables for the input size dimensions appear in the code as well.
      "original" -> runOriginalSgemm("keplerSgemm.cl", LocalSize((32,8,1)), GlobalSize((256, 128, 1)),
        aMat.transpose, bMat, cMat, alpha, beta, M, N, K))

    runs.foreach(r => {
      util.assertSame(r._2._1, goldMat, s"${r._1} is different from gold")
      println(s"${r._1} time: ${r._2._2}")
    })
  }
}
