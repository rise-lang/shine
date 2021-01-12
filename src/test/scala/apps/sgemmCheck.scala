package apps

import sgemm._
import util.gen
import shine.OpenCL.{GlobalSize, LocalSize}
import rise.core.types._
import shine.OpenCL.KernelExecutor.KernelWithSizes
import util.gen.c.function

//noinspection TypeAnnotation
class sgemmCheck extends test_util.TestsWithExecutor {

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
    c.sequential.t
  }

  test("Sequential gemm compiles to syntactically correct C") {
    function.asStringFromExpr("gemm")(c.sequential)
  }

  test("mali gemm type inference works") {
    mali_GEMM.t
  }

  test("mali gemm compiles to syntactically correct kernel") {
    gen.opencl.kernel.fromExpr()(mali_GEMM)
  }

  test("Kepler best type inference works") {
    keplerBest.t
  }

  test("Kepler best compiles to syntactically correct kernel") {
    gen.opencl.kernel.fromExpr("KERNEL", Some(LocalSize((32,8,1)), GlobalSize((256, 128, 1))))(keplerBest)
  }

  test("OpenCL sequential gemm versions produce the expected result") {
    val M = 512
    val N = 256
    val K = 64

    val (aMat, bMat, cMat, alpha, beta, goldMat) = randomSgemmGold(M, N, K)

    val localSize = LocalSize(1)
    val globalSize = GlobalSize(1)
    val kernel = gen.opencl.kernel.fromExpr("KERNEL", Some(localSize, globalSize))(sequential)

    val (flatOutput, _ ) = runSgemmKernel(KernelWithSizes(kernel, localSize, globalSize),
      aMat, bMat, cMat, alpha, beta, M, N, K)

    assert(flatOutput sameElements goldMat)
  }

  test("OpenCL mali_gemm version produces the expected result") {
    val M = 512
    val N = 256
    val K = 64

    val (aMat, bMat, cMat, alpha, beta, goldMat) = randomSgemmGold(M, N, K)

    val localSize = LocalSize((2, 2))
    val globalSize = GlobalSize((N/2, N/2))
    val kernel = gen.opencl.kernel.fromExpr("KERNEL", Some(localSize, globalSize))(mali_GEMM)

    val runs = Seq(
      "dpia" -> runSgemmKernel(
        KernelWithSizes(kernel, localSize, globalSize),
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

    val localSize = LocalSize((32,8,1))
    val globalSize = GlobalSize((256, 128, 1))
    val kernel = gen.opencl.kernel.fromExpr("KERNEL", Some(localSize, globalSize))(keplerBest)

    val runs = Seq(
      "dpia" -> runSgemmKernel(
        KernelWithSizes(kernel, localSize, globalSize),
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
