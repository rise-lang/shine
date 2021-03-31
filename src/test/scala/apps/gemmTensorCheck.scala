package apps

import apps.gemmTensor._
import apps.mmCheckUtils._
import rise.core.Expr
import shine.OpenCL._
import shine.cuda.KernelExecutor.{KernelNoSizes, KernelWithSizes}
import util._

//Cause some TypeChecking-Bugs the execution of the entire test-class could be fail
//Running each test individually should be successfull
class gemmTensorCheck extends test_util.TestWithCUDA {

  test("gemm with tensor cores produces expected result") {
    executeGEMM(simpleGemm)
  }

  test("gemm with tensor cores and multiple fragments per warp produces expected result") {
    executeGEMM(gemmMultipleFragmentsPerWarp)
  }

  //TODO fix this tests
  ignore("gemm with tensor cores and shared memory produces expected result 1") {
    executeGEMMWithSizes(gemmSharedMemory(config1),4)
    executeGEMMWithSizes(gemmSharedMemory(config2),4)
  }

  ignore("gemm with tensor cores and shared memory produces expected result 2") {
    executeGEMMWithSizes(gemmSharedMemory(config3),8)
    executeGEMMWithSizes(gemmSharedMemory(config4),8)
  }

  ignore("gemm with tensor cores and shared memory produces expected result 3") {
    executeGEMMWithSizes(gemmSharedMemory(config6),16)
    executeGEMMWithSizes(gemmSharedMemory(config7),16)
  }

  ignore("gemm with tensor cores and shared memory produces expected result 4") {
    executeGEMMWithSizes(gemmSharedMemoryV2(config1),4)
    executeGEMMWithSizes(gemmSharedMemoryV2(config2),4)
  }

  ignore("gemm with tensor cores and shared memory produces expected result 5") {
    executeGEMMWithSizes(gemmSharedMemoryV2(config3),8)
    executeGEMMWithSizes(gemmSharedMemoryV2(config4),8)
  }

  ignore("gemm with tensor cores and shared memory produces expected result 6") {
    executeGEMMWithSizes(gemmSharedMemoryV2(config6),16)
    executeGEMMWithSizes(gemmSharedMemoryV2(config7),16)
  }

  private def executeGEMM(gemmKernel: Expr, matrixBTranspose: Boolean = true, matrixATranspose: Boolean = false) : Unit = {
    val (alpha, beta, a, b, c, gold) = generateGold(m, n, k)

    val kernel = gen.cuda.kernel("gemm").fromExpr(gemmKernel)

    logger.debug(shine.cuda.KernelModule.translationToString(kernel))

    if (executeCudaTests) {
      val run = KernelNoSizes(kernel, compilerOptions).as[ScalaFunction `(`
        Int `,` Int `,` Int `,` Float `,` Float `,`
        Array[Array[Float]] `,` Array[Array[Float]] `,` Array[Array[Float]]
        `)=>` Array[Float]]

      val aMatrix = if (matrixATranspose) a.transpose else a
      val bMatrix = if (matrixBTranspose) b.transpose else b
      val (output, _) =  run(LocalSize(32), GlobalSize(32))(m `,` n `,` k `,` alpha `,` beta `,` aMatrix `,` bMatrix `,` c)

      checkResult(gold, output)
    }
  }

  private def executeGEMMWithSizes(gemmKernel: Expr, numberOfWarps: Int, matrixBTranspose: Boolean = true, matrixATranspose: Boolean = false) : Unit = {
    val (alpha, beta, a, b, c, gold) = generateGold(m, n, k)

    val (localSize, globalSize) = (LocalSize(numberOfWarps * 32), GlobalSize(numberOfWarps * 32))
    val kernel = gen.cuda.kernel(localSize, globalSize, "gemm").fromExpr(gemmKernel)

    val compilerOptions =
      if (numberOfWarps < 16)
        mmCheckUtils.compilerOptions
      else
        mmCheckUtils.compilerOptions.appended("-maxrregcount=120")

    logger.debug(shine.cuda.KernelModule.translationToString(kernel))

    if (executeCudaTests) {
      val run = KernelWithSizes(kernel, localSize, globalSize, compilerOptions).as[ScalaFunction `(`
        Int `,` Int `,` Int `,` Float `,` Float `,`
        Array[Array[Float]] `,` Array[Array[Float]] `,` Array[Array[Float]]
        `)=>` Array[Float]]

      val aMatrix = if (matrixATranspose) a.transpose else a
      val bMatrix = if (matrixBTranspose) b.transpose else b
      val (output, _) =  run(m `,` n `,` k `,` alpha `,` beta `,` aMatrix `,` bMatrix `,` c)

      checkResult(gold, output)
    }
  }

  /**
    * Generates random data for tests and calculates gold.
    *
    * @param m first dimension for matrix multiplication
    * @param n second dimension for matrix multiplication
    * @param k third dimension for matrix multiplication
    * @return (alpha, beta, a-matrix, b-matrix, c-matrix, result)
    */
  private def generateGold(m: Int, n: Int, k: Int): (Float, Float, Array[Array[Float]], Array[Array[Float]], Array[Array[Float]], Array[Float]) = {
    val rand = new scala.util.Random

    val alpha = 2f
    val beta = 1f
    val A = Array.fill(m, k)(rand.nextFloat() * 10)
    val B = Array.fill(k, n)(rand.nextFloat() * 10)
    val C = Array.fill(m, n)(rand.nextFloat() * 10)
    val gold = computeGold(alpha, beta, A, B, C).flatten

    (alpha, beta, A, B, C, gold)
  }

  /**
    * Execute MMA-operation (alpha*matrixA*matrixB + beta*matrixC) using scala.
    *
    * @param alpha first scalar factor
    * @param beta second scalar factor
    * @param matrixA first matrix factor
    * @param matrixB second matrix factor
    * @param matrixC accumulator matrix
    * @return result of MMA-operation
    */
  private def computeGold(alpha: Float, beta: Float, matrixA: Array[Array[Float]], matrixB: Array[Array[Float]],
                          matrixC: Array[Array[Float]]): Array[Array[Float]] = {
    assert(matrixA.transpose.length == matrixB.length)

    (matrixA zip matrixC)
    .map(rowAC =>
      (matrixB.transpose zip rowAC._2)
      .map(columnBC =>
        (rowAC._1 zip columnBC._1
        map
          Function.tupled(_ * _)).sum * alpha + columnBC._2 * beta))
  }
}
