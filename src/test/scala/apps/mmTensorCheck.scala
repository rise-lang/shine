package apps

import apps.mmTensor._
import rise.core.Expr
import shine.OpenCL._
import shine.cuda.KernelExecutor.{KernelNoSizes, KernelWithSizes}
import util._

class mmTensorCheck extends test_util.TestsWithYACX {
  import mmCheckUtils._

  test("matrix multiplication without tensor cores produces expected result") {
    executeMM(simpleMatMulWithoutTensorCores)
  }

  test("matrix multiplication a single fragment and tensor cores produces expected result") {
    val (a, b, gold) = generateGold(16, 16, 16)

    val kernel = gen.cuda.kernel("mm").fromExpr(simpleMatMulTile)

    try {
      println(shine.cuda.KernelModule.translationToString(kernel))

      val run = KernelNoSizes(kernel, compilerOptions).as[ScalaFunction `(`
        Array[Array[Float]] `,` Array[Array[Float]]
        `)=>` Array[Float]]

      val (output, _) =  run(LocalSize(1), GlobalSize(32))(a `,` b)

      checkResult(output, gold)
    } catch {
      case _: UnsatisfiedLinkError => System.err.println("UnsatisfiedLinkError")
    }
  }

  test("matrix multiplication with tensor cores produces expected result") {
    executeMM(simpleMatMul)
  }

  test("matrix multiplication with tensor cores 2 produces expected result") {
    executeMM(simpleMatMulBMatrixTransposed, true)
  }

  test("matrix multiplication with tensor cores 3 produces expected result") {
    executeMM(simpleMatMulLoopsSwaped)
  }

  test("matrix multiplication with tensor cores and multiple fragments per warp produces expected result") {
    executeMM(matMulMultipleFragmentsPerWarp(), true)
    executeMM(matMulMultipleFragmentsPerWarp(), true)
  }

  test("matrix multiplication with tensor cores and shared memory produces expected result 0") {
    executeMM(matMulShared0(64, 64, 64))
  }


  test("matrix multiplication with tensor cores and shared memory produces expected result 1") {
    executeMMWithSizes(matMulSharedMemory(config1),4)
    executeMMWithSizes(matMulSharedMemory(config2),4)
  }

  test("matrix multiplication with tensor cores and shared memory produces expected result 2") {
    executeMMWithSizes(matMulSharedMemory(config3),8)
    executeMMWithSizes(matMulSharedMemory(config4),8)
    executeMMWithSizes(matMulSharedMemory(config5),8)
  }

  test("matrix multiplication with tensor cores and shared memory produces expected result 3"){
    executeMMWithSizes(matMulSharedMemory(config7),16)
    executeMMWithSizes(matMulSharedMemory(config6),16)
  }

  test("matrix multiplication with tensor cores and shared memory produces expected result 4") {
    executeMMWithSizes(matMulSharedMemoryV2(config1),4)
    executeMMWithSizes(matMulSharedMemoryV2(config2),4)
  }

  test("matrix multiplication with tensor cores and shared memory produces expected result 5") {
    executeMMWithSizes(matMulSharedMemoryV2(config3),8)
    executeMMWithSizes(matMulSharedMemoryV2(config4),8)
  }

  test("matrix multiplication with tensor cores and shared memory produces expected result 6") {
    executeMMWithSizes(matMulSharedMemoryV2(config6),16)
    executeMMWithSizes(matMulSharedMemoryV2(config7),16)
  }

  test("matrix multiplication with tensor cores and shared memory produces expected result 7") {
    executeMMWithSizes(matMulSharedMemoryV3(config1),4)
    executeMMWithSizes(matMulSharedMemoryV3(config2),4)
  }

  test("matrix multiplication with tensor cores and shared memory produces expected result 8") {
    executeMMWithSizes(matMulSharedMemoryV3(config3),8)
    executeMMWithSizes(matMulSharedMemoryV3(config4),8)
  }

  //TODO TypeCheckBug
  //Expected to find `(output : acc[n39476.n39477.f32])' in the environment: `HashMap(...,(output : acc[(2*n39476*n39477*(1/^((2*n39477)))).n39477.f32]) -> DeclRef(output), ...)'
  test("matrix multiplication with tensor cores and shared memory produces expected result 9") {
    executeMMWithSizes(matMulSharedMemoryV4(config1),4)
    executeMMWithSizes(matMulSharedMemoryV4(config2),4)
  }

  test("matrix multiplication with tensor cores and shared memory produces expected result 10") {
    executeMMWithSizes(matMulSharedMemoryV4(config3),8)
    executeMMWithSizes(matMulSharedMemoryV4(config4),8)
  }

  test("matrix multiplication with tensor cores and shared memory produces expected result 11") {
    executeMMWithSizes(matMulSharedMemoryV4(config6),16)
    executeMMWithSizes(matMulSharedMemoryV4(config7),16)
  }


  private def executeMM(mmKernel: Expr, matrixBTranspose: Boolean = false, matrixATranspose: Boolean = false) : Unit = {
    val (a, b, gold) = generateGold(m, n, k)

    val kernel = gen.cuda.kernel("mm").fromExpr(mmKernel)

    try {
        println(shine.cuda.KernelModule.translationToString(kernel))

        val run = KernelNoSizes(kernel, compilerOptions).as[ScalaFunction `(`
          Int `,` Int `,` Int `,`
          Array[Array[Float]] `,` Array[Array[Float]]
          `)=>` Array[Float]]

        val aMatrix = if (matrixATranspose) a.transpose else a
        val bMatrix = if (matrixBTranspose) b.transpose else b

        //Run test with a single warp and a single thread block
        val (output, _) =  run(LocalSize(32), GlobalSize(32))(m `,` n `,` k `,` aMatrix `,` bMatrix)

        checkResult(gold, output)
    } catch {
      case _: UnsatisfiedLinkError => System.err.println("UnsatisfiedLinkError")
    }
  }

  private def executeMMWithSizes(mmKernel: Expr, numberOfWarps: Int, matrixBTranspose: Boolean = true,
                                 matrixATranspose: Boolean = false) : Unit = {
    val (a, b, gold) = generateGold(m, n, k)

    val (localSize, globalSize) = (LocalSize(numberOfWarps * 32), GlobalSize(numberOfWarps * 32))
    val kernel = gen.cuda.kernel(localSize, globalSize, "mm").fromExpr(mmKernel)

    val compilerOptions =
      if (numberOfWarps < 16)
        mmCheckUtils.compilerOptions
      else
        mmCheckUtils.compilerOptions.appended("-maxrregcount=80")

    try {
      println(shine.cuda.KernelModule.translationToString(kernel))

      val run = KernelWithSizes(kernel, localSize, globalSize, compilerOptions).as[ScalaFunction `(`
        Int `,` Int `,` Int `,`
        Array[Array[Float]] `,` Array[Array[Float]]
        `)=>` Array[Float]]

      val aMatrix = if (matrixATranspose) a.transpose else a
      val bMatrix = if (matrixBTranspose) b.transpose else b
      val (output, _) =  run((m `,` n `,` k `,` aMatrix `,` bMatrix))

      checkResult(gold, output)
    } catch {
      case _: UnsatisfiedLinkError => System.err.println("UnsatisfiedLinkError")
    }
  }


  /**
    * Generates random data for tests and calculates gold.
    *
    * @param m first dimension for matrix multiplication
    * @param n second dimension for matrix multiplication
    * @param k third dimension for matrix multiplication
    * @return (a-matrix, b-matrix, result)
    */
  private def generateGold(m: Int, n: Int, k: Int): (Array[Array[Float]], Array[Array[Float]], Array[Float]) = {
    val rand = new scala.util.Random

    val A = Array.fill(m, k)(rand.nextFloat() * 10)
    val B = Array.fill(k, n)(rand.nextFloat() * 10)
    val gold = computeGold(A, B).flatten

    (A, B, gold)
  }

  /**
    * Multiply matrixA with matrixB using scala.
    *
    * @param matrixA first matrix factor
    * @param matrixB second matrix factor
    * @return product of matrixA and matrixB
    */
  private def computeGold(matrixA: Array[Array[Float]], matrixB: Array[Array[Float]]): Array[Array[Float]] = {
    assert(matrixA.transpose.length == matrixB.length)

    matrixA.map(rowA =>
      matrixB.transpose.map(columnB =>
        (rowA zip columnB
          map Function.tupled(_ * _)).sum))
  }
}

object mmCheckUtils {
  val compilerOptions = List("--gpu-architecture=compute_70")

  var m = 256
  var n = 512
  var k = 384

  //Test with different tiling sizes
  val config1 = mmConfig(128, 64, 128)
  val config2 = mmConfig(64, 128, 128)

  val config3 = mmConfig(128, 128, 64)
  val config4 = mmConfig(128, 128, 96)
  val config5 = mmConfig(128, 128, 128)

  val config6 = mmConfig(256, 128, 64)
  val config7 = mmConfig(128, 256, 64)

  assert(m % 256 == 0)
  assert(n % 256 == 0)
  assert(k % 128 == 0 && k % 96 == 0)

  def checkResult(gold: Array[Float], output: Array[Float]): Unit ={
    if (!test_util.similar(output, gold)) {
      println("Expected:")
      printMatrix(gold, m, n)
      println("Result:")
      printMatrix(output, m, n)

      //To detect rounding errors
      var maxDiff = 0.0f
      for (i <- gold.indices) {
        maxDiff = Math.max(Math.abs(gold(i)-output(i)), maxDiff)
      }
      println("Max difference between result and gold: " + maxDiff)

      throw new Exception("False Result: output is different from gold")
    }
  }

  /**
    * Println a matrix.
    *
    * @param matrix matrix, which should be printed
    * @param m first dimension of matrix
    * @param n second dimension of matrix
    */
  def printMatrix(matrix: Array[Float], m: Int, n: Int): Unit = {
    assert(matrix.length == m*n)

    for (i <- 0 until m) {
      println(matrix.slice(i*n, i*n+n).mkString(", "))
    }
    println("")
  }
}