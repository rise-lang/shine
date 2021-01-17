package apps

import mmTensor._
import gemmTensor._
import org.scalatest.BeforeAndAfterAll
import rise.core.Expr
import shine.OpenCL.{LocalSize, NDRange}
import test_util.Tests
import util.{KernelArgCreator, gen}
import yacx.Executor.BenchmarkResult
import yacx._

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.immutable.ListMap

class BenchmarkTestTensorMM extends Tests with BeforeAndAfterAll {
  private val compilerOptions = scala.Array("--gpu-architecture=compute_70",
    //TODO only for run on palma
    "--include-path=/Applic.HPC/Easybuild/skylake/2019a/software/CUDA/10.1.105-GCC-8.2.0-2.31.1/targets/x86_64-linux/include",
    //doris
    "--include-path=/opt/cuda/targets/x86_64-linux/include")

  var dataSizes = Array[Long](768*768*4, 1536*1536*4, 2304*2304*4, 4608*4608*4, 6144*6144*4, 8448*8448*4)
  val iterations = 50

  var results: Map[String, BenchmarkResult] = Map.empty

  override def beforeAll() : Unit = {
    println("Initizialize Executor...")
    yacx.Executor.loadLibrary()
    val device = Devices.findDevice()

    println("Using GPU:")
    println(device)
    println("MultiprocessorCount:" + device.getMultiprocessorCount)
    println("SharedMemory per block: " + device.getSharedMemPerBlock)
    println("SharedMemory per SM: " + device.getSharedMemPerMultiprocessor)
  }

  override def afterAll(): Unit = {
    println("Benchmark tests completed\n")
    printResults(results)
  }

  private def benchmark(kernelExpression: Expr, name:String, kernelArgCreator: KernelArgCreator) : BenchmarkResult = {
    println(s"Benchmark $name")

    val kernel = gen.cuKernel(kernelExpression, name)

    compilerOptions.foreach(option => kernel.kernel.addCompilerOption(option))

    val benchmarkResult = kernel.kernel.benchmark(kernelArgCreator, iterations, dataSizes)

    results += (name -> benchmarkResult)

    benchmarkResult
  }

  private def benchmark(kernelName: String, kernelFile: String, name: String, kernelArgCreator: Executor.KernelArgCreator) : BenchmarkResult = {
    println(s"Benchmark $name (not shine-generated)")

    val code = new String(java.nio.file.Files.readAllBytes(new File("kernels/", kernelFile).toPath))

    val benchmarkResult = Executor.benchmark(code, kernelName, Options.createOptions(compilerOptions:_*),  Devices.findDevice, iterations, kernelArgCreator, dataSizes:_*)

    results += (name -> benchmarkResult)

    benchmarkResult
  }

  test("warmup") {
    val kernel = gen.cuKernel(matMulMultipleFragmentsPerWarp(), "mm")

    compilerOptions.foreach(option => kernel.kernel.addCompilerOption(option))

    kernel.kernel.benchmark(MMCreator(8), iterations, dataSizes)
  }

  ignore("performance slow kernels") {
    benchmark("simpleGemm", "simpleGemm.cu","nvidiaSimpleTensorGEMM4Warps", ExternelCodeGemmBenchmark(4))
    benchmark("simpleGemm","simpleGemm.cu", "nvidiaSimpleTensorGEMM8Warps", ExternelCodeGemmBenchmark(8))

    benchmark(simpleMatMul, "simpleMatMul4Warps", MMCreator(4))
    benchmark(simpleMatMulBMatrixTransposed, "simpleMatMulBMatrixTransposed4Warps", MMCreator(4))
    benchmark(simpleMatMulLoopsSwaped, "simpleMatMulLoopsSwaped4Warps", MMCreator(4))

    benchmark(simpleMatMul, "simpleMatMul8Warps", MMCreator(8))
    benchmark(simpleMatMulBMatrixTransposed, "simpleMatMulBMatrixTransposed8Warps", MMCreator(8))
    benchmark(simpleMatMulLoopsSwaped, "simpleMatMulLoopsSwaped8Warps", MMCreator(8))

    benchmark(matMulMultipleFragmentsPerWarp(), "matMulMultipleFragmentsPerWarp2Warps", MMCreator(2))
    benchmark(matMulMultipleFragmentsPerWarp(), "matMulMultipleFragmentsPerWarp4Warps", MMCreator(4))
    benchmark(matMulMultipleFragmentsPerWarp(), "matMulMultipleFragmentsPerWarp8Warps", MMCreator(8))

    benchmark(matMulMultipleFragmentsPerWarp(64, 48, 2*64, 4*48), "matMulMultipleFragmentsPerWarp8WarpsTiling2", MMCreator(8))
    benchmark(matMulMultipleFragmentsPerWarp(48, 48, 2*48, 4*48), "matMulMultipleFragmentsPerWarp8WarpsTiling3", MMCreator(8))
  }


  private def benchmark(name: String): Unit = {
    val (config, warps) =
      if (name.endsWith("128x64x128")){
        (mmConfig(128, 64, 128), 4)
      } else if (name.endsWith("64x128x128")){
        (mmConfig(64, 128, 128), 4)
      } else if (name.endsWith("128x128x64")){
        (mmConfig(128, 128, 64), 8)
      } else if (name.endsWith("128x128x96")){
        (mmConfig(128, 128, 96), 8)
      } else if (name.endsWith("128x128x128")){
        (mmConfig(128, 128, 128), 8)
      } else if (name.endsWith("256x128x64")){
        (mmConfig(256, 128, 64), 16)
      } else if (name.endsWith("128x256x64")){
        (mmConfig(128, 256, 64), 16)
      } else {
        ???
      }

    val (kernel: Expr, creator) =
      if (name.startsWith("matMulSharedMemory_")){
        (matMulSharedMemory(config), MMCreator(warps))
      } else if (name.startsWith("matMulSharedMemoryV2_")) {
        (matMulSharedMemoryV2(config), MMCreator(warps))
      } else if (name.startsWith("matMulSharedMemoryV3_")) {
        (matMulSharedMemoryV3(config), MMCreator(warps))
      } else if (name.startsWith("matMulSharedMemoryV4_")) {
        (matMulSharedMemoryV4(config), MMCreator(warps))
      } else if (name.startsWith("gemmSharedMemory_")) {
        (gemmSharedMemory(config), GEMMCreator(warps))
      } else if (name.startsWith("gemmSharedMemoryV2_")) {
        (gemmSharedMemoryV2(config), GEMMCreator(warps))
      } else {
        ???
      }

      benchmark(kernel, name, creator)

  }

  test("performance mm shared memory V1"){
    benchmark("matMulSharedMemory_128x64x128")
    benchmark("matMulSharedMemory_64x128x128")

    benchmark("matMulSharedMemory_128x128x64")
    benchmark("matMulSharedMemory_128x128x96")
    benchmark("matMulSharedMemory_128x128x128")

    benchmark("matMulSharedMemory_256x128x64")
    benchmark("matMulSharedMemory_128x256x64")
  }

  test("performance mm shared memory V2"){
    benchmark("matMulSharedMemoryV2_128x64x128")
    benchmark("matMulSharedMemoryV2_64x128x128")

    benchmark("matMulSharedMemoryV2_128x128x64")
    benchmark("matMulSharedMemoryV2_128x128x96")

    benchmark("matMulSharedMemoryV2_256x128x64")
    benchmark("matMulSharedMemoryV2_128x256x64")
  }

  test("performance mm shared memory V3"){
    benchmark("matMulSharedMemoryV3_128x64x128")
    benchmark("matMulSharedMemoryV3_64x128x128")

    benchmark("matMulSharedMemoryV3_128x128x64")
    benchmark("matMulSharedMemoryV3_128x128x96")
  }

  test("performance mm shared memory V4"){
    benchmark("matMulSharedMemoryV4_128x64x128")
    benchmark("matMulSharedMemoryV4_64x128x128")

    benchmark("matMulSharedMemoryV4_128x128x64")
    benchmark("matMulSharedMemoryV4_128x128x96")

    benchmark("matMulSharedMemoryV4_256x128x64")
    benchmark("matMulSharedMemoryV4_128x256x64")
  }

  test("performance gemm shared memory V1"){
    benchmark("gemmSharedMemory_128x128x64")
    benchmark("gemmSharedMemory_128x128x96")

    benchmark("gemmSharedMemory_256x128x64")
    benchmark("gemmSharedMemory_128x256x64")
  }

  test("performance gemm shared memory V2"){
    benchmark("gemmSharedMemoryV2_128x128x64")
    benchmark("gemmSharedMemoryV2_128x128x96")

    benchmark("gemmSharedMemoryV2_256x128x64")
    benchmark("gemmSharedMemoryV2_128x256x64")
  }

  test("nvidia kernel test") {
    val tmp = dataSizes
    dataSizes = Array[Long](4096*4096*4)
    benchmark("fastGemm", "fastGemm.cu",  "nvidiaOptimizedTensorGEMM", NvidiaFastGemmBenchmark())
    benchmark("fastGemmOriginal", "fastGemmOriginal.cu", "nvidiaOptimizedTensorGEMMOriginal.cu", NvidiaFastGemmBenchmarkOriginal())
    dataSizes = tmp
  }



  val rand = new scala.util.Random
  val alpha = 2f
  val beta = 3f

  def multiprocessors : Int = Devices.findDevice.getMultiprocessorCount

  def createMatrix(m: Int, n: Int) : Array[Array[Float]] = {
    Array.fill(m, n)(rand.nextFloat() * 10)
  }

  class MMCreator(warps: Int, blocks: Int) extends KernelArgCreator {

    override def getDataLength(dataSizeBytes: Long): Int = Math.sqrt(dataSizeBytes/FloatArg.SIZE_BYTES.toDouble).asInstanceOf[Int]

    override def createArgs(dim: Int): Array[Any] = {
      println(s"CreateArgs: m = n = k = $dim")

      val a = createMatrix(dim, dim)
      val b = createMatrix(dim, dim)

      Array[Any](dim, dim, dim, a, b)
    }

    override def getGridDim(dataLength: Int): NDRange = NDRange(blocks, 1, 1)

    override def getBlockDim(dataLength: Int): LocalSize = LocalSize(32*warps)
  }

  object MMCreator {
    def apply(warps: Int, blocks: Int = multiprocessors) : MMCreator = new MMCreator(warps, blocks)
  }

  case class GEMMCreator(warps: Int, blocks: Int = multiprocessors)
    extends MMCreator(warps, blocks) {

    override def createArgs(dim: Int): Array[Any] = {
      println(s"CreateArgs: m = n = k = $dim")

      val a = createMatrix(dim, dim)
      val b = createMatrix(dim, dim)
      val c = createMatrix(dim, dim)

      Array[Any](dim, dim, dim, alpha, beta, a, b, c)
    }
  }

  class ExternelCodeGemmBenchmark(warps: Int, blocks: Int) extends Executor.KernelArgCreator {
    override def getDataLength(dataSizeBytes: Long): Int = Math.sqrt(dataSizeBytes / FloatArg.SIZE_BYTES.toDouble).toInt

    override def getGrid0(dim: Int): Int = blocks

    override def getBlock0(dim: Int): Int = warps * 32

    override def createArgs(dim: Int): Array[KernelArg] = {
      println(s"CreateArgs: m = n = k = $dim")

      val a = createMatrix(dim, dim).flatten
      val b = createMatrix(dim, dim).flatten
      val c = createMatrix(dim, dim).flatten

      val aMatrixArg = HalfArg.create(a:_*)
      val bMatrixArg = HalfArg.createTransposed(b, dim, dim)
      val cMatrixArg = FloatArg.create(c: _*)
      val dMatrixArg = FloatArg.createOutput(dim * dim)
      val mArg = IntArg.createValue(dim)
      val nArg = IntArg.createValue(dim)
      val kArg = IntArg.createValue(dim)
      val alphaArg = FloatArg.createValue(alpha)
      val betaArg = FloatArg.createValue(beta)

      Array[KernelArg](aMatrixArg, bMatrixArg, cMatrixArg, dMatrixArg, mArg, nArg, kArg, alphaArg, betaArg)
    }
  }

  object ExternelCodeGemmBenchmark {
    def apply(warps: Int, blocks: Int = multiprocessors) : ExternelCodeGemmBenchmark = new ExternelCodeGemmBenchmark(warps, blocks)
  }

  class NvidiaFastGemmBenchmark(blocks: Int)
    extends ExternelCodeGemmBenchmark(8, blocks) {
    // Constants for shared memory calculation
    val M = 16
    val N = 16
    val K = 16
    // If you change this, don't forget to adjust the SHARED_MEMORY_LIMIT_64K in the
    // kernel, too.
    val SHARED_MEMORY_LIMIT_64K = true
    val BLOCK_ROW_WARPS = 2
    val BLOCK_COL_WARPS = 4
    val WARP_ROW_TILES = 4
    val WARP_COL_TILES = 2
    val BLOCK_COL_TILES: Int = WARP_COL_TILES * BLOCK_COL_WARPS
    val CHUNK_K: Int = if (SHARED_MEMORY_LIMIT_64K) 4 else 8
    val SKEW_HALF = 8
    // Get Device
    val device: Device = Devices.findDevice

    // Compute required shared memory size
    val SHMEM_SZ: Long = Math.max(HalfArg.SIZE_BYTES * (BLOCK_COL_TILES * M) * (CHUNK_K * K + SKEW_HALF) * 2, M *
      (BLOCK_ROW_WARPS * WARP_ROW_TILES) * N * (BLOCK_COL_WARPS * WARP_COL_TILES) * FloatArg.SIZE_BYTES)

    // Calculate and print out the required and available shared memory size
    val required: Long = SHMEM_SZ / 1024
    val available: Long = device.getSharedMemPerMultiprocessor / 1024
    System.out.println("Required shared memory size per multiprocessor: " + required + " KB")
    System.out.println("Available shared memory size per multiprocessor: " + available + " KB")

    // Check if there's enough shared memory per block available on the device for
    // this kernel
    if (required > available) {
      System.out.println("Not enough shared memory per block available on the device for this kernel! Abort!")
      System.out.println("Please use the simple GEMM kernel instead or increase the amount of shared memory per block if possible!")
      System.exit(1)
    }

    override def getSharedMemory(dataSizeBytes: Long): Long = SHMEM_SZ
  }

  object NvidiaFastGemmBenchmark {
    def apply(blocks: Int = multiprocessors) : NvidiaFastGemmBenchmark = new NvidiaFastGemmBenchmark(blocks)
  }

  case class NvidiaFastGemmBenchmarkOriginal(blocks: Int = multiprocessors) extends NvidiaFastGemmBenchmark(blocks) {
    override def createArgs(dim: Int): Array[KernelArg] = {
      assert(dim == 16*256)

      val args = super.createArgs(dim)
      Array[KernelArg](args(0), args(1), args(2), args(3), args(7), args(8))
    }
  }


  def printResults(results: Map[String, BenchmarkResult], dataSizeSort: Int = -1) : Unit = {
    if (results.isEmpty){
      println("No results")
      return
    }

    val dataSizeSortIndex =
      if (dataSizeSort == -1)
        results.head._2.getDataSizes.length-1
      else
        results.head._2.getDataSizes.indexOf(dataSizeSort)

    val sorted = ListMap(results.toSeq.sortWith(_._2.getAverage()(dataSizeSortIndex).getLaunch >= _._2.getAverage()(dataSizeSortIndex).getLaunch):_*)

    println(s"Name:               Average launch with datasize $dataSizeSort   standardDeviation")
    sorted foreach {case (name, result) => {
      var nameLength = name.length
      print("Name: " + name)

      while (nameLength < 15) {
        print(" ")
        nameLength += 1
      }

      print(result.getAverage()(dataSizeSortIndex).getLaunch)
      print(" ")
      print(getMinMaxSderivation(result.getResult()(dataSizeSortIndex), result.getAverage()(dataSizeSortIndex))._2)
    }}
  }

  //Todo move this function to yacx
  def toCsv(benchmarkResult: BenchmarkResult, name: String, baseLine: Option[BenchmarkResult] = None): Unit = {
    val file = new File(name)

    val filePrinter = new BufferedWriter(new FileWriter(file))

    filePrinter.write("%Benchmark " + name)
    filePrinter.newLine()
    filePrinter.newLine()
    filePrinter.write("dimension execution-time baseline total-time upload-time download-time min max standard-deviation")
    filePrinter.newLine()

    for (i <- benchmarkResult.getDataSizes.indices) {
      val dataSize = benchmarkResult.getDataSizes()(i)
      val kernelTime = benchmarkResult.getAverage()(i)

      filePrinter.write("" + Math.sqrt(dataSize/4d).toInt)
      filePrinter.write(" " + kernelTime.getLaunch)
      if (baseLine.isDefined)
        filePrinter.write(" " + (baseLine.get.getAverage()(i).getLaunch / kernelTime.getLaunch))
      else
        filePrinter.write(" " + 0)
      filePrinter.write(" " + kernelTime.getTotal)
      filePrinter.write(" " + kernelTime.getUpload)
      filePrinter.write(" " + kernelTime.getDownload)

      val resultsI = benchmarkResult.getResult()(i)
      val (min, max, sderivation) = getMinMaxSderivation(benchmarkResult.getResult()(i), benchmarkResult.getAverage()(i))

      filePrinter.write(" " + min)
      filePrinter.write(" " + max)
      filePrinter.write(" " + sderivation)

      filePrinter.newLine()
    }

    filePrinter.close()
  }

  def getMinMaxSderivation(times: Array[KernelTime], average: KernelTime) : (Double, Double, Double) = {
    var max = times(0)
    var min = times(0)
    var variance = 0D

    for (j <- times.indices) {
      val launchIJ = times(j)
      val launchIJLaunch = launchIJ.getLaunch

      if (launchIJLaunch > max.getLaunch)
        max = launchIJ

      if (launchIJLaunch < min.getLaunch)
        min = launchIJ

      variance += (launchIJLaunch - average.getLaunch) * (launchIJLaunch -average.getLaunch)
    }

    (min.getLaunch, max.getLaunch, Math.sqrt(variance / times.length))
  }
}

