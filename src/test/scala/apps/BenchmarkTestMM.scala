package apps

import mmTensor._
import gemmTensor._
import rise.core.Expr
import shine.OpenCL.{GlobalSize, LocalSize, NDRange}
import util.{KernelArgCreator, gen}
import yacx.{Device, Devices}

class BenchmarkTestMM extends BenchmarkTest {
//  private val test = false
//  private var palma = false
//
//  val l1 = 44
//  var dataSizes96 = new Array[Long](l1-1)
//  for (i <- 1 until l1)
//    dataSizes96(i-1) = 384*i*384*i*4
//
//  val l2 = 66*2
//  var dataSizes128 = new Array[Long](l2-1)
//  for (i <- 1 until l2)
//    dataSizes128(i-1) = 128*i*128*i*4
//
//  val l3 = 66
//  var dataSizes256 = new Array[Long](l3-1)
//  for (i <- 1 until l3)
//    dataSizes256(i-1) = 256*i*256*i*4
//
//
//  dataSizes256 = Array[Long](768*768*4, 1536*1536*4, 2304*2304*4, 4608*4608*4, 6144*6144*4, 8448*8448*4)
//  dataSizes128 = Array[Long](768*768*4, 1536*1536*4, 2304*2304*4, 4608*4608*4, 6144*6144*4, 8448*8448*4)
//  dataSizes96 =  Array[Long](768*768*4, 1536*1536*4, 2304*2304*4, 4608*4608*4, 6144*6144*4, 8448*8448*4)
//
//  dataSizes = dataSizes128
//  iterations = 50
//
//  test("run warmup") {
//    //Ignore this test-results
//    benchmarkYacx("fastGemm", "fastGemm.cu", "nvidiaOptimizedTensorGEMM", new FastGemmBenchmark)
//
//    //Run tests for speedup comparison
//    LatexPrinter.FASTGEMMBENCHMATK = benchmarkYacx("fastGemm", "fastGemm.cu", "nvidiaOptimizedTensorGEMM", new FastGemmBenchmark)
////    LatexPrinter.SIMPLEGEMMBENCHMARK = benchmarkYacx(simpleMatMul, "simpleMatMul4Warps", MMCreator(4))
//  }
//
//  test("run tests"){
//    latexPrinter = LatexFilePrinter("kernels")
//    benchmarkYacx("fastGemm", "fastGemm.cu", "nvidiaOptimizedTensorGEMM", new FastGemmBenchmark)
//
//
////    benchmark("matMulSharedMemory_128x64x128")
////    benchmark("matMulSharedMemory_64x128x128")
//
//    benchmark("matMulSharedMemory_128x128x64")
////    benchmark("matMulSharedMemory_128x128x96")
//    benchmark("matMulSharedMemory_128x128x128")
//
////    benchmark("matMulSharedMemory_256x128x64")
////    benchmark("matMulSharedMemory_128x256x64")
//
//
//    benchmark("gemmSharedMemory_128x128x64")
//    benchmark("gemmSharedMemory_128x128x96")
//
//
////    benchmark("gemmSharedMemory_256x128x64")
////    benchmark("gemmSharedMemory_128x256x64")
//
//
//    benchmark("gemmSharedMemoryV2_128x128x64")
//    benchmark("gemmSharedMemoryV2_128x128x96")
//
//    benchmark("gemmSharedMemoryV2_256x128x64")
////    benchmark("gemmSharedMemoryV2_128x256x64")
//
//
////    benchmark("matMulSharedMemoryV2_128x64x128")
////    benchmark("matMulSharedMemoryV2_64x128x128")
//
//    benchmark("matMulSharedMemoryV2_128x128x64")
////    benchmark("matMulSharedMemoryV2_128x128x96")
//
////    benchmark("matMulSharedMemoryV2_256x128x64")
////    benchmark("matMulSharedMemoryV2_128x256x64")
//
//
////    benchmark("matMulSharedMemoryV3_128x64x128")
////    benchmark("matMulSharedMemoryV3_64x128x128")
//
//    benchmark("matMulSharedMemoryV3_128x128x64")
////    benchmark("matMulSharedMemoryV3_128x128x96")
//
//
////    benchmark("matMulSharedMemoryV4_128x64x128")
////    benchmark("matMulSharedMemoryV4_64x128x128")
//
//    benchmark("matMulSharedMemoryV4_128x128x64")
////    benchmark("matMulSharedMemoryV4_128x128x96")
//
////    benchmark("matMulSharedMemoryV4_256x128x64")
////    benchmark("matMulSharedMemoryV4_128x256x64")
//  }
//
//  test("performance simple") {
//    latexPrinter = LatexFilePrinter("Simple")
//    benchmarkYacx("simpleGemm", "simpleGemm.cu","nvidiaSimpleTensorGEMM4Warps", new SimpleGemmBenchmark(4))
//    benchmarkYacx(simpleMatMul, "simpleMatMul4Warps", MMCreator(4))
//    benchmarkYacx(simpleMatMulBMatrixTransposed, "simpleMatMulBMatrixTransposed4Warps", MMCreator(4))
//    benchmarkYacx(simpleMatMulLoopsSwaped, "simpleMatMulLoopsSwaped4Warps", MMCreator(4))
//
//    benchmarkYacx("simpleGemm","simpleGemm.cu", "nvidiaSimpleTensorGEMM8Warps", new SimpleGemmBenchmark(8))
//    benchmarkYacx(simpleMatMul, "simpleMatMul8Warps", MMCreator(8))
//    benchmarkYacx(simpleMatMulBMatrixTransposed, "simpleMatMulBMatrixTransposed8Warps", MMCreator(8))
//    benchmarkYacx(simpleMatMulLoopsSwaped, "simpleMatMulLoopsSwaped8Warps", MMCreator(8))
//
//
//    latexPrinter = LatexFilePrinter("Tiling")
//    benchmarkYacx("simpleGemm","simpleGemm.cu", "nvidiaSimpleTensorGEMM4Warps", new SimpleGemmBenchmark(4))
//    benchmarkYacx(matMulMultipleFragmentsPerWarp(), "matMulMultipleFragmentsPerWarp2Warps", MMCreator(2))
//    benchmarkYacx(matMulMultipleFragmentsPerWarp(), "matMulMultipleFragmentsPerWarp4Warps", MMCreator(4))
//    benchmarkYacx(matMulMultipleFragmentsPerWarp(), "matMulMultipleFragmentsPerWarp8Warps", MMCreator(8))
//    dataSizes = dataSizes96
//    benchmarkYacx(matMulMultipleFragmentsPerWarp(64, 48, 2*64, 4*48), "matMulMultipleFragmentsPerWarp8WarpsTiling2", MMCreator(8))
//    benchmarkYacx(matMulMultipleFragmentsPerWarp(48, 48, 2*48, 4*48), "matMulMultipleFragmentsPerWarp8WarpsTiling3", MMCreator(8))
//  }
//
//
//  private def benchmark(name: String): Unit = {
//    val (config, warps) =
//      if (name.endsWith("128x64x128")){
//        dataSizes = dataSizes128
//        (mmConfig(128, 64, 128), 4)
//      } else if (name.endsWith("64x128x128")){
//        dataSizes = dataSizes128
//        (mmConfig(64, 128, 128), 4)
//      } else if (name.endsWith("128x128x64")){
//        dataSizes = dataSizes128
//        (mmConfig(128, 128, 64), 8)
//      } else if (name.endsWith("128x128x96")){
//        dataSizes = dataSizes96
//        (mmConfig(128, 128, 96), 8)
//      } else if (name.endsWith("128x128x128")){
//        dataSizes = dataSizes128
//        (mmConfig(128, 128, 128), 8)
//      } else if (name.endsWith("256x128x64")){
//        dataSizes = dataSizes256
//        (mmConfig(256, 128, 64), 16)
//      } else if (name.endsWith("128x256x64")){
//        if (palma)
//          return
//        dataSizes = dataSizes256
//        (mmConfig(128, 256, 64), 16)
//      } else {
//        ???
//      }
//
//    val (kernel, creator) =
//      if (name.startsWith("matMulSharedMemory_")){
//        (matMulSharedMemory(config), MMCreator(warps))
//      } else if (name.startsWith("matMulSharedMemoryV2_")) {
//        (matMulSharedMemoryV2(config), MMCreator(warps))
//      } else if (name.startsWith("matMulSharedMemoryV3_")) {
//        (matMulSharedMemoryV3(config), MMCreator(warps))
//      } else if (name.startsWith("matMulSharedMemoryV4_")) {
//        (matMulSharedMemoryV4(config), MMCreator(warps))
//      } else if (name.startsWith("gemmSharedMemory_")) {
//        if (!palma || warps != 16)
//          (gemmSharedMemory(config), GEMMCreator(warps))
//        else
//          return
//      } else if (name.startsWith("gemmSharedMemoryV2_")) {
//        if (!palma || warps != 16)
//          (gemmSharedMemoryV2(config), GEMMCreator(warps))
//        else
//          return
//      } else {
//        ???
//      }
//
//      benchmarkYacxWithSizes(kernel, name, creator)
//
//  }
  
//  test("performance shared memory V1"){
//    latexPrinter = LatexFilePrinter("SharedV1")
//    benchmark("matMulSharedMemory_128x64x128")
//    benchmark("matMulSharedMemory_64x128x128")
//
//    benchmark("matMulSharedMemory_128x128x64")
//    benchmark("matMulSharedMemory_128x128x96")
//    benchmark("matMulSharedMemory_128x128x128")
//
//    benchmark("matMulSharedMemory_256x128x64")
//    benchmark("matMulSharedMemory_128x256x64")
//  }
//
//  test("performance shared memory V2"){
//    latexPrinter = LatexFilePrinter("SharedV2")
//    benchmark("matMulSharedMemoryV2_128x64x128")
//    benchmark("matMulSharedMemoryV2_64x128x128")
//
//    benchmark("matMulSharedMemoryV2_128x128x64")
//    benchmark("matMulSharedMemoryV2_128x128x96")
//
//    benchmark("matMulSharedMemoryV2_256x128x64")
//    benchmark("matMulSharedMemoryV2_128x256x64")
//  }
//
//  test("performance shared memory V3"){
//    latexPrinter = LatexFilePrinter("SharedV3")
//    benchmark("matMulSharedMemoryV3_128x64x128")
//    benchmark("matMulSharedMemoryV3_64x128x128")
//
//    benchmark("matMulSharedMemoryV3_128x128x64")
//    benchmark("matMulSharedMemoryV3_128x128x96")
//  }
//
//  test("performance shared memory V4"){
//    latexPrinter = LatexFilePrinter("SharedV4")
//    benchmark("matMulSharedMemoryV4_128x64x128")
//    benchmark("matMulSharedMemoryV4_64x128x128")
//
//    benchmark("matMulSharedMemoryV4_128x128x64")
//    benchmark("matMulSharedMemoryV4_128x128x96")
//
//    benchmark("matMulSharedMemoryV4_256x128x64")
//    benchmark("matMulSharedMemoryV4_128x256x64")
//  }
//
////  //???
////  v2 ist hier am schnellsten und v4 am langsamsten
////  bei amderen tests war v4 die schnellste Version
//  test("performance test epilog"){
//    val tmp = dataSizes
//    val k = 64
//    val mn = 24576
//    dataSizes = Array[Long](16384*k*4, mn*k*4)
//
//    latexPrinter = LatexFilePrinter("SharedSmallK")
//    benchmarkYacxWithSizes(matMulSharedMemoryV2(mmConfig(128, 128, 64)), "matMulSharedMemoryV2_128x128x64SmallK", MMCreatorSmallK(k, 8))
//
//    benchmarkYacxWithSizes(matMulSharedMemoryV3(mmConfig(128, 128, 64)), "matMulSharedMemoryV3_128x128x64SmallK", MMCreatorSmallK(k, 8))
//
//    benchmarkYacxWithSizes(matMulSharedMemoryV4(mmConfig(128, 128, 64)), "matMulSharedMemoryV4_128x128x64SmallK", MMCreatorSmallK(k, 8))
//
//    dataSizes = tmp
//  }
//
//  test("performance gemm shared memory V1"){
//    latexPrinter = LatexFilePrinter("GemmSharedV1")
//    benchmark("gemmSharedMemory_128x128x64")
//    benchmark("gemmSharedMemory_128x128x96")
//
//    benchmark("gemmSharedMemory_256x128x64")
//    benchmark("gemmSharedMemory_128x256x64")
//  }
//
//  test("performance gemm shared memory V2"){
//    latexPrinter = LatexFilePrinter("GemmSharedV2")
//    benchmark("gemmSharedMemoryV2_128x128x64")
//    benchmark("gemmSharedMemoryV2_128x128x96")
//
//    benchmark("gemmSharedMemoryV2_256x128x64")
//    benchmark("gemmSharedMemoryV2_128x256x64")
//  }
//
//  test("performance rise vs hand optimzed"){
//    latexPrinter = LatexFilePrinter("SharedFast")
//    benchmarkYacx("fastGemm", "fastGemm.cu",  "nvidiaOptimizedTensorGEMM", new FastGemmBenchmark)
//    benchmark("gemmSharedMemory_128x128x64")
//    benchmark("gemmSharedMemory_128x128x96")
//
//    benchmark("gemmSharedMemory_256x128x64")
//    benchmark("gemmSharedMemory_128x256x64")
//
//
//    benchmark("gemmSharedMemoryV2_128x128x64")
//    benchmark("gemmSharedMemoryV2_128x128x96")
//
//    benchmark("gemmSharedMemoryV2_256x128x64")
//    benchmark("gemmSharedMemoryV2_128x256x64")
//  }
//
//  test("performance mm vs gemm") {
//    latexPrinter = LatexFilePrinter("GEMM")
//    benchmark("matMulSharedMemoryV4_128x128x64")
//    benchmark("matMulSharedMemoryV4_128x128x96")
//
//    benchmark("matMulSharedMemoryV4_256x128x64")
//    benchmark("matMulSharedMemoryV4_128x256x64")
//
//    benchmark("gemmSharedMemoryV2_128x128x64")
//    benchmark("gemmSharedMemoryV2_128x128x96")
//
//    benchmark("gemmSharedMemoryV2_256x128x64")
//    benchmark("gemmSharedMemoryV2_128x256x64")
//  }
//
//  test("fastGemm vs fastGemmOrginal test") {
//    latexPrinter = LatexFilePrinter("kernel")
//    val tmp = dataSizes
//    dataSizes = Array[Long](4096*4096*4)
//    benchmarkYacx("fastGemm", "fastGemm.cu",  "nvidiaOptimizedTensorGEMM", new FastGemmBenchmark)
//    benchmarkYacx("fastGemmOriginal", "fastGemmOriginal.cu", "nvidiaOptimizedTensorGEMMOriginal.cu", new FastGemmBenchmarkOriginal)
//    dataSizes = tmp
//  }
}

