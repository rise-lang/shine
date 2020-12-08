package apps

import java.io.File
import java.nio.file.{Files, Path}

import apps.mmTensor._
import apps.gemmTensor._
import rise.core.Expr
import shine.OpenCL._
import test_util.Tests
import util.gen

import scala.reflect.macros.TypecheckException

class GenKernel extends Tests {

//  private val config1 = mmConfig(128, 64, 128)
//  private val config2 = mmConfig(64, 128, 128)
//
//  private val config3 = mmConfig(128, 128, 64)
//  private val config4 = mmConfig(128, 128, 96)
//  private val config5 = mmConfig(128, 128, 128)
//
//  private val config6 = mmConfig(256, 128, 64)
//  private val config7 = mmConfig(128, 256, 64)
//
//  test("genKernel"){
//    val dir = new File("kernels/tensor/")
//    if (!dir.exists())
//      dir.mkdir()
//
//    genKernel
//  }
//
//  private def genKernel: Unit = {
//    while (true){
//      try {
//        genKernelIfNotExists
//        return
//      } catch {
//        case e: TypecheckException =>
//          System.err.println("TypeCheckError")
//          e.printStackTrace()
//      }
//    }
//  }
//
//  private def genKernelIfNotExists: Unit = {
//    genKernelMM(simpleMatMul, "simpleMatMul")
//    genKernelMM(simpleMatMulBMatrixTransposed, "simpleMatMulBMatrixTransposed", true)
//    genKernelMM(simpleMatMulLoopsSwaped, "simpleMatMulLoopsSwaped")
//    genKernelMM(matMulMultipleFragmentsPerWarp(), "matMulMultipleFragmentsPerWarp", true)
//
//    genKernelMM(matMulShared0(64, 64, 64), "matMulShared0_64x64x64")
//
//    genKernelMMWithSizes(matMulSharedMemory(config1), "matMulSharedMemory_128x64x128", 4)
//    genKernelMMWithSizes(matMulSharedMemory(config2), "matMulSharedMemory_64x128x128", 4)
//
//    genKernelMMWithSizes(matMulSharedMemory(config3), "matMulSharedMemory_128x128x64", 8)
//    genKernelMMWithSizes(matMulSharedMemory(config4), "matMulSharedMemory_128x128x96", 8)
//    genKernelMMWithSizes(matMulSharedMemory(config5), "matMulSharedMemory_128x128x128", 8)
//
//    genKernelMMWithSizes(matMulSharedMemory(config6), "matMulSharedMemory_256x128x64", 16)
//    genKernelMMWithSizes(matMulSharedMemory(config7), "matMulSharedMemory_128x256x64", 16)
//
//
//    genKernelMMWithSizes(matMulSharedMemoryV2(config1), "matMulSharedMemoryV2_128x64x128", 4)
//    genKernelMMWithSizes(matMulSharedMemoryV2(config2), "matMulSharedMemoryV2_64x128x128", 4)
//
//    genKernelMMWithSizes(matMulSharedMemoryV2(config3), "matMulSharedMemoryV2_128x128x64", 8)
//    genKernelMMWithSizes(matMulSharedMemoryV2(config4), "matMulSharedMemoryV2_128x128x96", 8)
//
//    genKernelMMWithSizes(matMulSharedMemoryV2(config6), "matMulSharedMemoryV2_256x128x64", 16)
//    genKernelMMWithSizes(matMulSharedMemoryV2(config7), "matMulSharedMemoryV2_128x256x64", 16)
//
//
//    genKernelMMWithSizes(matMulSharedMemoryV3(config1), "matMulSharedMemoryV3_128x64x128", 4)
//    genKernelMMWithSizes(matMulSharedMemoryV3(config2), "matMulSharedMemoryV3_64x128x128", 4)
//
//    genKernelMMWithSizes(matMulSharedMemoryV3(config3), "matMulSharedMemoryV3_128x128x64", 8)
//    genKernelMMWithSizes(matMulSharedMemoryV3(config4), "matMulSharedMemoryV3_128x128x96", 8)
//
//
//    genKernelMMWithSizes(matMulSharedMemoryV4(config1), "matMulSharedMemoryV4_128x64x128", 4)
//    genKernelMMWithSizes(matMulSharedMemoryV4(config2), "matMulSharedMemoryV4_64x128x128", 4)
//
//    genKernelMMWithSizes(matMulSharedMemoryV4(config3), "matMulSharedMemoryV4_128x128x64", 8)
//    genKernelMMWithSizes(matMulSharedMemoryV4(config4), "matMulSharedMemoryV4_128x128x96", 8)
//
//    genKernelMMWithSizes(matMulSharedMemoryV4(config6), "matMulSharedMemoryV4_256x128x64", 16)
//    genKernelMMWithSizes(matMulSharedMemoryV4(config7), "matMulSharedMemoryV4_128x256x64", 16)
//
//
//    genKernelGEMM(simpleGemm, "simpleGemm")
//    genKernelGEMM(gemmMultipleFragmentsPerWarp, "gemmMultipleFragmentsPerWarp")
//
//
//    genKernelGEMMWithSizes(matMulSharedMemoryV4(config3), "gemmSharedMemory_128x128x64", 8)
//    genKernelGEMMWithSizes(matMulSharedMemoryV4(config4), "gemmSharedMemory_128x128x96", 8)
//
//    genKernelGEMMWithSizes(matMulSharedMemoryV4(config6), "gemmSharedMemory_256x128x64", 16)
//    genKernelGEMMWithSizes(matMulSharedMemoryV4(config7), "gemmSharedMemory_128x256x64", 16)
//
//
//    genKernelGEMMWithSizes(matMulSharedMemoryV4(config3), "gemmSharedMemoryV2_128x128x64", 8)
//    genKernelGEMMWithSizes(matMulSharedMemoryV4(config4), "gemmSharedMemoryV2_128x128x96", 8)
//
//    genKernelGEMMWithSizes(matMulSharedMemoryV4(config6), "gemmSharedMemoryV2_256x128x64", 16)
//    genKernelGEMMWithSizes(matMulSharedMemoryV4(config7), "gemmSharedMemoryV2_128x256x64", 16)
//  }
//
//  private def genKernelMM(mmKernel: Expr, name: String, matrixBTranspose: Boolean = false) : Unit = genKernel(mmKernel, true, name, matrixBTranspose)
//  private def genKernelGEMM(mmKernel: Expr, name: String, matrixBTranspose: Boolean = false) : Unit = genKernel(mmKernel, false, name, matrixBTranspose)
//  private def genKernelMMWithSizes(mmKernel: Expr, name: String, warps: Int, matrixBTranspose: Boolean = true) : Unit = genKernel(mmKernel, true, name, 72, warps, matrixBTranspose)
//  private def genKernelGEMMWithSizes(mmKernel: Expr, name: String, warps: Int, matrixBTranspose: Boolean = true) : Unit = genKernel(mmKernel, false, name, 72, warps, matrixBTranspose)
//
//  private def genKernel(mmKernel: Expr, matmul: Boolean, name: String, matrixBTranspose: Boolean = false) : Unit = {
//    val file = new File("kernels/tensor/" + name)
//
//    if (!file.exists()){
//      val code = gen.cuKernel(mmKernel, if (matmul) "matmul" else "gemm").code
//
//      val sb = new StringBuilder()
//      if (matmul)
//        sb.append("//Matrixmultiplikationskernel\n")
//      else
//        sb.append("//GEMMKernel\n")
//      sb.append("//Name: " + name + "\n")
//      sb.append("//B-Matrix transpoiniert? " + matrixBTranspose + "\n\n")
//
//      sb.append(code)
//      sb.append("\n")
//      Files.write(new File("kernels/tensor/" + name).toPath, sb.toString.getBytes())
//    }
//  }
//
//  private def genKernel(mmKernel: Expr, matmul: Boolean, name: String, blocks: Int, warps: Int, matrixBTranspose: Boolean) : Unit = {
//    val file = new File("kernels/tensor/" + name)
//
//    if (!file.exists()){
//      val code = gen.cuKernel(LocalSize(warps*32), GlobalSize(warps*32*blocks))(mmKernel, if (matmul) "matmul" else "gemm").code
//
//      val sb = new StringBuilder()
//      if (matmul)
//        sb.append("//Matrixmultiplikationskernel\n")
//      else
//        sb.append("//GEMMKernel\n")
//      sb.append("//Name: " + name + "\n")
//      sb.append("//B-Matrix transpoiniert? " + matrixBTranspose + "\n\n")
//
//      sb.append("//Feste Anzahl an zu startenden Threads und Blöcken benötigt:\n")
//      sb.append("//Blöcke: " + blocks + "\n")
//      sb.append("//Warps: " + warps + " (" + (warps*32) + " Threads) \n\n")
//
//      sb.append(code)
//      sb.append("\n")
//      Files.write(new File("kernels/tensor/" + name).toPath, sb.toString.getBytes())
//    }
//  }
}
