package apps

import java.io.{File, FileOutputStream, PrintWriter}

import mm._
import opencl.executor.Executor
import shine.OpenCL.{GlobalSize, LocalSize}
import util.gen

object mmTuning {

  // we don't tune input size
  private val N = 64
  private val M = 128
  private val O = 128
//
//  private val N = 512
//  private val M = 1024
//  private val O = 1024

  private def randGold(): (Array[Array[Float]], Array[Array[Float]], Array[Float]) = {
    val rand = new scala.util.Random
    val At = Array.fill(O, N)(rand.nextFloat() * 10)
    val B = Array.fill(O, M)(rand.nextFloat() * 10)
    val gold = computeGold(N, M, O, At, B).flatten
    (At, B, gold)
  }

  // main
  def main(args: Array[String]): Unit = {
    Executor.loadLibrary()
    Executor.init()

    // read in values from args
    val v3 = args(0).toInt
    val v4 = args(1).toInt
    val v5 = args(2).toInt
    val v6 = args(3).toInt
    val v7 = args(4).toInt
    val v8 = args(5).toInt

    // global local size
    val ls0 = args(6).toInt
    val ls1 = args(7).toInt
    val gs0 = args(8).toInt
    val gs1 = args(9).toInt
//
//    println("v3: " + v3)
//    println("v4: " + v4)
//    println("v5: " + v5)
//    println("v6: " + v6)
//    println("v7: " + v7)
//    println("v8: " + v8)
//    println("ls0: " + ls0)
//    println("ls1: " + ls1)
//    println("gs0: " + gs0)
//    println("gs1: " + gs1)
//
    // default values
    //    val v3 = 4
//    val v4 = 8
//    val v5 = 64
//    val v6 = 128
//    val v7 = 128
//    val v8 = 16

    // generate kernel
    val kernel = genMMKernel(v3, v4, v5, v6, v7, v8)

    // create random values
    println("compute gold")
    val (at, b, gold) = randGold()
    println("compute gold finished")

    // run kernel
    try{
//      println("gen kernel")
//      val code = gen.OpenCLKernel(kernel)
//      println("execute kernel")
//      val result = runKernel(code, LocalSize((32, 8)), GlobalSize((M/4, N/8)), at, b)
//      val result = runKernel(gen.OpenCLKernel(kernel), LocalSize((32, 8)), GlobalSize((M/4, N/8)), at, b)

      val result = runKernel(gen.OpenCLKernel(kernel), LocalSize((ls0, ls1)), GlobalSize((gs0, gs1)), at, b)

//      val kernel2 = gen.OpenCLKernel(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(kernel, "")

//      val runtime = Executor.execute(, ls0, ls1, 1, gs0, gs1, 1, (at, b))

      Executor.shutdown()

      // check result
      val testSame = util.assertSame(result._1, gold, "result is different from gold")
//      println("unit: " + result._2.unit)
      println(result._2.value)

//      val costfile = new PrintWriter(new FileOutputStream(new File("/home/jo/development/lift/atf/atfc/build/costfile.txt"), false))
//      costfile.println(result._2.value.toString)
//      costfile.close()
      Executor.shutdown()
      sys.exit(0)
    }catch{
      case e:Throwable => {
        Executor.shutdown()

        println("error: " + e.getCause)

//        val costfile = new PrintWriter(new FileOutputStream(new File("/home/jo/development/lift/atf/atfc/build/costfile.txt"), false))
//        costfile.println(Double.MaxValue.toString)
//        costfile.close()

        sys.exit(1)
      }
    }
  }
}
