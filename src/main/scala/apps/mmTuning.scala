package apps

import mm._
import opencl.executor.Executor
import shine.OpenCL.{GlobalSize, LocalSize}
import util.gen

object mmTuning {

  // we don't tune input size
  private val N = 64
  private val M = 128
  private val O = 128

//  private val N = 1024
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
//
//    println("v3: " + v3)
//    println("v4: " + v4)
//    println("v5: " + v5)
//    println("v6: " + v6)
//    println("v7: " + v7)
//    println("v8: " + v8)

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
    val (at, b, gold) = randGold()

    // run kernel
    try{
//      println("gen kernel")
//      val code = gen.OpenCLKernel(kernel)
//      println("execute kernel")
//      val result = runKernel(code, LocalSize((32, 8)), GlobalSize((M/4, N/8)), at, b)
      val result = runKernel(gen.OpenCLKernel(kernel), LocalSize((32, 8)), GlobalSize((M/4, N/8)), at, b)
      Executor.shutdown()

      // check result
      val testSame = util.assertSame(result._1, gold, "result is different from gold")
      println(result._2.value)
      sys.exit(0)
    }catch{
      case e:Throwable => {
        Executor.shutdown()
        sys.exit(1)
      }
    }
  }
}
