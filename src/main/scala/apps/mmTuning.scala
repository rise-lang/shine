package apps

import java.io.{BufferedWriter, File, FileOutputStream, FileWriter, PrintWriter}
import mm._
import opencl.executor.Executor
import shine.OpenCL.{GlobalSize, LocalSize}
import util.{gen, writeToPath}

import java.nio.file.{Files, Paths}

object mmTuning {

  // we don't tune input size
//  private val N = 64
//  private val M = 128
//  private val O = 128
//
//  private val N = 512
//  private val M = 1024
//  private val O = 1024
  case class timestamps(init:Long, genExpr: Long, input: Long, genKernel: Long, run: Long, check: Long, cleanUp:Long, overall:Long)

  private def randGold(N:Int, M:Int, O:Int): (Array[Array[Float]], Array[Array[Float]], Array[Float]) = {
    val rand = new scala.util.Random
    val At = Array.fill(O, N)(rand.nextInt(6).toFloat)
    val B = Array.fill(O, M)(rand.nextInt(6).toFloat)
    val gold = computeGold(N, M, O, At, B).flatten
    (At, B, gold)
  }

  def fileToArray(file: String, dim0: Int, dim1:Int):Array[Array[Float]] = {
    val array  = Array.ofDim[Float](dim0, dim1)

    val bufferedSource = io.Source.fromFile(file)

    var i = 0
    for (line <- bufferedSource.getLines()) {
      val cols = line.split(",").map(_.trim.toFloat)
      array(i) = cols
      i +=1
    }
    bufferedSource.close

    array
  }

  def arrayToString(array: Array[Array[Float]]):String ={

    var AtString = ""
    array.foreach(row => {
      var rowString = ""
      row.foreach(elem => {
        rowString += elem.toString + ","
      })
      AtString += rowString.dropRight(1) + "\n"
    })


    AtString
  }

  private def randGold2(N:Int, M:Int, O:Int): (Array[Array[Float]], Array[Array[Float]], Array[Float]) = {
//    private def randGold2(N:Int, M:Int, O:Int) = {

      val files = !Files.exists(Paths.get("At.csv")) || !Files.exists(Paths.get("B.csv")) || !Files.exists(Paths.get("gold.csv"))
      if(files) {

      val rand = new scala.util.Random
        val At = Array.fill(O, N)(rand.nextInt(6).toFloat)
        val B = Array.fill(O, M)(rand.nextInt(6).toFloat)
        println("compute gold")
        val gold = computeGold(N, M, O, At, B)
        println("compute gold finished")

        writeToPath("At.csv", arrayToString(At))
        writeToPath("B.csv", arrayToString(B))
        writeToPath("gold.csv", arrayToString(gold))
      }else{
        println("read from files")
      }

      val At = fileToArray("At.csv", O, N)
      val B = fileToArray("B.csv", O, M)
      val gold = fileToArray("gold.csv", N, M).flatten


      (At, B, gold)
  }

  // main
  def main(args: Array[String]): Unit = {
    val overallStart = System.currentTimeMillis()
    var times = Set.empty[timestamps]

    val initStart = System.currentTimeMillis()
    Executor.loadLibrary()
    Executor.init()
    val init = System.currentTimeMillis() - initStart

    val inputStart = System.currentTimeMillis()
    val N = args(0).toInt
    val M = args(1).toInt
    val O = args(2).toInt

    // read in values from args
    val v3 = args(3).toInt
    val v4 = args(4).toInt
    val v5 = args(5).toInt
    val v6 = args(6).toInt
    val v7 = args(7).toInt
    val v8 = args(8).toInt

    // global local size
    val ls0 = args(9).toInt
    val ls1 = args(10).toInt
    val gs0 = args(11).toInt
    val gs1 = args(12).toInt
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

    // create random values
    println("timestamp")
    val (at, b, gold) = randGold2(N,M,O)
    val input = System.currentTimeMillis() - inputStart

    // generate kernel
    println("timestamp")
    val genExprStart = System.currentTimeMillis()
    // FIXME: delete this file entirely?
    val kernel = ??? // genMMKernel(v3, v4, v5, v6, v7, v8)
    val genExpr = System.currentTimeMillis() - genExprStart

    // run kernel
    try{
//      println("gen kernel")
//      val code = gen.OpenCLKernel(kernel)
//      println("execute kernel")
//      val result = runKernel(code, LocalSize((32, 8)), GlobalSize((M/4, N/8)), at, b)
//      val result = runKernel(gen.OpenCLKernel(kernel), LocalSize((32, 8)), GlobalSize((M/4, N/8)), at, b)
      println("timestamp")
      val genKernelStart = System.currentTimeMillis()
      try{
        val magister = gen.opencl.kernel.fromExpr(kernel)
        val genKernel = System.currentTimeMillis() - genKernelStart

        println("timestamp")
        val runStart = System.currentTimeMillis()
        try{
          val result = runKernel(magister, LocalSize((ls0, ls1)), GlobalSize((gs0, gs1)), at, b)
          val run = System.currentTimeMillis() - runStart

          //      val kernel2 = gen.OpenCLKernel(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(kernel, "")

          //      val runtime = Executor.execute(, ls0, ls1, 1, gs0, gs1, 1, (at, b))

          //      Executor.shutdown()

          // check result
          println("timestamp")
          val checkStart = System.currentTimeMillis()
          try{
            val testSame = util.assertSame(result._1, gold, "result is different from gold")
            val check = System.currentTimeMillis() - checkStart
            //      println("unit: " + result._2.unit)
            println(result._2.value)

            //      val costfile = new PrintWriter(new FileOutputStream(new File("/home/jo/development/lift/atf/atfc/build/costfile.txt"), false))
            //      costfile.println(result._2.value.toString)
            //      costfile.close()
            val cleanUpStart = System.currentTimeMillis()
            Executor.shutdown()
            val cleanUp = System.currentTimeMillis() - cleanUpStart

            val overall = System.currentTimeMillis() - overallStart

            // process times
            val header = Files.exists(Paths.get("times.csv")) match {
              case true => ""
              case false => "N,M,O,v3,v4,v5,v6,v7,v8,runtime,init,genExpr,input,genKernel,run,check,cleanUp,overall\n"
            }
            val content = N.toString + "," + M.toString + "," + O.toString + "," + v3.toString + "," + v4.toString + "," + v5.toString + "," + v6.toString + "," + v7.toString + "," + v8.toString + "," + result._2.value.toString + "," + init.toString + "," + genExpr.toString + "," + input.toString + "," + genKernel.toString + "," + run.toString + "," + check.toString + "," + cleanUp.toString + "," + overall.toString + "\n"
            val writer = new BufferedWriter(new FileWriter("times.csv", true));
            writer.write(header + content);
            writer.close();

            sys.exit(0)
          } catch{
            case e:Throwable => {
              val overall = System.currentTimeMillis() - overallStart

              // process times
              val header = Files.exists(Paths.get("times.csv")) match {
                case true => ""
                case false => "N,M,O,v3,v4,v5,v6,v7,v8,runtime,init,genExpr,input,genKernel,run,check,cleanUp,overall\n"
              }
              val content = N.toString + "," + M.toString + "," + O.toString + "," + v3.toString + "," + v4.toString + "," + v5.toString + "," + v6.toString + "," + v7.toString + "," + v8.toString + "," + result._2.value.toString + "," + init.toString + "," + genExpr.toString + "," + input.toString + "," + genKernel.toString + "," + run.toString + "," + "0" + "," + "0" + "," + overall.toString + "\n"
              val writer = new BufferedWriter(new FileWriter("times.csv", true));
              writer.write(header + content);
              writer.close();

              throw e
            }
          }

        } catch{
          case e:Throwable => {
            val run = System.currentTimeMillis() - runStart
            val overall = System.currentTimeMillis() - overallStart

            // process times
            val header = Files.exists(Paths.get("times.csv")) match {
              case true => ""
              case false => "N,M,O,v3,v4,v5,v6,v7,v8,runtime,init,genExpr,input,genKernel,run,check,cleanUp,overall\n"
            }
            val content = N.toString + "," + M.toString + "," + O.toString + "," + v3.toString + "," + v4.toString + "," + v5.toString + "," + v6.toString + "," + v7.toString + "," + v8.toString + "," + "0" + "," + init.toString + "," + genExpr.toString + "," + input.toString + "," + genKernel.toString + "," + run.toString + "," + "0" + "," + "0" + "," + overall.toString + "\n"
            val writer = new BufferedWriter(new FileWriter("times.csv", true));
            writer.write(header + content);
            writer.close();

            throw e
          }
        }

      } catch{
        case e:Throwable => {
          val overall = System.currentTimeMillis() - overallStart
          // process times
          val header = Files.exists(Paths.get("times.csv")) match {
            case true => ""
            case false => "N,M,O,v3,v4,v5,v6,v7,v8,runtime,init,genExpr,input,genKernel,run,check,cleanUp,overall\n"
          }
          val content = N.toString + "," + M.toString + "," + O.toString + "," + v3.toString + "," + v4.toString + "," + v5.toString + "," + v6.toString + "," + v7.toString + "," + v8.toString + "," + "0" + "," + init.toString + "," + genExpr.toString + "," + input.toString + "," + "0" + "," + "0" + "," + "0" + "," + "0" + "," + overall.toString + "\n"
          val writer = new BufferedWriter(new FileWriter("times.csv", true));
          writer.write(header + content);
          writer.close();

          throw e
        }

      }

    }catch{
      case e:Throwable => {
        Executor.shutdown()
        println("error: " + e)

//        val costfile = new PrintWriter(new FileOutputStream(new File("/home/jo/development/lift/atf/atfc/build/costfile.txt"), false))
//        costfile.println(Double.MaxValue.toString)
//        costfile.close()

        sys.exit(1)
      }
    }
  }
}
