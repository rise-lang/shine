package benchmarks.sparse

import java.io.{File, FileWriter}

import idealised.OpenCL.PrivateMemory
import idealised.OpenCL.SurfaceLanguage.DSL.{depMapGlobal, depMapWorkgroup, mapLocal, oclReduceSeq, toLocal}
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Primitives.{AsIndex, Fst, Idx, Snd}
import idealised.SurfaceLanguage.Types._
import lift.arithmetic.Cst
import opencl.executor.Executor

import scala.util.Random

object Benchmark {

  val numReps = 1

  def main(args:Array[String]):Unit = {

    val inputFilePath = args.headOption
    val outputFilePath = if(args.length >= 2) Some(args(1)) else None
    outputFilePath.foreach(path => println(s"Results will be appended to output file $path"))

    def printout(s:String):Unit = {
      println(s)
      outputFilePath
        .map(new FileWriter(_, true))
        .foreach(fw => {
          fw.write(s + "\n")
          fw.close()
        })
    }

    inputFilePath match {
      case None => println("Provide path to matrix file or directory")
      case Some(path) =>
        val file = new File(path)
        if (file.isDirectory) {
          file.listFiles().foreach(testFile(_, printout))
        } else {
          testFile(file, printout)
        }
    }
  }

  private def testFile(file: File, printout:String => Unit):Unit = {
    if(!file.exists()) {
      println(s"Matrix file ${file.getAbsolutePath} does not exist")
    } else {
      println(s"Loading matrix file ${file.getAbsolutePath}")
      try {
        val cooMatrix = COOMatrix.loadMatrixMarketFormat(file)
        println(s"Loaded matrix of size ${(cooMatrix.numRows, cooMatrix.numCols, cooMatrix.entries.length)}")

        val localSizes = Seq(16, 32, 64, 128, 256)
        val globalSizes = Seq(6400, 8192, 10240)

        Executor.loadAndInit()

        /*{
          println("Two arrays...")
          val matrix = TwoArrayCSR(cooMatrix)
          for {
            localSize <- localSizes
            globalSize <- globalSizes
          } {
            csrTwoArrays(file.getName, matrix)(localSize, globalSize, 10.0)(printout)
          }
        }*/

        {
          println("Three arrays...")
          val matrix = ThreeArrayCSR(cooMatrix)
          for {
            localSize <- localSizes
            globalSize <- globalSizes
          } {
            csrThreeArraysCoalesced(file.getName, matrix)(localSize, globalSize, 10.0)(printout)
          }
        }

        Executor.shutdown()
      } catch {
          case _:ArrayIndexOutOfBoundsException => println("Something went wrong in the loading of the matrix")
      }
    }
  }

  private def repeatNTimes[A](n:Int, f:() => A)(maxBy:A => Double):A = {
    val xs = for(_ <- 0 until n) yield f()
    xs.maxBy(maxBy)
  }


  private def csrTwoArrays(name:String, spm:TwoArrayCSR)(localSize:Int, globalSize:Int, allowedEps:Double)(printout:String => Unit) = {
    val f = nFun(n => nFun(m =>
      fun(ArrayType(n + 1, int))(dict =>
        letNat(nFun(i => Idx(dict, AsIndex(n + 1, i))), lookup =>
          fun(DepArrayType(n, i => ArrayType(lookup(i + 1) - lookup(i), TupleType(IndexType(m), float))))(matrix =>
            fun(ArrayType(m, float))(vector =>
              matrix :>> depMapGlobal(
                oclReduceSeq(fun(pair => fun(accum => accum + Snd(pair, None) * Idx(vector, Fst(pair, None)))),0.0f, PrivateMemory)
              )
            )
          )
        )
      )
    )
    )
    val typed = TypeInference(f, Map())

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(typed))

    //SyntaxChecker.checkOpenCL(code)
    //println(code)

    val random = new Random
    def randomValue = random.nextInt(9).toFloat + 1

    // input values
    val n = spm.numRows
    val m = spm.numCols
    val dict: Array[Int] = spm.offsets
    val matrix: Array[Array[(Int, Float)]] = spm.entries
    val vector: Array[Float] = Array.tabulate(m)(_ => randomValue) // vector values

    // compute gold output
    val gold = matrix.map( row =>
      row.foldLeft(0.0f) { (accum, pair) =>
        accum + pair._2 * vector(pair._1)
      }
    )

    import idealised.OpenCL._
    val runKernel = p.kernel.as[ScalaFunction `(` Int `,` Int `,` Array[Int] `,` Array[Array[(Int, Float)]] `,` Array[Float] `)=>` Array[Float]](localSize, globalSize)
    val runF = () => runKernel(n `,` m `,` dict `,` matrix `,` vector)
    val (output, time) = repeatNTimes(numReps, runF)(_._2.value)

    val wrongCount = gold.zip(output).map(x => Math.abs(x._1 - x._2)).count(_ > allowedEps)
    //assert(wrongCount == 0)

    printout(s"name=$name, n=$n, m=$m, localSize=$localSize, globalSize=$globalSize, version=csrThreeArr, runtime=$time, incorrect=$wrongCount")
    time
  }


  private def csrThreeArraysCoalesced(name:String, spm:ThreeArrayCSR)(localSize:Int, globalSize:Int, allowedEps:Double)(printout:String => Unit) = {
    val f = nFun(n => nFun(m =>
      fun(ArrayType(n + 1, int))(dict =>
        letNat(nFun(i => Idx(dict, AsIndex(n + 1, i))), lookup =>
          fun(DepArrayType(n, i => ArrayType(lookup(i + 1) - lookup(i), IndexType(m))))(xCoords =>
            fun(DepArrayType(n, i => ArrayType(lookup(i + 1) - lookup(i), float)))(values =>
              fun(ArrayType(m, float))(vector =>
                depZip(xCoords, values) :>> depMapWorkgroup.withIndex(nFun(rowID =>
                  fun(twoRows => {
                    def rowLength = lookup(rowID+1) - lookup(rowID)
                    def chunkedRowXs = Fst(twoRows) :>> pad(0, rowLength % 32, AsIndex(m, Cst(0))) :>> split(32)
                    def chunkedRowVals = Snd(twoRows) :>> pad(0, rowLength % 32, 0.0f) :>> split(32)
                    zip(chunkedRowXs, chunkedRowVals) :>>
                      mapLocal(fun(rowPair => zip(Fst(rowPair), Snd(rowPair)) :>>
                        toLocal(oclReduceSeq(fun(pair => fun(accum => accum + Snd(pair) * Idx(vector, Fst(pair)))), 0.0f, PrivateMemory))
                      )) :>> oclReduceSeq(fun(x => fun(y => x + y)), 1.0f, PrivateMemory)
                }))
              )
            )
          )
        )
      )
    )))

    val typed = TypeInference(f, Map())

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(typed))

    //SyntaxChecker.checkOpenCL(code)
    println(p.code)

    val random = new Random
    def randomValue = random.nextInt(9).toFloat + 1

    // input values
    val n = spm.numRows
    val m = spm.numCols
    // input values
    val dict: Array[Int] = spm.offsets
    val xCoords: Array[Array[Int]] = spm.colIdx
    val values: Array[Array[Float]] = spm.entries
    val vector: Array[Float] = Array.tabulate(m)(_ => randomValue) // vector values


    // compute gold output
    val gold = xCoords.zip(values).map( row =>
      row._1.zip(row._2).foldLeft(0.0f) { (accum, pair) =>
        accum + pair._2 * vector(pair._1)
      }
    )
    import idealised.OpenCL._
    val runKernel = p.kernel.as[ScalaFunction `(` Int `,` Int `,` Array[Int] `,` Array[Array[Int]] `,` Array[Array[Float]] `,` Array[Float] `)=>` Array[Float]](localSize, globalSize)
    val runF = () => runKernel(n `,` m `,` dict `,` xCoords `,` values `,` vector)

    val (output, time) = repeatNTimes(numReps, runF)(_._2.value)
    /*
       def print1D[T]: Array[T] => String = x => x.mkString("[", ", ", "]")
        def print2D[T]: Array[Array[T]] => String = x => x.map(print1D).mkString("[\n  ", ",\n  ", "\n]")
        println(s"Vector\n: ${print1D(vector)}")
        println(s"Matrix\n: ${print2D(xCoords.zip(values).map(x => x._1.zip(x._2)))}")
        println(s"Row lengths\n: ${print1D(rowLengths)}")
        println(s"Dict\n: ${print1D(dict)}")
        println(s"\nGold\n: ${print1D(gold)}")
        println(s"\nOutput\n: ${print1D(output)}")

     */

    val wrongCount = gold.zip(output).map(x => Math.abs(x._1 - x._2)).count(_ > allowedEps)
    //assert(wrongCount == 0)

    printout(s"name=$name, n=$n, m=$m, localSize=$localSize, globalSize=$globalSize, version=csrThreeArr, runtime=$time, incorrect=$wrongCount")
    time
  }


  private def csrThreeArrays(name:String, spm:ThreeArrayCSR)(localSize:Int, globalSize:Int, allowedEps:Double)(printout:String => Unit) = {
    val f = nFun(n => nFun(m =>
      fun(ArrayType(n + 1, int))(dict =>
        letNat(nFun(i => Idx(dict, AsIndex(n + 1, i))), lookup =>
          fun(DepArrayType(n, i => ArrayType(lookup(i + 1) - lookup(i), IndexType(m))))(xCoords =>
            fun(DepArrayType(n, i => ArrayType(lookup(i + 1) - lookup(i), float)))(values =>
              fun(ArrayType(m, float))(vector =>
                depZip(xCoords, values) :>> depMapGlobal(fun(pair => zip(Fst(pair, None), Snd(pair, None)) :>>
                  oclReduceSeq(fun(pair => fun(accum => accum + Snd(pair, None) * Idx(vector, Fst(pair, None)))), 0.0f, PrivateMemory)
                )
                )  :>> printType("Final result")
              )
            )
          )
        )
      )
    ))

    val typed = TypeInference(f, Map())

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(typed))

    //SyntaxChecker.checkOpenCL(code)
    //println(code)

    val random = new Random
    def randomValue = random.nextInt(9).toFloat + 1

    // input values
    val n = spm.numRows
    val m = spm.numCols
    // input values
    val dict: Array[Int] = spm.offsets
    val xCoords: Array[Array[Int]] = spm.colIdx
    val values: Array[Array[Float]] = spm.entries
    val vector: Array[Float] = Array.tabulate(m)(_ => randomValue) // vector values


    // compute gold output
    val gold = xCoords.zip(values).map( row =>
      row._1.zip(row._2).foldLeft(0.0f) { (accum, pair) =>
        accum + pair._2 * vector(pair._1)
      }
    )
    import idealised.OpenCL._
    val runKernel = p.kernel.as[ScalaFunction `(` Int `,` Int `,` Array[Int] `,` Array[Array[Int]] `,` Array[Array[Float]] `,` Array[Float] `)=>` Array[Float]](localSize, globalSize)
    val runF = () => runKernel(n `,` m `,` dict `,` xCoords `,` values `,` vector)

    val (output, time) = repeatNTimes(numReps, runF)(_._2.value)
    /*
       def print1D[T]: Array[T] => String = x => x.mkString("[", ", ", "]")
        def print2D[T]: Array[Array[T]] => String = x => x.map(print1D).mkString("[\n  ", ",\n  ", "\n]")
        println(s"Vector\n: ${print1D(vector)}")
        println(s"Matrix\n: ${print2D(xCoords.zip(values).map(x => x._1.zip(x._2)))}")
        println(s"Row lengths\n: ${print1D(rowLengths)}")
        println(s"Dict\n: ${print1D(dict)}")
        println(s"\nGold\n: ${print1D(gold)}")
        println(s"\nOutput\n: ${print1D(output)}")

     */

    val wrongCount = gold.zip(output).map(x => Math.abs(x._1 - x._2)).count(_ > allowedEps)
    //assert(wrongCount == 0)

    printout(s"name=$name, n=$n, m=$m, localSize=$localSize, globalSize=$globalSize, version=csrThreeArr, runtime=$time, incorrect=$wrongCount")
    time
  }
}
