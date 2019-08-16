package benchmarks.sparse

import java.io.File

import idealised.OpenCL.PrivateMemory
import idealised.OpenCL.SurfaceLanguage.DSL.{depMapGlobal, oclReduceSeq}
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Primitives.{AsIndex, Fst, Idx, Snd}
import idealised.SurfaceLanguage.Types._
import opencl.executor.Executor

import scala.util.Random

object Benchmark {

  def main(args:Array[String]):Unit = {
    args.headOption match {
      case None => println("Provide path to matrix filename")
      case Some(path) =>
        val file = new File(path)
        if(!file.exists()) {
          println(s"Matrix file ${file.getAbsolutePath} does not exist")
        } else {
          println(s"Loading matrix file ${file.getAbsolutePath}")
          val matrix = TwoArrayCSR.loadMatrixMarketFormat(file)
          println(s"Loaded matrix of size ${(matrix.numRows, matrix.numCols, matrix.entries.length)}")


          val localSizes = Seq(4, 8, 16, 32, 64, 128)
          val globalSizes = Seq(8192, 16384, 32768, 65536)

          for {
            localSize <- localSizes
            globalSize <- globalSizes
          } {
            csrTwoArrays(matrix)(localSize, globalSize, 10.0)
          }
        }
    }
  }


  private def csrTwoArrays(spm:TwoArrayCSR)(localSize:Int, globalSize:Int, allowedEps:Double) = {
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

    Executor.loadAndInit()

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
    val (output, time) = runKernel(n `,` m `,` dict `,` matrix `,` vector)

    val wrongCount = gold.zip(output).map(x => Math.abs(x._1 - x._2)).count(_ > allowedEps)
    val correct = wrongCount
    //assert(wrongCount == 0)

    Executor.shutdown()

    println(s"n=$n, m=$m, localSize=$localSize, globalSize=$globalSize, runtime=$time, correct=$correct")
    time
  }
  private def offsetBasedMMWithZip = {
    nFun(n => nFun(m =>
      fun(ArrayType(n + 1, int))(dict =>
        letNat(nFun(i => Idx(dict, AsIndex(n + 1, i))), lookup =>
          fun(DepArrayType(n, i => ArrayType(lookup(i + 1) - lookup(i), IndexType(m))))(xCoords =>
            fun(DepArrayType(n, i => ArrayType(lookup(i + 1) - lookup(i), float)))(values =>
              fun(ArrayType(m, float))(vector =>
                depZip(xCoords, values) :>> depMapGlobal(fun(pair => zip(Fst(pair, None), Snd(pair, None)) :>>
                  oclReduceSeq(fun(pair => fun(accum => accum + Snd(pair, None) * Idx(vector, Fst(pair, None)))), 0.0f, PrivateMemory)
                )
                )
              )
            )
          )
        )
      )
    ))
  }
}
