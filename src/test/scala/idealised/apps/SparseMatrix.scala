package idealised.apps

import idealised.OpenCL.PrivateMemory
import idealised.OpenCL.SurfaceLanguage.DSL.{depMapGlobal, oclReduceSeq}
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Primitives.{AsIndex, Fst, Idx, Snd}
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import opencl.executor.Executor

import scala.util.Random

class SparseMatrix extends idealised.util.Tests {
  test("Length-based matrix multiply") {
    val f = nFun(n => nFun(m =>
      fun(ArrayType(n, IndexType(m)))(dict =>
        letNat(nFun(i => Idx(dict, AsIndex(n, i))), lenF =>
          fun(DepArrayType(n, i => ArrayType(lenF(i), TupleType(IndexType(m), float))))(matrix =>
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

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)

    Executor.loadAndInit()

    val random = new Random
    def randomValue = random.nextInt(9).toFloat + 1

    // input values
    val n: Int = 1024 // number of rows
    val m: Int = 512  // length of vector (max length of row)
    val dict: Array[Int] = Array.tabulate(n)(_ => random.nextInt(m-1)+1) // length of rows (of max m length)
    val matrix: Array[Array[(Int, Float)]] = Array.tabulate(n)(rowIdx => Array.tabulate(dict(rowIdx))(_ => (random.nextInt(m), randomValue)) ) // matrix values (as pairs of x-coord + value)
    val vector: Array[Float] = Array.tabulate(m)(_ => randomValue) // vector values

    // compute gold output
    val gold = matrix.map( row =>
      row.foldLeft(0.0f) { (accum, pair) =>
        accum + pair._2 * vector(pair._1)
      }
    )

    import idealised.OpenCL._
    val runKernel = p.kernel.as[ScalaFunction `(` Int `,` Int `,` Array[Int] `,` Array[Array[(Int, Float)]] `,` Array[Float] `)=>` Array[Float]](1, n)
    val (output, _) = runKernel(n `,` m `,` dict `,` matrix `,` vector)

    assert(gold sameElements output)

    //    def print1D[T]: Array[T] => String = x => x.mkString("[", ", ", "]")
    //    def print2D[T]: Array[Array[T]] => String = x => x.map(print1D).mkString("[\n  ", ",\n  ", "\n]")
    //    println(s"Vector\n: ${print1D(vector)}")
    //    println(s"Matrix\n: ${print2D(matrix)}")
    //    println(s"Dict\n: ${print1D(dict)}")
    //    println(s"\nGold\n: ${print1D(gold)}")
    //    println(s"\nOutput\n: ${print1D(output)}")

    Executor.shutdown()
  }

  test("Length-based matrix multiply using zip") {
    val f = nFun(n => nFun(m =>
      fun(ArrayType(n, IndexType(m)))(dict =>
        letNat(nFun(i => Idx(dict, AsIndex(n, i))), lenF =>
          fun(DepArrayType(n, i => ArrayType(lenF(i), IndexType(m))))(xCoords =>
            fun(DepArrayType(n, i => ArrayType(lenF(i), float)))(values =>
              fun(ArrayType(m, float))(vector =>
                depZip(xCoords, values)
                  // DepArrayType(n, i => ArrayType(lenF(i), TupleType(IndexType(m), float)))
                  :>> depMapGlobal(fun(pair => zip(Fst(pair, None), Snd(pair, None)) :>>
                  oclReduceSeq(fun(pair => fun(accum => accum + Snd(pair, None) * Idx(vector, Fst(pair, None)))),0.0f, PrivateMemory)
                ))
              )
            )
          )
        )
      )
    ))

    val typed = TypeInference(f, Map())

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(typed))

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)

    Executor.loadAndInit()

    val random = new Random
    def randomValue = random.nextInt(9).toFloat + 1

    // input values
    val n: Int = 1024 // number of rows
    val m: Int = 512  // length of vector (max length of row)
    val dict: Array[Int] = Array.tabulate(n)(_ => random.nextInt(m-1)+1) // length of rows (of max m length)
    val xCoords: Array[Array[Int]] = Array.tabulate(n)(rowIdx => Array.tabulate(dict(rowIdx))(_ => random.nextInt(m)))
    val values: Array[Array[Float]] = Array.tabulate(n)(rowIdx => Array.tabulate(dict(rowIdx))(_ => randomValue)) // matrix values (as pairs of x-coord + value)
    val vector: Array[Float] = Array.tabulate(m)(_ => randomValue) // vector values

    // compute gold output
    val gold = xCoords.zip(values).map( row =>
      row._1.zip(row._2).foldLeft(0.0f) { (accum, pair) =>
        accum + pair._2 * vector(pair._1)
      }
    )

    import idealised.OpenCL._
    val runKernel = p.kernel.as[ScalaFunction `(` Int `,` Int `,` Array[Int] `,` Array[Array[Int]] `,` Array[Array[Float]] `,` Array[Float] `)=>` Array[Float]](1, n)
    val (output, _) = runKernel(n `,` m `,` dict `,` xCoords `,` values `,` vector)

    assert(gold sameElements output)

    //    def print1D[T]: Array[T] => String = x => x.mkString("[", ", ", "]")
    //    def print2D[T]: Array[Array[T]] => String = x => x.map(print1D).mkString("[\n  ", ",\n  ", "\n]")
    //    println(s"Vector\n: ${print1D(vector)}")
    //    println(s"Matrix\n: ${print2D(matrix)}")
    //    println(s"Dict\n: ${print1D(dict)}")
    //    println(s"\nGold\n: ${print1D(gold)}")
    //    println(s"\nOutput\n: ${print1D(output)}")

    Executor.shutdown()
  }

  /**
    * @param n number of rows
    * @param m length of vector (max length of row)
    * @param localSize local sizes
    */
  private def offsetBasedMMNoZip(n:Int, m:Int, localSize:Int):Double = {
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

    val code = p.code
    //SyntaxChecker.checkOpenCL(code)
    println(code)

    Executor.loadAndInit()

    val random = new Random
    def randomValue = random.nextInt(9).toFloat + 1

    // input values
    val rowLengths: Array[Int] = Array.tabulate(n)(_ => random.nextInt(m-1)+1)
    val dict: Array[Int] = rowLengths.scan(0)(_+_) // offset into rows (of max m length)
    val matrix: Array[Array[(Int, Float)]] = Array.tabulate(n)(rowIdx => Array.tabulate(rowLengths(rowIdx))(_ => (random.nextInt(m), randomValue)) ) // matrix values (as pairs of x-coord + value)
    val vector: Array[Float] = Array.tabulate(m)(_ => randomValue) // vector values

    // compute gold output
    val gold = matrix.map( row =>
      row.foldLeft(0.0f) { (accum, pair) =>
        accum + pair._2 * vector(pair._1)
      }
    )

    import idealised.OpenCL._
    val runKernel = p.kernel.as[ScalaFunction `(` Int `,` Int `,` Array[Int] `,` Array[Array[(Int, Float)]] `,` Array[Float] `)=>` Array[Float]](localSize, n)
    val (output, time) = runKernel(n `,` m `,` dict `,` matrix `,` vector)

    assert(gold sameElements output)

    //    def print1D[T]: Array[T] => String = x => x.mkString("[", ", ", "]")
    //    def print2D[T]: Array[Array[T]] => String = x => x.map(print1D).mkString("[\n  ", ",\n  ", "\n]")
    //    println(s"Vector\n: ${print1D(vector)}")
    //    println(s"Matrix\n: ${print2D(matrix)}")
    //    println(s"Row lengths\n: ${print1D(rowLengths)}")
    //    println(s"Dict\n: ${print1D(dict)}")
    //    println(s"\nGold\n: ${print1D(gold)}")
    //    println(s"\nOutput\n: ${print1D(output)}")

    Executor.shutdown()

    time.value
  }

  test("Offset based matrix multiply") {
   offsetBasedMMNoZip(1024, 512, 1)
  }

  /**
    * @param n number of rows
    * @param m length of vector (max length of row)
    * @param localSize local sizes
    */
  private def offsetBasedMMWithZip(n:Int, m:Int, localSize:Int):Double = {
    val f = nFun(n => nFun(m =>
      fun(ArrayType(n + 1, int))(dict =>
        letNat(nFun(i => Idx(dict, AsIndex(n + 1, i))), lookup =>
          fun(DepArrayType(n, i => ArrayType(lookup(i + 1) - lookup(i), IndexType(m))))( xCoords =>
            fun(DepArrayType(n, i => ArrayType(lookup(i + 1) - lookup(i), float)))(values =>
              fun(ArrayType(m, float))(vector =>
                depZip(xCoords, values) :>> depMapGlobal(fun(pair => zip(Fst(pair, None), Snd(pair, None)) :>>
                  oclReduceSeq(fun(pair => fun(accum => accum + Snd(pair, None) * Idx(vector, Fst(pair, None)))),0.0f, PrivateMemory)
                )
                )
              )
            )
          )
        )
      )
    )
    )

    val typed = TypeInference(f, Map())

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(typed))

    val code = p.code
    //SyntaxChecker.checkOpenCL(code)
    println(code)

    Executor.loadAndInit()

    val random = new Random
    def randomValue = random.nextInt(9).toFloat + 1

    // input values
    val rowLengths: Array[Int] = Array.tabulate(n)(_ => random.nextInt(m-1)+1)
    val dict: Array[Int] = rowLengths.scan(0)(_+_) // offset into rows (of max m length)
    val xCoords: Array[Array[Int]] = Array.tabulate(n)(rowIdx => Array.tabulate(rowLengths(rowIdx))(_ => random.nextInt(m)))
    val values: Array[Array[Float]] = Array.tabulate(n)(rowIdx => Array.tabulate(rowLengths(rowIdx))(_ => randomValue)) // matrix values (as pairs of x-coord + value)
    val vector: Array[Float] = Array.tabulate(m)(_ => randomValue) // vector values

    // compute gold outputt
    val gold = xCoords.zip(values).map( row =>
      row._1.zip(row._2).foldLeft(0.0f) { (accum, pair) =>
        accum + pair._2 * vector(pair._1)
      }
    )

    import idealised.OpenCL._
    val runKernel = p.kernel.as[ScalaFunction `(` Int `,` Int `,` Array[Int] `,` Array[Array[Int]] `,` Array[Array[Float]] `,` Array[Float] `)=>` Array[Float]](localSize, n)
    val (output, time) = runKernel(n `,` m `,` dict `,` xCoords `,` values `,` vector)

    assert(gold sameElements output)
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

    Executor.shutdown()

    time.value
  }

  test("Offset based matrix multiply using zip") {
    offsetBasedMMWithZip(1024, 1024, 1)
  }
}
