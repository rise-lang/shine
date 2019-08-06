package idealised.apps

import idealised.OpenCL.{GlobalMemory, PrivateMemory, ScalaFunction}
import idealised.OpenCL.SurfaceLanguage.DSL.{depMapGlobal, mapGlobal, oclReduceSeq}
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Primitives.{AsIndex, Fst, Idx, Snd}
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import opencl.executor.Executor

import scala.util.Random

class SparseVector extends idealised.util.Tests {

  test("sparse vector dense vector add") {
    val f = nFun(n => nFun(m =>
      fun(ArrayType(n, TupleType(IndexType(m), float)))(sparse =>
        fun(ArrayType(m, float))(dense =>
          sparse :>> mapSeq(fun(pair => Snd(pair, None) + Idx(dense, Fst(pair, None))))
        )
      )
    ))

    val typed = TypeInference(f, Map())

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(typed))

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)

    def runScala(indices:Array[Int], sparse:Array[Float], dense:Array[Float]):Array[Float] = {
      indices.zip(sparse).map({
        case (index, x) => dense(index) + x
      })
    }

    def runTest():Unit = {
      Executor.loadAndInit()
      val random = new Random()
      val length = 64
      val numEntries = 10 + random.nextInt(20)

      val indices = (0 until numEntries).map(_ => random.nextInt(length)).toArray
      val sparse = (0 until numEntries).map(_ => random.nextFloat()).toArray
      val dense = (0 until length).map(_ => random.nextFloat()).toArray

      import idealised.OpenCL._
      val runKernel = p.kernel.as[ScalaFunction `(` Int `,` Int `,` Array[(Int, Float)] `,` Array[Float] `)=>` Array[Float]](1, length)
      val (output, _) = runKernel(numEntries `,` length `,` indices.zip(sparse) `,` dense)

      Executor.shutdown()

      val scalaOutput = runScala(indices, sparse, dense)
      assert(output.zip(scalaOutput).forall(x => Math.abs(x._1 - x._2) < 0.01))
    }
    runTest()

  }

  test("2 array sparse vector dense vector add") {
    val f = nFun(n => nFun(m =>
      fun(ArrayType(n, IndexType(m)))(indices =>
        fun(ArrayType(n, float))(sparse =>
          fun(ArrayType(m, float))(dense =>
            zip(indices, sparse) :>> mapSeq(fun(pair => Snd(pair, None) + Idx(dense, Fst(pair, None))))
          )
        )
      )))

    val typed = TypeInference(f, Map())

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(typed))

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)

    def runScala(indices:Array[Int], sparse:Array[Float], dense:Array[Float]):Array[Float] = {
      indices.zip(sparse).map({
        case (index, x) => dense(index) + x
      })
    }

    def runTest():Unit = {
      Executor.loadAndInit()
      val random = new Random()
      val length = 64
      val numEntries = 10 + random.nextInt(20)

      val indices = (0 until numEntries).map(_ => random.nextInt(length)).toArray
      val sparse = (0 until numEntries).map(_ => random.nextFloat()).toArray
      val dense = (0 until length).map(_ => random.nextFloat()).toArray

      import idealised.OpenCL._
      val runKernel = p.kernel.as[ScalaFunction `(` Int `,` Int `,` Array[Int] `,` Array[Float] `,` Array[Float] `)=>` Array[Float]](1, length)
      val (output, _) = runKernel(numEntries `,` length `,` indices `,` sparse `,` dense)

      Executor.shutdown()

      val scalaOutput = runScala(indices, sparse, dense)
      assert(output.zip(scalaOutput).forall(x => Math.abs(x._1 - x._2) < 0.01))
    }
    runTest()
  }

  test("sparse vector dense vector dot product") {
    val f = nFun(n => nFun(m =>
      fun(ArrayType(n, TupleType(IndexType(m), float)))(sparse =>
        fun(ArrayType(m, float))(dense =>
          sparse :>> split(n) :>> mapGlobal(
            oclReduceSeq(fun(pair => fun(accum => accum + Snd(pair, None) * Idx(dense, Fst(pair, None)))),0.0f, PrivateMemory))
        )
      )
    )
    )

    val typed = TypeInference(f, Map())

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(typed))

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)

    def runScala(indices:Array[Int], sparse:Array[Float], dense:Array[Float]):Float = {
      indices.zip(sparse).foldLeft(0.0f)({
        case (accum, (index, x)) => accum + dense(index) * x
      })
    }

    def runTest():Unit = {
      Executor.loadAndInit()
      val random = new Random()
      val length = 64
      val numEntries = 10 + random.nextInt(20)

      val indices = (0 until numEntries).map(_ => random.nextInt(length)).toArray
      val sparse = (0 until numEntries).map(_ => random.nextFloat()).toArray
      val dense = (0 until length).map(_ => random.nextFloat()).toArray

      import idealised.OpenCL._
      val runKernel = p.kernel.as[ScalaFunction `(` Int `,` Int `,` Array[(Int, Float)] `,` Array[Float] `)=>` Array[Float]](1, length)
      val (output, _) = runKernel(numEntries `,` length `,` indices.zip(sparse) `,` dense)

      Executor.shutdown()

      val scalaOutput = runScala(indices, sparse, dense)
      assert(Math.abs(scalaOutput - output(0)) < 0.01)
    }
    runTest()

  }

  test("2 array sparse vector dense vector dot product") {
    val f = nFun(n => nFun(m =>
      fun(ArrayType(n, IndexType(m)))(indices =>
        fun(ArrayType(n, float))(sparse =>
          fun(ArrayType(m, float))(dense =>
            zip(indices, sparse) :>> split(n) :>> mapGlobal(
              oclReduceSeq(fun(pair => fun(accum => accum + Snd(pair, None) * Idx(dense, Fst(pair, None)))),0.0f, PrivateMemory))
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

    def runScala(indices:Array[Int], sparse:Array[Float], dense:Array[Float]):Float = {
      indices.zip(sparse).foldLeft(0.0f)({
        case (accum, (index, x)) => accum + dense(index) * x
      })
    }

    def runTest():Unit = {
      Executor.loadAndInit()
      val random = new Random()
      val length = 64
      val numEntries = 10 + random.nextInt(20)

      val indices = (0 until numEntries).map(_ => random.nextInt(length)).toArray
      val sparse = (0 until numEntries).map(_ => random.nextFloat()).toArray
      val dense = (0 until length).map(_ => random.nextFloat()).toArray

      import idealised.OpenCL._
      val runKernel = p.kernel.as[ScalaFunction `(` Int `,` Int `,` Array[Int] `,` Array[Float] `,` Array[Float] `)=>` Array[Float]](1, length)
      val (output, _) = runKernel(numEntries `,` length `,` indices `,` sparse `,` dense)

      Executor.shutdown()

      val scalaOutput = runScala(indices, sparse, dense)
      assert(Math.abs(scalaOutput - output(0)) < 0.01)
    }
    runTest()
  }

  test("dense matrix sparse vector multiplication") {
    val f = nFun(n => nFun(m =>
      fun(ArrayType(n, ArrayType(m, float)))(matrix =>
        nFun(k => fun(ArrayType(k, TupleType(IndexType(m), float)))(vector =>
          matrix :>> mapGlobal(fun(row =>
            vector :>> oclReduceSeq(
              fun(pair => fun(accum => accum + Snd(pair, None) * Idx(row, Fst(pair, None)))), 0.0f, PrivateMemory)
          ))
        ))
      )
    ))

    val typed = TypeInference(f, Map())

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(typed))

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)

    def runScala(matrix:Array[Array[Float]], indices:Array[Int], vector:Array[Float]):Array[Float] = {
      matrix.map(row =>
        indices.zip(vector).map({case (index, x) => row(index) * x }).sum
      )
    }

    def runTest():Unit = {
      Executor.loadAndInit()
      val random = new Random()
      val length = 64
      val numEntries = 10 + random.nextInt(20)

      val matrix = Array.tabulate(length)(_ => Array.tabulate(length)(_ => random.nextFloat()))
      val indices = Array.tabulate(numEntries)(_ => random.nextInt(length))
      val vector = Array.tabulate(numEntries)(_ => random.nextFloat())

      import idealised.OpenCL._
      val runKernel = p.kernel.as[ScalaFunction `(` Int `,` Int `,` Array[Array[Float]] `,` Int `,` Array[(Int, Float)] `)=>` Array[Float]](1, length)
      val (output, _) = runKernel(length `,` length `,` matrix `,` numEntries `,` indices.zip(vector))

      Executor.shutdown()

      val scalaOutput = runScala(matrix, indices, vector)
      assert(output.zip(scalaOutput).forall({case (x,y) => Math.abs(x - y) < 0.01}))
    }
    runTest()
  }

  test("2 array dense matrix sparse vector multiplication") {
    val f = nFun(n => nFun(m =>
      fun(ArrayType(n, ArrayType(m, float)))(matrix =>
        nFun(k =>
          fun(ArrayType(k, IndexType(m)))(indices =>
            fun(ArrayType(k, float))(sparse =>
              matrix :>> mapGlobal(fun(row =>
                zip(indices, sparse) :>> oclReduceSeq(
                  fun(pair => fun(accum => accum + Snd(pair, None) * Idx(row, Fst(pair, None)))), 0.0f, PrivateMemory)
              ))
            ))
        )
      )
    ))

    val typed = TypeInference(f, Map())

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(typed))

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)


    def runScala(matrix:Array[Array[Float]], indices:Array[Int], vector:Array[Float]):Array[Float] = {
      matrix.map(row =>
        indices.zip(vector).map({case (index, x) => row(index) * x }).sum
      )
    }

    def runTest():Unit = {
      Executor.loadAndInit()
      val random = new Random()
      val length = 64
      val numEntries = 10 + random.nextInt(20)

      val matrix = Array.tabulate(length)(_ => Array.tabulate(length)(_ => random.nextFloat()))
      val indices = Array.tabulate(numEntries)(_ => random.nextInt(length))
      val vector = Array.tabulate(numEntries)(_ => random.nextFloat())

      import idealised.OpenCL._
      val runKernel = p.kernel.as[ScalaFunction `(` Int `,` Int `,` Array[Array[Float]] `,` Int `,` Array[Int] `,` Array[Float] `)=>` Array[Float]](1, length)
      val (output, _) = runKernel(length `,` length `,` matrix `,` numEntries `,` indices `,` vector)

      Executor.shutdown()

      val scalaOutput = runScala(matrix, indices, vector)
      assert(output.zip(scalaOutput).forall({case (x,y) => Math.abs(x - y) < 0.01}))
    }
    runTest()
  }

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

  test("Offset based matrix multiply") {
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
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }
}
