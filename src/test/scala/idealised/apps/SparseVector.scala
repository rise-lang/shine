package idealised.apps

import idealised.OpenCL.{GlobalMemory, PrivateMemory, ScalaFunction}
import idealised.OpenCL.SurfaceLanguage.DSL.{mapGlobal, oclReduceSeq}
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

  test("Sparse matrix dense vector") {
    val f = nFun(n =>
      fun(ArrayType(n, int))(dict =>
        letNat(dict, dictN =>
          letNat(fun(ArrayType(n, int))(dict => nFun(x => Idx(dict, AsIndex(n, x)))), lenF =>
            fun(DepArrayType(n, i => ArrayType(lenF(dictN, i), float)))(arr => depMapSeq(mapSeq(fun(x => x + 1.0f)), arr))
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
