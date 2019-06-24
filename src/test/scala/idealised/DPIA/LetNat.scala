package idealised.DPIA
import idealised.OpenCL.{GlobalMemory, ScalaFunction}
import idealised.OpenCL.SurfaceLanguage.DSL.oclReduceSeq
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Primitives.{Fst, Idx, Snd}
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

import scala.util.Random

class LetNat extends idealised.util.Tests{

  test("Simple function") {
    val program = nFun(n =>
      fun(ArrayType(n, float))(xs =>
            letNat(5, five =>
              mapSeq(fun(x => x))(drop(five(), xs)
      ))))

    val typed = TypeInference(program, Map())
    println(typed.t)
    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(typed))

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }

  test("Simple functions with no capture") {
    val program = nFun(n =>
      fun(ArrayType(n, float))(xs =>
        letNat(fun(int)(x => x + 2), f =>
          letNat(fun(int)(x => x + 1), g =>
            letNat(5, five =>
              mapSeq(fun(x => x))(take(g(f(five())), xs))
            )
          )
        )
    ))

    val typed = TypeInference(program, Map())

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(typed))

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }

  test("Inlined single value") {
    val f = nFun(n => fun(IndexType(10))(idx =>
        fun(ArrayType(n, float))(xs =>
            letNat(idx,
              f => xs :>> take(f()) :>> mapSeq(fun(x => x + 1.0f)))
        )
      ))

    val typed = TypeInference(f, Map())

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(typed))

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }


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

    val random = new Random()
    val length = 64
    val numEntries = 10 + random.nextInt(20)

    val indices = (0 until numEntries).map(_ => random.nextInt(length)).toArray
    val sparse = (0 until numEntries).map(_ => random.nextFloat()).toArray
    val dense = (0 until length).map(_ => random.nextFloat()).toArray

    import idealised.OpenCL._
    val runKernel = p.kernel.as[ScalaFunction `(` Int `,` Int `,` Array[Int] `,`Array[Float] `,` Array[Float] `)=>` Array[Float]](1, 1)
    val (output, _) = runKernel(numEntries `,` length `,` indices `,` sparse `,` dense)

    val scalaOutput = runScala(indices, sparse, dense)
    assert(output.zip(scalaOutput).forall(x => x._1 - x._2 < 0.01))
  }

  test("sparse vector dense vector multiplication") {
    val f = nFun(n => nFun(m =>
      fun(ArrayType(n, TupleType(IndexType(m), float)))(sparse =>
        fun(ArrayType(m, float))(dense =>
          sparse :>> split(n) :>> mapSeq(
            oclReduceSeq(fun(pair => fun(accum => accum + Snd(pair, None) + Idx(dense, Fst(pair, None)))),0.0f, GlobalMemory))
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

  test("2 array sparse vector dense vector multiplication") {
    val f = nFun(n => nFun(m =>
      fun(ArrayType(n, IndexType(m)))(indices =>
        fun(ArrayType(n, float))(sparse =>
        fun(ArrayType(m, float))(dense =>
          zip(indices, sparse) :>> split(n) :>> mapSeq(
            oclReduceSeq(fun(pair => fun(accum => accum + Snd(pair, None) + Idx(dense, Fst(pair, None)))),0.0f, GlobalMemory))
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

  test("dense matrix sparse vector multiplication") {
    val f = nFun(n => nFun(m =>
      fun(ArrayType(n, ArrayType(m, float)))(matrix =>
          nFun(k => fun(ArrayType(k, TupleType(IndexType(m), float)))(vector =>
            matrix :>> mapSeq(fun(row =>
              vector :>> oclReduceSeq(
                fun(pair => fun(accum => accum + Snd(pair, None) + Idx(row, Fst(pair, None)))), 0.0f, GlobalMemory)
            ))
          ))
      )
    ))

    val typed = TypeInference(f, Map())

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(typed))

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)  }

  test("2 array dense matrix sparse vector multiplication") {
    val f = nFun(n => nFun(m =>
      fun(ArrayType(n, ArrayType(m, float)))(matrix =>
        nFun(k =>
          fun(ArrayType(k, IndexType(m)))(indices =>
          fun(ArrayType(k, float))(sparse =>
            matrix :>> mapSeq(fun(row =>
              zip(indices, sparse) :>> oclReduceSeq(
                fun(pair => fun(accum => accum + Snd(pair, None) + Idx(row, Fst(pair, None)))), 0.0f, GlobalMemory)
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
  }
}
