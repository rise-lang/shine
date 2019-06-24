package idealised.DPIA
import idealised.OpenCL.{GlobalMemory, ScalaFunction}
import idealised.OpenCL.SurfaceLanguage.DSL.{mapGlobal, oclReduceSeq}
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Primitives.{AsIndex, Fst, Idx, Snd}
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import opencl.executor.Executor

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

  test("dense matrix sparse vector multiplication") {
    val f = nFun(n => nFun(m =>
      fun(ArrayType(n, ArrayType(m, float)))(matrix =>
          nFun(k => fun(ArrayType(n, TupleType(IndexType(m), float)))(vector =>
            matrix :>> mapSeq(fun(row =>
              vector :>> oclReduceSeq(
                fun(pair => fun(accum => accum + Snd(pair, None) + Idx(row, Fst(pair, None)))), 0.0f, GlobalMemory)
            ))
          ))
      )
    )
    )

    val typed = TypeInference(f, Map())

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(typed))

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)  }
}
