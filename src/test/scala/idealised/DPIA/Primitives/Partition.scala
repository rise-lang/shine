package idealised.DPIA.Primitives

import idealised.DPIA.{Nat, NatIdentifier}
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.{->, Expr}
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import lift.arithmetic._

import scala.util.Random


class Partition extends idealised.util.Tests {
  test("Simple partition into a triangle C") {
    val N = Cst(6)

    val lenF = (i: NatIdentifier) => i + 1

    val slideExample =
      nFun(n =>
        fun(ArrayType(n, float))(xs => xs :>> partition(3, lenF) :>> depMapSeq(mapSeq(fun(x => x)))))

    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(slideExample, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
  }

  test("Partition threeway with pad and unrolling") {
    opencl.executor.Executor.loadAndInit()
    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}

    val padAmount = 3

    def lenF(n: Nat) = SteppedCase(3, n, 3) _

    val padAndPartition = nFun(n =>
      fun(ArrayType(n, float))(xs => xs :>>
        pad(padAmount, padAmount, l(0.0f)) :>>
        partition(3, lenF(n)) :>>
        depMapSeqUnroll(mapSeq(fun(x => x + l(1.0f))))))

    val p = idealised.OpenCL.KernelGenerator.makeCode(1, 1)(TypeInference(padAndPartition, Map()).toPhrase)
    val kernelF = p.as[ScalaFunction `(` Int `,` Array[Float] `)=>` Array[Float]]
    val input = Array.fill(128)(5.0f)
    val (output, time) = kernelF(128 `,` input)

    val scalaOutput = (Array.fill(padAmount)(0.0f) ++ input ++ Array.fill(padAmount)(0.0f)).map(x => x + 1.0f)

    assert(output.zip(scalaOutput).forall { case (x, y) => x - y < 0.01 })

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    opencl.executor.Executor.shutdown()
  }
}
