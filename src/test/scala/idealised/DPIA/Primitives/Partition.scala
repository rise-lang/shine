package idealised.DPIA.Primitives

import benchmarks.core.SimpleRunOpenCLProgram
import idealised.DPIA.Nat
import idealised.OpenCL.KernelWithSizes
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.{Expr, NatIdentifier}
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import idealised.utils.Time.ms
import idealised.utils.TimeSpan
import lift.arithmetic._

import scala.util.Random


class Partition extends idealised.util.Tests {
  test("Simple partition into a triangle C") {
    val N = Cst(6)

    val lenF = (i: NatIdentifier) => i + 1

    val slideExample =
      nFun(n =>
        fun(ArrayType(n, float))(xs => xs :>> partition(3, lenF) :>> depMapSeq(mapSeq(fun(x => x)))))

    val p = idealised.C.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(slideExample, Map())))
    val code = p.code
    SyntaxChecker(code)
  }

  test("Partition threeway with pad and unrolling") {
    val padAmount = 3

    def lenF(n: Nat) = SteppedCase(3, n, 3) _

    val padAndPartition = nFun(n =>
      fun(ArrayType(n, float))(xs => xs :>>
        pad(padAmount, padAmount, 0.0f) :>>
        partition(3, lenF(n)) :>>
        depMapSeqUnroll(mapSeq(fun(x => x + 1.0f)))))

    case class Run() extends SimpleRunOpenCLProgram(false) {
      val inputSize: Int = 128

      override type Input = Array[Float]

      override def dpiaProgram: Expr = padAndPartition

      override protected def makeInput(random: Random): Input = {
        Array.fill(inputSize)(5.0f)
      }

      override protected def runScalaProgram(input: Array[Float]): Array[Float] = {
        (Array.fill(padAmount)(0.0f) ++ input ++ Array.fill(padAmount)(0.0f)).map(x => x + 1.0f)
      }

      override protected def runKernel(k: KernelWithSizes, input: Array[Float]): (Array[Float], TimeSpan[ms]) = {
        import idealised.OpenCL._

        val kernelFun = k.as[ScalaFunction `(` Int `,` Input `)=>` Array[Float]]
        kernelFun(inputSize `,` input)
      }
    }

    Run().run(1, 1).correctness.check()
  }
}
