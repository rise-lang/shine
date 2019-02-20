package idealised.DPIA.Primitives

import idealised.DPIA.NatIdentifier
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import lift.arithmetic._


class Partition extends idealised.util.Tests {
  test("Simple partition into a triangle C") {
    val N = SizeVar("N")
    val lenF = (i:NatIdentifier) => i + 1

    val slideExample = fun(ArrayType(N, float))(xs => xs :>> partition(3, lenF) :>> depMapSeq(mapSeq(fun(x => x + 1.0f))))

    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(slideExample, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)
  }

  test("Partition threeway with pad") {
    opencl.executor.Executor.loadAndInit()
    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}

    val N = SizeVar("N")

    val lenF =  SteppedCase(3, N, 3) _

    val padAndPartition = fun(ArrayType(N, float))(xs => xs :>>
      pad(3, 3,1.0f) :>>
      partition(3, lenF) :>> depMapSeq(mapSeq(fun(x => x + 1.0f))))

    val p = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(padAndPartition, Map()).toPhrase, localSize = 1, globalSize = 1)
    val kernelF = p.as[ScalaFunction`(`Array[Float]`)=>`Array[Float]]
    val input = Array.fill(128)(5.0f)
    val (output, time) = kernelF(input `;`)

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
    opencl.executor.Executor.shutdown()
  }
}
