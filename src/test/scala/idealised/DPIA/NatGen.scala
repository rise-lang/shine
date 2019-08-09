package idealised.DPIA

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Primitives.{AsIndex, Idx}
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import opencl.executor.Executor

import scala.util.Random

class NatGen extends idealised.util.Tests {
  test("Negative-exponent power generates valid code") {
    val f =
      nFun(n =>
        fun(ArrayType(n, int))(array => mapSeq(fun(_ => Idx(array, AsIndex(n, n.pow(-1)))), array)))

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(f, Map())))

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)

    Executor.loadAndInit()
    val length = 10
    val random = new Random()
    val array = (0 until length).map(_ => random.nextInt(length)).toArray

    import idealised.OpenCL._
    val runKernel = p.kernel.as[ScalaFunction `(` Int `,` Array[Int] `)=>` Array[Int]](LocalSize(1), GlobalSize(length))
    val (output, _) = runKernel(length `,` array)

    assert(output.forall(_ == array.head))
    Executor.shutdown()
  }
}
