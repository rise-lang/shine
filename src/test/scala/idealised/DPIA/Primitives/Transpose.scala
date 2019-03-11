package idealised.DPIA.Primitives

import idealised.OpenCL.{ScalaFunction, `(`, `)=>`}
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types.{ArrayType, DepArrayType, TypeInference, float}
import lift.arithmetic.{NamedVar, StartFromRange}

import scala.util.Random

class Transpose extends idealised.util.Tests {
  test("'Type level transposition' with join->split (OpenCL 2D)") {
    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}
    opencl.executor.Executor.loadAndInit()

    val N = NamedVar("N", StartFromRange(1))
    val M = NamedVar("M", StartFromRange(1))
    val f = fun(ArrayType(N, ArrayType(M, float)))(xs => xs :>> join :>> split(M))

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(1,1)(TypeInference(f, Map()).toPhrase)
    val kernelF = kernel.as[ScalaFunction`(`Array[Array[Float]]`)=>`Array[Float]]

    val random = new Random()
    val actualN = 9
    val actualM = 6
    val input = Array.fill(actualN)(Array.fill(actualM)(random.nextFloat()))
    val scalaOutput = input.flatten

    val (kernelOutput, _) = kernelF(input `;`)

    println(kernel.code)
    opencl.executor.Executor.shutdown()

    assert(kernelOutput sameElements scalaOutput)
  }

  test("Transpose 2D array (OpenCL)") {
    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}
    opencl.executor.Executor.loadAndInit()

    val N = NamedVar("N", StartFromRange(1))
    val M = NamedVar("M", StartFromRange(1))
    val f = fun(ArrayType(N, ArrayType(M, float)))(xs => xs :>> transpose)

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(1,1)(TypeInference(f, Map()).toPhrase)
    val kernelF = kernel.as[ScalaFunction`(`Array[Array[Float]]`)=>`Array[Float]]

    val random = new Random()
    val actualN = 9
    val actualM = 6
    val input = Array.fill(actualN)(Array.fill(actualM)(random.nextFloat()))
    val scalaOutput = input.transpose.flatten

    val (kernelOutput, _) = kernelF(input `;`)

    println(kernel.code)
    opencl.executor.Executor.shutdown()

    assert(kernelOutput sameElements scalaOutput)
  }

  test("TransposeArrayDep (OpenCL)") {
    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}
    opencl.executor.Executor.loadAndInit()


    val f = nFun(n => nFun(m => fun(ArrayType(n, DepArrayType(m, i => ArrayType(i + 1, float))))(xs => xs :>> transpose :>> depMapSeq(fun (x => x)))))

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(1,1)(TypeInference(f, Map()).toPhrase)
    val kernelF = kernel.as[ScalaFunction`(`Int `,` Int `,` Array[Array[Array[Float]]]`)=>`Array[Float]]

    val random = new Random()
    val actualN = 9
    val actualM = 6
    val input = Array.fill(actualN)(Array.tabulate(actualM)(i => Array.fill(i + 1)(random.nextFloat())))
    val scalaOutput = input.transpose.flatten.flatten

    val (kernelOutput, _) = kernelF(actualN `,` actualM `,` input)

    println(kernel.code)
    opencl.executor.Executor.shutdown()

    assert(kernelOutput sameElements scalaOutput)
  }
}
