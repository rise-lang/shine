package idealised.DPIA.Primitives

import idealised.OpenCL.`,`
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.{Execute, SyntaxChecker}
import lift.arithmetic.{NamedVar, StartFromRange}

import scala.util.Random

class Transpose extends idealised.util.Tests {
  test("Simple transpose should produce the expected result on a test") {
    def checkResult[T <: idealised.SurfaceLanguage.Types.Type]
    (e: idealised.SurfaceLanguage.Expr[T]) =
    {
      val p = idealised.C.ProgramGenerator.makeCode(TypeInference(e, Map()).toPhrase)
      SyntaxChecker(p.code)
      println(p.code)

      val testCode = s"""
#include <stdio.h>

${p.code}

int main(int argc, char** argv) {
  const int N = 6;
  const int M = 10;

  int input[N * M];
  for (int i = 0; i < (N * M); i++) { input[i] = i; }

  int output[M * N];
  ${p.function.name}(output, N, M, input);

  for (int i = 0; i < M; i++) {
    for (int j = 0; j < N; j++) {
      int o = output[i * N + j];
      int expected = j * M + i;

      if (o != expected) {
        fprintf(stderr, "(%i, %i)\\n", i, j);
        fprintf(stderr, "found %i, expected %i\\n", o, expected);
        return 1;
      }
    }
  }

  return 0;
}
"""
      Execute(testCode)
    }
    val gatherExp = nFun(n => nFun(m => fun(ArrayType(n, ArrayType(m, int)))(a =>
      a :>> transpose() :>> mapSeq(mapSeq(fun(x => x)))
    )))
    val scatterExp = nFun(n => nFun(m => fun(ArrayType(n, ArrayType(m, int)))(a =>
      a :>> mapSeq(mapSeq(fun(x => x))) :>> transpose()
    )))
    checkResult(gatherExp)
    checkResult(scatterExp)
  }


  test("'Type level transposition' with join->split (OpenCL 2D)") {
    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}
    opencl.executor.Executor.loadAndInit()


    val f = nFun(n => nFun(m => fun(ArrayType(n, ArrayType(m, float)))(xs => xs :>> join :>> split(m))))

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(1,1)(TypeInference(f, Map()).toPhrase)
    val kernelF = kernel.as[ScalaFunction`(`Int `,` Int `,` Array[Array[Float]]`)=>`Array[Float]]

    val random = new Random()
    val actualN = 9
    val actualM = 6
    val input = Array.fill(actualN)(Array.fill(actualM)(random.nextFloat()))
    val scalaOutput = input.flatten

    val (kernelOutput, _) = kernelF(actualN `,` actualM `,` input)

    println(kernel.code)
    opencl.executor.Executor.shutdown()

    assert(kernelOutput sameElements scalaOutput)
  }

  test("Transpose 2D array (OpenCL)") {
    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}
    opencl.executor.Executor.loadAndInit()


    val f = nFun(n => nFun(m => fun(ArrayType(n, ArrayType(m, float)))(xs => xs :>> transpose)))

    val kernel = idealised.OpenCL.KernelGenerator.makeCode(1,1)(TypeInference(f, Map()).toPhrase)
    val kernelF = kernel.as[ScalaFunction`(`Int `,` Int `,` Array[Array[Float]]`)=>`Array[Float]]

    val random = new Random()
    val actualN = 9
    val actualM = 6
    val input = Array.fill(actualN)(Array.fill(actualM)(random.nextFloat()))
    val scalaOutput = input.transpose.flatten

    val (kernelOutput, _) = kernelF(actualN `,` actualM `,` input)

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
