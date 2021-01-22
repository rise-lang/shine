package shine.DPIA.Primitives

import rise.core.DSL._
import rise.core.primitives._
import rise.core.types._
import util.gen.c.function
import util.{Execute, gen}

class Transpose extends test_util.Tests {
  test("Simple transpose should produce the expected result on a test") {
    def checkResult(e: rise.core.Expr) = {
      val transposeFun = function("transpose").asStringFromExpr(e)

      val testCode =
        s"""
#include <stdio.h>

$transposeFun

int main(int argc, char** argv) {
  const int N = 6;
  const int M = 10;

  int input[N * M];
  for (int i = 0; i < (N * M); i++) { input[i] = i; }

  int output[M * N];
  transpose(output, N, M, input);

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

    val gatherExp = depFun((n: Nat, m: Nat) => fun(ArrayType(n, ArrayType(m, int)))(a =>
      a |> transpose |> mapSeq(mapSeq(fun(x => x)))
    ))
    val scatterExp = depFun((n: Nat, m: Nat) => fun(ArrayType(n, ArrayType(m, int)))(a =>
      a |> mapSeq(mapSeq(fun(x => x))) |> transpose
    ))
    checkResult(gatherExp)
    checkResult(scatterExp)
  }

/* TODO
  test("'Type level transposition' with join->split (OpenCL 2D)") {
    val f = depFun((n: Nat) => depFun((m: Nat) => fun(ArrayType(n, ArrayType(m, float)))(xs => xs :>> join :>> split(m))))

    val actualN = 9
    val actualM = 6

    case class Run() extends SimpleRunOpenCLProgram(false) {
      override type Input = Array[Array[Float]]

      override def dpiaProgram: Expr = f

      override protected def makeInput(random: Random): Array[Array[Float]] = {
        Array.fill(actualN)(Array.fill(actualM)(random.nextFloat()))
      }

      override protected def runScalaProgram(input: Array[Array[Float]]): Array[Float] = {
        input.flatten
      }

      override protected def runKernel(k: KernelWithSizes, input: Array[Array[Float]]): (Array[Float], TimeSpan[ms]) = {
        import idealised.OpenCL._

        val kernelFun = k.as[ScalaFunction `(` Int `,` Int `,` Input `)=>` Array[Float]]
        kernelFun(actualN `,` actualM `,` input)
      }
    }

    Run().run(1, 1).correctness.check()
  }

  test("Transpose 2D array (OpenCL)") {
    val f = depFun((n: Nat) => depFun((m: Nat) => fun(ArrayType(n, ArrayType(m, float)))(xs => xs :>> transpose)))

    val actualN = 9
    val actualM = 6

    case class Run() extends SimpleRunOpenCLProgram(false) {
      override type Input = Array[Array[Float]]

      override def dpiaProgram: Expr = f

      override protected def makeInput(random: Random): Array[Array[Float]] = {
        Array.fill(actualN)(Array.fill(actualM)(random.nextFloat()))
      }

      override protected def runScalaProgram(input: Array[Array[Float]]): Array[Float] = {
        input.transpose.flatten
      }

      override protected def runKernel(k: KernelWithSizes, input: Array[Array[Float]]): (Array[Float], TimeSpan[ms]) = {
        import idealised.OpenCL._

        val kernelFun = k.as[ScalaFunction `(` Int `,` Int `,` Input `)=>` Array[Float]]
        kernelFun(actualN `,` actualM `,` input)
      }
    }

    Run().run(1, 1).correctness.check()
  }

  test("TransposeArrayDep (OpenCL)") {
    val f = depFun((n: Nat) => depFun((m: Nat) => fun(ArrayType(n, DepArrayType(m, i => ArrayType(i + 1, float))))(xs => xs :>> transpose :>> depMapSeq(fun(x => x)))))

    val actualN = 9
    val actualM = 6

    case class Run() extends SimpleRunOpenCLProgram(false) {
      override type Input = Array[Array[Array[Float]]]

      override def dpiaProgram: Expr = f

      override protected def makeInput(random: Random): Array[Array[Array[Float]]] = {
        Array.fill(actualN)(Array.tabulate(actualM)(i => Array.fill(i + 1)(random.nextFloat())))
      }

      override protected def runScalaProgram(input: Array[Array[Array[Float]]]): Array[Float] = {
        input.transpose.flatten.flatten
      }

      override protected def runKernel(k: KernelWithSizes, input: Array[Array[Array[Float]]]): (Array[Float], TimeSpan[ms]) = {
        import idealised.OpenCL._

        val kernelFun = k.as[ScalaFunction `(` Int `,` Int `,` Input `)=>` Array[Float]]
        kernelFun(actualN `,` actualM `,` input)
      }
    }

    Run().run(1, 1).correctness.check()
  }
*/
}
