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
    val N = SizeVar("N")
    val lenF = (i:NatIdentifier) => i + 1

    val slideExample = fun(ArrayType(N, float))(xs => xs :>> partition(3, lenF) :>> depMapSeq(mapSeq(fun(x => x + 1.0f))))

    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(slideExample, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)
  }

  test("Partition threeway with pad and unrolling") {
    opencl.executor.Executor.loadAndInit()
    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}

    val N = SizeVar("N")
    val padAmount = 3

    val lenF =  SteppedCase(3, N, 3) _

    val padAndPartition = fun(ArrayType(N, float))(xs => xs :>>
      pad(padAmount, padAmount,0.0f) :>>
      partition(3, lenF) :>>
      depMapSeqUnroll(mapSeq(fun(x => x + 1.0f))))

    val p = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(padAndPartition, Map()).toPhrase, localSize = 1, globalSize = 1)
    val kernelF = p.as[ScalaFunction`(`Array[Float]`)=>`Array[Float]]
    val input = Array.fill(128)(5.0f)
    val (output, time) = kernelF(input `;`)

    val scalaOutput = (Array.fill(padAmount)(0.0f) ++ input ++ Array.fill(padAmount)(0.0f)).map(x => x + 1.0f)

    assert(output.zip(scalaOutput).forall{ case (x,y) => x - y < 0.01 })

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
    opencl.executor.Executor.shutdown()
  }

  test("Partition 2D") {
    opencl.executor.Executor.loadAndInit()
    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}

    def partition2D(size:ArithExpr, lenF:NatIdentifier => Nat):Expr[DataType -> DataType] = {
      map(
        partition(size, lenF)
      ) >>> partition(size, lenF)
    }

    val N = SizeVar("N")

    val lenF:NatIdentifier => Nat = (m:NatIdentifier) => SteppedCase(1, N-2, 1)(m)
    val f = fun(ArrayType(N, ArrayType(N, float)))(xs =>
        xs :>> partition2D(Cst(3), lenF) :>> depMapSeq(mapSeq(depMapSeq(mapSeq(fun(x => x)))))
      )
    val actualN = 128

    val p = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(f, Map()).toPhrase, localSize = 1, globalSize = 1)
    println(p.code)
    val kernelF = p.as[ScalaFunction`(`Array[Array[Float]]`)=>`Array[Float]]
    val input = Array.fill(actualN)(Array.fill(actualN)(1.0f))
    val (output, _) = kernelF(input `;`)


    val scalaOutput = input.flatten

    assert(output.zip(scalaOutput).forall{ case (x,y) => x - y < 0.01 })

    val code = p.code
    SyntaxChecker.checkOpenCL(code)

    opencl.executor.Executor.shutdown()
  }

  /*test("Partition 2D transposed") {
    opencl.executor.Executor.loadAndInit()
    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}

    def partition2D(size:ArithExpr, lenF:NatIdentifier => Nat):Expr[DataType -> DataType] = {
      map(
        partition(size, lenF)
      ) >>> partition(size, lenF)
    }

    val N = SizeVar("N")

    val outerSize = 3
    val lenF:NatIdentifier => Nat = (m:NatIdentifier) => SteppedCase(outerSize, N-outerSize*2, outerSize)(m)
    val f = fun(ArrayType(N, ArrayType(N, float)))(xs =>
      xs :>> partition2D(Cst(3), lenF) :>> depMapSeqUnroll(fun(inner => inner :>> transpose :>> depMapSeqUnroll(mapSeq(mapSeq(fun(x => x))))))
    )
    val actualN = 16

    val p = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(f, Map()).toPhrase, localSize = 1, globalSize = 1)
    println(p.code)
    val kernelF = p.as[ScalaFunction`(`Array[Array[Float]]`)=>`Array[Float]]
    val rand = new Random()
    val input = Array.tabulate(actualN)(i => Array.tabulate(actualN)(j => i + j + 0.0f))
    val (output, _) = kernelF(input `;`)


    val scalaOutput = input.flatten

    val correct = output.zip(scalaOutput).forall{ case (x,y) =>
      x - y < 0.01
    }

    assert(correct)
    val code = p.code
    SyntaxChecker.checkOpenCL(code)

    opencl.executor.Executor.shutdown()
  }*/
}
