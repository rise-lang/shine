package rise.OpenCL

import shine.OpenCL._
import rise.core._
import rise.core.DSL._
import rise.core.types._
import rise.OpenCL.DSL._
import util.gen

import scala.language.{postfixOps, reflectiveCalls}

class ExecuteOpenCL extends shine.test_util.TestsWithExecutor {
  test("Running a simple kernel with generic input size") {
    val f: Expr = nFun(n => fun(ArrayType(n, int))(
      xs => xs |> mapSeq(fun(x => x + l(1)))))

    val kernel = gen.OpenCLKernel(f)

    val kernelF = kernel.as[ScalaFunction`(`Int`,`Array[Int]`)=>`Array[Int]].withSizes(LocalSize(1), GlobalSize(1))
    val xs = Array.fill(8)(0)

    val (result, time) = kernelF(8`,`xs)
    println(time)

    val gold = Array.fill(8)(1)
    assertResult(gold)(result)
  }

  test("Running a simple kernel with fixed input size") {
    val n = 8
    val f: Expr = fun(ArrayType(n, int))(
      xs => xs |> mapSeq(fun(x => x + l(1))))

    val kernel = gen.OpenCLKernel(f)

    val kernelF = kernel.as[ScalaFunction`(`Array[Int]`)=>`Array[Int]].withSizes(LocalSize(1), GlobalSize(1))
    val xs = Array.fill(n)(0)

    val (result, time) = kernelF(xs`;`)
    println(time)

    val gold = Array.fill(n)(1)
    assertResult(gold)(result)
  }

  test("Running a simple kernel with nat-dependent split") {
    val n = 8
    val f: Expr = fun(ArrayType(n, int))(
      xs => nFun(s => xs |> split(s) |> mapSeq(mapSeq(fun(x => x + l(1)))) |> join))

    val kernel = gen.OpenCLKernel(f)

    val kernelF = kernel.as[ScalaFunction`(`Array[Int]`,`Int`)=>`Array[Int]]
    val xs = Array.fill(n)(0)

    val (result, _) = kernelF(LocalSize(1), GlobalSize(1))(xs`,`n)

    val gold = Array.fill(n)(1)
    assertResult(gold)(result)
  }

  test("Running a simple kernel with multiple generic input sizes") {
    val m = 4
    val n = 8
    val f: Expr = nFun(m => nFun(n => fun(ArrayType(m, ArrayType(n, int)))(xs =>
      xs |> mapSeq(mapSeq(fun(x => x + l(1)))))))

    val kernel = gen.OpenCLKernel(f)

    val kernelF = kernel.as[ScalaFunction`(`Int`,`Int`,`Array[Array[Int]]`)=>`Array[Int]].withSizes(LocalSize(1), GlobalSize(1))
    val xs = Array.fill(m)(Array.fill(n)(0))

    val (result, time) =  kernelF(m`,`n`,`xs)
    println(time)

    val gold = Array.fill(m)(Array.fill(n)(1)).flatten
    assertResult(gold)(result)
  }

  test("Running a simple kernel mixing nat-dependent with normal functions") {
    val n = 8
    val s = 2
    val f: Expr = nFun(n => fun(ArrayType(n, int))(xs => nFun(s =>
      xs |> split(s) |> mapSeq(mapSeq(fun(x => x + l(1)))) |> join)))

    val kernel = gen.OpenCLKernel(f)

    val kernelF = kernel.as[ScalaFunction`(`Int`,`Array[Int]`,`Int`)=>`Array[Int]]

    val xs = Array.fill(n)(2)
    val (result, _) =  kernelF(LocalSize(1), GlobalSize(1))(n`,`xs`,`s)

    val gold = xs.map(x => x + 1)
    assertResult(gold)(result)
  }

  test("Running a simple kernel with fixed input size and multiple threads") {
    val n = 8
    val f: Expr = fun(ArrayType(n, int))(xs =>
      xs |> mapGlobal(fun(x => x + l(1))))

    val kernel = gen.OpenCLKernel(f)

    val kernelF = kernel.as[ScalaFunction`(`Array[Int]`)=>`Array[Int]].withSizes(LocalSize(1), GlobalSize(1))
    val xs = Array.fill(n)(0)

    val (result, time) =  kernelF(xs`;`)
    println(time)

    val gold = Array.fill(n)(1)
    assertResult(gold)(result)
  }
}