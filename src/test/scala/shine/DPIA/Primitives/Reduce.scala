package shine.DPIA.Primitives

import arithexpr.arithmetic.Cst
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types.{AddressSpace, _}
import rise.openCL.primitives.oclReduceSeq
import shine.OpenCL.KernelExecutor.KernelNoSizes.fromKernelModule
import shine.OpenCL._
import util.gen
import util.gen.c.function

import scala.language.postfixOps

class Reduce extends test_util.TestsWithExecutor {
  val add = fun(a => fun(b => a + b))

  test("Simple example should generate syntactically valid C code" +
    "with one loop") {
    val e =
      depFun((n: Nat) => fun(ArrayType(n, f32))(a =>
        a |> reduceSeq(add)(lf32(0.0f))))

    val code = function.asStringFromExpr(e)

    "for".r.findAllIn(code).length shouldBe 1
  }

  test("Fusing a reduce into a map should generate syntactically" +
    "valid C code") {
    val e =
      depFun((h: Nat, w: Nat) =>
        fun(ArrayType(h, ArrayType(w, f32)))(a =>
          a |> map(reduceSeq(add)(lf32(0.0f))) |> mapSeq(fun(x => x))
        ))

    function.asStringFromExpr(e)
  }

  test("Fusing a reduce into another should generate syntactically" +
    "valid C code with two loops") {
    val e =
      depFun((h: Nat, w: Nat) =>
        fun(ArrayType(h, ArrayType(w, f32)))(a =>
          a |> map(reduceSeq(add)(lf32(0.0f))) |> reduceSeq(add)(lf32(0.0f))
      ))

    val code = function.asStringFromExpr(e)

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("oclReduceSeq produces correct result over 2D array" +
    "using private memory") {
    import scala.util.Random

    val random = new Random()

    val initExp = depFun((n: Nat) =>
      generate(fun(IndexType(n))(_ => lf32(0.0f)))
        |> mapSeq (fun(x => x)))

    val e = depFun((m: Nat, n: Nat) =>
      fun(m`.`n`.`f32)(arr => arr
        |> oclReduceSeq (AddressSpace.Private)
          (fun((in1, in2) => zip (in1) (in2) |> mapSeq (fun(t => t.`1` + t.`2`))))
          (initExp (n))
        |> mapSeq (fun(x => x))))

    val m = 64
    val n = 64
    val A = Array.fill(m, n)((random.nextInt(10) + 1).toFloat)

    val gold = A.reduce((row1, row2) => row1.zip(row2).map(in => in._1 + in._2))

    val runKernel = gen.opencl.kernel.fromExpr(e(m)(n)).as[In `=`
      Array[Array[Float]], Out[Array[Float]]]
    val (out, _)  = runKernel(LocalSize(1), GlobalSize(1))(A`;`)

    assertResult(gold)(out)
  }

  test("Record access to specify initial accumulator value" +
    "of reduceSeq generates syntactically valid C code") {
    val n = 8
    val initRecordExp =
      (zip (generate(fun(IndexType(n))(_ => lf32(0.0f))))
           (generate(fun(IndexType(n))(_ => lf32(0.0f))))
        |> primitives.idx(natAsIndex (n) (l(Cst(0)))))

    def e(init : ToBeTyped[Expr]): ToBeTyped[Expr] = depFun((n: Nat) =>
      fun(n`.`f32)(arr =>
        arr |> reduceSeq (fun(_ + _))  (init)))

    logger.debug("Fst:")
    function.asStringFromExpr(e(initRecordExp.`1`))
    logger.debug("Snd:")
    function.asStringFromExpr(e(initRecordExp.`2`))
  }

  test("Record access to specify initial accumulator value" +
    "of oclReduceSeq produces correct result") {
    import scala.util.Random

    val random = new Random()

    val initRecordExp = makePair(lf32(0.0f))(lf32(0.0f))

    def e(init : ToBeTyped[Expr]) = depFun((n: Nat) =>
      fun(n`.`f32)(arr =>
        arr |> oclReduceSeq (AddressSpace.Global) (fun(_ + _))  (init)))

    val n = 64
    val A = Array.fill(n)((random.nextInt(10) + 1).toFloat)

    val gold = A.sum

    def runKernel(initWithRecordAccess: ToBeTyped[Expr]) =
      gen.opencl.kernel.fromExpr(e(initWithRecordAccess))
        .as[In `=`Int`,`Array[Float], Out[Array[Float]]]

    val (out1, _) =
      runKernel(initRecordExp.`1`)(LocalSize(1), GlobalSize(1))(n `,` A)
    val (out2, _) =
      runKernel(initRecordExp.`2`)(LocalSize(1), GlobalSize(1))(n `,` A)

    assertResult(gold)(out1(0))
    assertResult(gold)(out2(0))
  }
}
