package shine.DPIA.Primitives

import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.types._
import rise.core.types.AddressSpace
import util.gen
import shine.OpenCL._
import rise.OpenCL.DSL._
import rise.core.{Expr, Literal}
import rise.core.semantics.NatData

import scala.language.postfixOps

class Reduce extends shine.test_util.TestsWithExecutor {
  val add = fun(a => fun(b => a + b))

  test("Simple example should generate syntactic valid C code with one loop") {
    val e =
      nFun(n => fun(ArrayType(n, f32))(a =>
        a |> reduceSeq(add)(l(0.0f))))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 1
  }

  test("Fusing a reduce into a map should generate syntactic valid C code") {
    val e =
      nFun(h => nFun(w =>
        fun(ArrayType(h, ArrayType(w, f32)))(a =>
          a |> map(reduceSeq(add)(l(0.0f))) |> mapSeq(fun(x => x))
        )))

    gen.CProgram(e)
  }

  test("Fusing a reduce into another should generate syntactic valid C code with two loops") {
    val e =
      nFun(h => nFun(w =>
        fun(ArrayType(h, ArrayType(w, f32)))(a =>
          a |> map(reduceSeq(add)(l(0.0f))) |> reduceSeq(add)(l(0.0f))
      )))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("oclReduceSeq produces correct result over 2D array using private memory") {
    import scala.util.Random

    val random = new Random()

    val initExp = nFun(n =>
      generate(fun(IndexType(n))(_ => l(0.0f)))
        |> mapSeq (fun(x => x)))

    val e = nFun((m, n) =>
              fun(m`.`n`.`f32)(arr =>
                arr |> oclReduceSeq (AddressSpace.Private)
                                    (fun((in1, in2) => zip (in1) (in2) |> mapSeq (fun(t => t._1 + t._2))))
                                    (initExp (n))
                    |> mapSeq (fun(x => x))))

    val m = 64
    val n = 64
    val A = Array.fill(m, n)((random.nextInt(10) + 1).toFloat)

    val gold = A.reduce((row1, row2) => row1.zip(row2).map(in => in._1 + in._2))

    val runKernel = gen.OpenCLKernel(e(m)(n)).as[ScalaFunction `(`
      Array[Array[Float]] `)=>` Array[Float]]
    val (out, _)  = runKernel(LocalSize(1), GlobalSize(1))(A`;`)

    assertResult(gold)(out)
  }

  test("Record access to specify initial accumulator value of reduceSeq generates syntactically valid C code") {
    val n = 8
    val initRecordExp =
      (zip (generate(fun(IndexType(n))(_ => l(0.0f))) |> mapSeq (fun(x => x)))
           (generate(fun(IndexType(n))(_ => l(0.0f))))
        |> idx(natAsIndex (n) (Literal(NatData(0)))))

    def e(init : Expr) = nFun(n =>
      fun(n`.`f32)(arr =>
        arr |> reduceSeq (fun(_ + _))  (init)))

    println("Fst:")
    gen.CProgram(e(initRecordExp._1)).code
    println("Snd:")
    gen.CProgram(e(initRecordExp._2)).code
  }

  test("Record access to specify initial accumulator value of oclReduceSeq produces correct result") {
    import scala.util.Random

    val random = new Random()

    val initRecordExp = pair(l(0.0f), l(0.0f))

    def e(init : Expr) = nFun(n =>
      fun(n`.`f32)(arr =>
        arr |> oclReduceSeq (AddressSpace.Global) (fun(_ + _))  (init)))

    val n = 64
    val A = Array.fill(n)((random.nextInt(10) + 1).toFloat)

    val gold = A.sum

    def runKernel(initWithRecordAccess: Expr) =
      gen.OpenCLKernel(e(initWithRecordAccess)).as[ScalaFunction `(`Int`,`Array[Float]`)=>`Array[Float]]

    val (out1, _) = runKernel(initRecordExp._1)(LocalSize(1), GlobalSize(1))(n `,` A)
    val (out2, _) = runKernel(initRecordExp._2)(LocalSize(1), GlobalSize(1))(n `,` A)

    assertResult(gold)(out1(0))
    assertResult(gold)(out2(0))
  }
}
