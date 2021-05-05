package shine.DPIA.Primitives

import rise.core.{Expr, Lambda}
import rise.core.DSL._
import rise.core.primitives._
import Type._
import rise.core.types._
import util.gen
import util.gen.c.function

class Generate extends test_util.Tests {
  val id: ToBeTyped[Lambda] = fun(x => x)
  val addT: ToBeTyped[Lambda] = fun(x => fst(x) + snd(x))
  val cos: ToBeTyped[Expr] = foreignFun("callCos", Seq("x"), "{ return cos(x); }", f64 ->: f64)

  test("Very simple one-dimensional generate generates syntactically correct code in C.") {
    val e = depFun((n: Nat) => generate(fun(IndexType(n))(i => cast(i) + lf64(1.0))) |> mapSeq(id))
    function.asStringFromExpr(e)
  }

  test("Very simplistic generate, using index and maximum index size" +
    "generates syntactically correct code in C.") {
    val e =
      depFun((n: Nat) => generate(fun(IndexType(n))(i => indexAsNat(i) + n)) |> mapSeq(id))
    function.asStringFromExpr(e)
  }

  test("One-dimensional generate generates syntactically correct code in C.") {
    val e = depFun((n: Nat) => fun(ArrayType(n, f64))(in =>
      zip(in)(generate(fun(IndexType(n))(i => cos(cast(indexAsNat(i) + n)))))
      |> mapSeq(addT)
    ))
    function.asStringFromExpr(e)
  }

  test("Two-dimensional generate generates syntactically correct code in C.") {
    val e = depFun((m: Nat, n: Nat) => fun(ArrayType(m, ArrayType(n, f64)))(in =>
      zip(in)(
        generate(fun(IndexType(m))(i =>
          generate(fun(IndexType(n))(j =>
            cos(cast((indexAsNat(j) + n) * indexAsNat(i) + m))
          )))))
        |> mapSeq(fun(t => zip(fst(t))(snd(t)) |> mapSeq(addT)))
    ))
    function.asStringFromExpr(e)
  }

  ignore("Syntactically correct code for complex Generate can be generated in C.") {
    val N = 8
    val LPrevIter: Nat = 1
    val p = 2

    val reorderedB =
      generate(fun(IndexType(LPrevIter))(i =>
        generate(fun(IndexType(p))(j =>
          generate(fun(IndexType(p))(k => {
            val exponentWoMinus2 = (j * LPrevIter) + i * (k / (p * LPrevIter))
            val exponent = (cast(exponentWoMinus2) :: f64) * lf64(-2.0)
            makePair(cast(foreignFun("cospi", f64 ->: f64)(exponent)) :: f32)(
              cast(foreignFun("sinpi", f64 ->: f64)(exponent)) :: f32)
          }))))))

    val id = fun(x => x)
    val generateSth = fun(ArrayType(N, f32))(_ =>
      reorderedB >> mapSeq(mapSeq(mapSeq(id))))

    gen.opencl.kernel.fromExpr(generateSth)
  }
}
