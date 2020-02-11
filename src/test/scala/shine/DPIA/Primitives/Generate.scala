package shine.DPIA.Primitives

import rise.core._
import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.types._
import util.gen

class Generate extends shine.test_util.Tests {
  val id = fun(x => x)
  val addT = fun(x => fst(x) + snd(x))
  val cos = foreignFun("callCos", Seq("x"), "{ return cos(x); }", f64 ->: f64)

  test("Very simple one-dimensional generate generates syntactically correct code in C.") {
    val e = nFun(n => generate(fun(IndexType(n))(i => cast(i) + l(1.0))) |> mapSeq(id))
    gen.CProgram(e)
  }

  test("Very simplistic generate, using index and maximum index size" +
    "generates syntactically correct code in C.") {
    val e =
      nFun(n => generate(fun(IndexType(n))(i => indexAsNat(i) + n)) |> mapSeq(id))
    gen.CProgram(e)
  }

  test("One-dimensional generate generates syntactically correct code in C.") {
    val e = nFun(n => fun(ArrayType(n, f64))(in =>
      zip(in)(generate(fun(IndexType(n))(i => cos(cast(indexAsNat(i) + n)))))
      |> mapSeq(addT)
    ))
    gen.CProgram(e)
  }

  test("Two-dimensional generate generates syntactically correct code in C.") {
    val e = nFun(m => nFun(n => fun(ArrayType(m, ArrayType(n, f64)))(in =>
      zip(in)(
        generate(fun(IndexType(m))(i =>
          generate(fun(IndexType(n))(j =>
            cos(cast((indexAsNat(j) + n) * indexAsNat(i) + m))
          )))))
        |> mapSeq(fun(t => zip(fst(t))(snd(t)) |> mapSeq(addT)))
    )))
    gen.CProgram(e)
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
            val exponent = (cast(exponentWoMinus2) :: f64) * l(-2.0)
            pair(cast(foreignFun("cospi", f64 ->: f64)(exponent)) :: f32,
              cast(foreignFun("sinpi", f64 ->: f64)(exponent)) :: f32)
          }))))))

    val id = fun(x => x)
    val generateSth = fun(ArrayType(N, f32))(_ =>
      reorderedB >> mapSeq(mapSeq(mapSeq(id))))

    gen.OpenCLKernel(generateSth)
  }
}
