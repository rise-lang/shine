package idealised.DPIA.Primitives

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import idealised.util.gen

class Generate extends idealised.util.Tests {
  val id = fun(x => x)
  val addT = fun(x => fst(x) + snd(x))
  val cos = foreignFun("callCos", Seq("x"), "{ return cos(x); }", double._R ->: double._R)

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
    val e = nFun(n => fun(ArrayType(n, double))(in =>
      zip(in)(generate(fun(IndexType(n))(i => cos(cast(indexAsNat(i) + n)))))
      |> mapSeq(addT)
    ))
    gen.CProgram(e)
  }

  test("Two-dimensional generate generates syntactically correct code in C.") {
    val e = nFun(m => nFun(n => fun(ArrayType(m, ArrayType(n, double)))(in =>
      zip(in)(
        generate(fun(IndexType(m))(i =>
          generate(fun(IndexType(n))(j =>
            cos(cast((indexAsNat(j) + n) * indexAsNat(i) + m))
          )))))
        |> mapSeq(fun(t => zip(fst(t))(snd(t)) |> mapSeq(addT)))
    )))
    gen.CProgram(e)
  }

  // FIXME: mapNatExpr and natFromNatExpr
  /*
  ignore("Syntactically correct code for complex Generate can be generated in C.") {
    val N = 8
    val LPrevIter = 1
    val p = 2

    val reorderedB =
      generate(fun(IndexType(LPrevIter))(i =>
        generate(fun(IndexType(p))(j =>
          generate(fun(IndexType(p))(k => {
            val exponentWoMinus2 =
              mapNatExpr(mapNatExpr(indexAsNat(j), j => j * LPrevIter) +
                mapNatExpr(indexAsNat(i), i => i) * mapNatExpr(indexAsNat(k), k => k / (p * LPrevIter)), x => x)
            val exponent = cast(double, exponentWoMinus2) * -2.0
            tuple(cast(float, oclFun("cospi", double, double, exponent)),
              cast(float, oclFun("sinpi", double, double, exponent)))
          }))))))

    val id = fun(x => x)
    val generateSth = fun(ArrayType(N, float))(_ =>
      reorderedB :>> mapSeq(mapSeq(mapSeq(id))))

    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(generateSth, Map()))
    val program = idealised.OpenCL.KernelGenerator.makeCode(phrase)
    println(program.code)
    SyntaxChecker.checkOpenCL(program.code)
  }
  */
}
