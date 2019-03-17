package idealised.DPIA.Primitives

import idealised.OpenCL.SurfaceLanguage.DSL.oclFun
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

class Generate extends idealised.util.Tests {
  test("Very simple one-dimensional generate generates syntactically correct code in C.") {
    val id = fun(x => x)
    val simpleGenerate = nFun(n => generate(fun(IndexType(n))(i => cast(double, i) + 1.0)) :>> mapSeq(id))
    val program = idealised.C.ProgramGenerator.makeCode(TypeInference(simpleGenerate, Map()).convertToPhrase)

    println(program.code)
    SyntaxChecker(program.code)
  }

  test("Very simplistic generate, using index and maximum index size" +
    "generates syntactically correct code in C.") {
    val id = fun(x => x)
    val simpleGenerate =
      nFun(n => generate(fun(IndexType(n))(i => indexAsNat(i) + n)) :>> mapSeq(id))
    val program = idealised.C.ProgramGenerator.makeCode(TypeInference(simpleGenerate, Map()).convertToPhrase)

    println(program.code)
    SyntaxChecker(program.code)
  }

  test("One-dimensional generate generates syntactically correct code in C.") {
    val add = fun(x => x._1 + x._2)
    val simpleMap = nFun(n => fun(ArrayType(n, double))(in =>
      zip(in,
        generate(fun(IndexType(n))(i =>
          foreignFun(double, "callCos", (double, "x"), "{ return cos(x); }",
            cast(double, indexAsNat(i) + n)))
      )) :>>
        mapSeq(add)))

    val phrase = TypeInference(simpleMap, Map()).convertToPhrase
    val program = idealised.C.ProgramGenerator.makeCode(phrase)
    println(program.code)

    SyntaxChecker(program.code)
  }

  test("Two-dimensional generate generates syntactically correct code in C.") {
    val add = fun(x => x._1 + x._2)
    val simpleMap = nFun((m, n) => fun(ArrayType(m, ArrayType(n, double)))(in =>
      zip(in,
        generate(fun(IndexType(m))(i =>
          generate(fun(IndexType(n))(j =>
            foreignFun(double, "callCos", (double, "x"), "{ return cos(x); }",
              // TODO how to implicitly cast, with Nat on the lhs of a binary op?
              cast(double, (indexAsNat(j) + n) * indexAsNat(i) + m))
            )))))
        :>> mapSeq(fun(t => zip(t._1, t._2) :>> mapSeq(add)))
    ))

    val phrase = TypeInference(simpleMap, Map()).convertToPhrase
    val program = idealised.C.ProgramGenerator.makeCode(phrase)
    println(program.code)

    SyntaxChecker(program.code)
  }

  test("Syntactically correct code for complex Generate can be generated in C.") {
    val N = 8
    val LPrevIter = 1
    val p = 2

    val reorderedB =
      generate(fun(IndexType(LPrevIter))(i =>
        generate(fun(IndexType(p))(j =>
          generate(fun(IndexType(p))(k => {
            val exponentWoMinus2 =
              fmapNatExpr(fmapNatExpr(indexAsNat(j), j => j * LPrevIter) +
                fmapNatExpr(indexAsNat(i), i => i) * fmapNatExpr(indexAsNat(k), k => k / (p * LPrevIter)), x => x)
            val exponent = cast(double, exponentWoMinus2) * -2.0
            tuple(cast(float, oclFun("cospi", double, double, exponent)),
              cast(float, oclFun("sinpi", double, double, exponent)))
          }))))))

    val id = fun(x => x)
    val generateSth = fun(ArrayType(N, float))(_ =>
      reorderedB :>> mapSeq(mapSeq(mapSeq(id))))

    val phrase = TypeInference(generateSth, Map()).convertToPhrase
    val program = idealised.OpenCL.KernelGenerator.makeCode(phrase)
    println(program.code)
    SyntaxChecker.checkOpenCL(program.code)
  }
}
