package idealised.DPIA.Primitives

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
      nFun(n => generate(fun(IndexType(n))(i => asNat(i) + n)) :>> mapSeq(id))
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
            cast(double, asNat(i) + n)))
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
              cast(double, (asNat(j) + n) * asNat(i) + m))
            )))))
        :>> mapSeq(fun(t => zip(t._1, t._2) :>> mapSeq(add)))
    ))

    val phrase = TypeInference(simpleMap, Map()).convertToPhrase
    val program = idealised.C.ProgramGenerator.makeCode(phrase)
    println(program.code)

    SyntaxChecker(program.code)
  }
}
