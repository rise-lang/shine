package idealised.DPIA.Primitives

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

class Reduce extends idealised.util.Tests {
  val add = fun(a => fun(b => a + b))

  test("Simple example should generate syntactic valid C code with one loop") {
    val e =
      nFun(n =>
        fun(ArrayType(n, float))(a => a :>> reduceSeq(add, 0.0f)))

    val p = idealised.C.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(e, Map())))
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 1
  }

  test("Folding a reduce into a map should generate syntactic valide C code") {
    val e =
      nFun(h => nFun(w =>
        fun(ArrayType(h, ArrayType(w, float)))(a =>
          a :>> map(reduceSeq(add, 0.0f)) :>> mapSeq(fun(x => x))
        )))

    val p = idealised.C.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(e, Map())))
    val code = p.code
    SyntaxChecker(code)
    println(code)
  }

  test("Folding a reduce into another should generate syntactic valid C code with two loops") {
    val e =
      nFun(h => nFun(w =>
        fun(ArrayType(h, ArrayType(w, float)))(a =>
        a :>> map(reduceSeq(add, 0.0f)) :>> reduceSeq(add, 0.0f)
      )))

    val p = idealised.C.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(e, Map())))
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("Folding a reduce into a map from the other side should generate syntactic valide C code") {
    val e =
      nFun(h => nFun(w =>
        fun(ArrayType(h, ArrayType(w, float)))(a =>
          a :>> mapSeq(mapSeq(fun(x => x))) :>> map(reduceSeq(add, 0.0f))
        )))

    val p = idealised.C.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(e, Map())))
    val code = p.code
    SyntaxChecker(code)
    println(code)
  }
}
