package idealised.apps

import idealised.DPIA.Phrases.Phrase
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.NatIdentifier
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

class dot extends idealised.util.Tests {

  private def xsT(N : NatIdentifier) = ArrayType(N, float)
  private def ysT(N : NatIdentifier) = ArrayType(N, float)

  private val mult = fun(x => x._1 * x._2)
  private val add = fun((x, a) => x + a)

  private val simpleDotProduct = nFun(n => fun(xsT(n))(xs => fun(ysT(n))(ys =>
    zip(xs, ys) :>> mapSeq(mult) :>> reduceSeq(add, 0.0f)
  )))

  test("Simple dot product type inference works") {
    val typed = TypeInference(simpleDotProduct, Map())

    val N = Phrase.natParamsOfUncurriedFunction(typed.toPhrase).head
    assertResult(NatDependentFunctionType(N, FunctionType(xsT(N), FunctionType(ysT(N), float)))) {
      typed.t.get
    }
  }

  test("Simple dot product translation to phrase works and preserves types") {
    import idealised.DPIA.Types.float
    import idealised.DPIA._
    val phrase = TypeInference(simpleDotProduct, Map()).convertToPhrase

    val N = Phrase.natParamsOfUncurriedFunction(phrase).head
    val dt = float
    assertResult(N -> (exp"[$N.$dt]" -> (exp"[$N.$dt]" -> exp"[$dt]"))) {
      phrase.t
    }
  }

  // C
  test("Simple dot product compiles to syntactically correct C") {
    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(simpleDotProduct, Map()).toPhrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  // OpenMP
  test("Dot product CPU vector 1 compiles to syntactically correct OpenMP") {
    import idealised.OpenMP.SurfaceLanguage.DSL._

    val dotCPUVector1 = nFun(n => fun(xsT(n))(xs => fun(ysT(n))(ys =>
      zip(
        xs :>> asVector(4),
        ys:>> asVector(4)
      ) :>>
        split(2048 * 64) :>>
        mapPar(
          split(2048) >>>
            mapSeq(
              reduceSeq(fun(x => fun(a => mult(x) + a)), vectorize(4, 0.0f))
            )
        ) :>> join :>> asScalar
    )))

    val phrase = TypeInference(dotCPUVector1, Map()).toPhrase
    val p = idealised.OpenMP.ProgramGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Intel derived no warp dot product 1 compiles to syntactically correct OpenMP") {
    import idealised.OpenMP.SurfaceLanguage.DSL._

    val intelDerivedNoWarpDot1 = nFun(n =>
      fun(xsT(n))(xs => fun(ysT(n))(ys =>
        zip(
          xs :>> asVector(4),
          ys:>> asVector(4)
        ) :>>
          split(8192) :>>
          mapPar(
            split(8192) >>>
              mapSeq(
                reduceSeq(fun(x => fun(a => mult(x) + a)), vectorize(4, 0.0f))
              )
          ) :>> join :>> asScalar
      )))

    val phrase = TypeInference(intelDerivedNoWarpDot1, Map()).toPhrase
    val p = idealised.OpenMP.ProgramGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Dot product CPU 1 compiles to syntactically correct OpenMP") {
    import idealised.OpenMP.SurfaceLanguage.DSL._

    val dotCPU1 = nFun(n => fun(xsT(n))(xs => fun(ysT(n))(ys =>
      zip(xs, ys) :>>
        split(2048 * 128) :>>
        mapPar(
          split(2048) >>>
            mapSeq(
              reduceSeq(fun(x => fun(a => mult(x) + a)), 0.0f)
            )
        ) :>> join
    )))

    val phrase = TypeInference(dotCPU1, Map()).toPhrase
    val p = idealised.OpenMP.ProgramGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Dot product CPU 2 compiles to syntactically correct OpenMP") {
    import idealised.OpenMP.SurfaceLanguage.DSL._

    val dotCPU2 = nFun(n => fun(xsT(n))(in =>
      in :>>
        split(128) :>>
        mapPar(
          split(128) >>>
            mapSeq(
              reduceSeq(fun(x => fun(a => x + a)), 0.0f)
            )
        ) :>> join
    ))

    val phrase = TypeInference(dotCPU2, Map()).toPhrase
    val p = idealised.OpenMP.ProgramGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

}
