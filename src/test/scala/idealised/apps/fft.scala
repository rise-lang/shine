package idealised.apps

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.Semantics._
import idealised.OpenMP.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.LiteralExpr
import idealised.util.SyntaxChecker

class fft extends idealised.util.Tests {
  test("One-dimensional generator generates syntactically correct code in C.") {
    val N = 8

    val add = fun(x => x._1 + x._2)
    val simpleMap = fun(ArrayType(N, double))(in =>
      zip(in, generate(N, dFun(n => fun(IndexType(N))(i =>
        foreignFun(double, "callCos", (double, "x"), "{ return cos(x); }", cast(double, i))
      )))) :>> mapSeq(add)
    )

    val phrase = TypeInference(simpleMap, Map()).convertToPhrase
    val program = idealised.C.ProgramGenerator.makeCode(phrase)
    println(program.code)

    SyntaxChecker(program.code)
  }

  test("Two-dimensional generator generates syntactically correct code in C.") {
    val M = 8
    val N = 8

    val add = fun(x => x._1 + x._2)
    val simpleMap = fun(ArrayType(M, ArrayType(N, double)))(in =>
      zip(in,
        generate(M, dFun(m => fun(IndexType(M))(i =>
          generate(N, dFun(n => fun(IndexType(N))(j =>
            foreignFun(double, "callCos", (double, "x"), "{ return cos(x); }", cast(double, j) * cast(double, i)))))))))
        :>> mapSeq(fun(t => zip(t._1, t._2) :>> mapSeq(add)))
    )

    val phrase = TypeInference(simpleMap, Map()).convertToPhrase
    val program = idealised.C.ProgramGenerator.makeCode(phrase)
    println(program.code)

    SyntaxChecker(program.code)
  }

  test("Correct FFT iteration code can be generated in OpenMP.") {
    val N = 8
    val LPrevIter = 1
    val p = 2
    val r = N / (LPrevIter * p)

    val cmultandsum = fun(vt => fun(acc => {
      val lres = acc._1 + (vt._1._1 * vt._2._1 - vt._1._2 * vt._2._2)
      val rres = acc._2 + (vt._1._2 * vt._2._1 + vt._1._1 * vt._2._2)
      tuple(lres, rres)
    }))

    //val reorderedB = Array3DFromUserFunGenerator(
    //  genReorderedTwiddleWithDFTUserFun(complexConjugate = inverse), TypeOfB)
    /*
    def genReorderedTwiddleWithDFTUserFun(complexConjugate: Boolean = false): UserFun = {
      val signString = if (complexConjugate) "" else "-"
      UserFun("genTwiddleWithDFT",
        Array("j", "k", "l", "LPrevIter", "pheight", "pwidth"),
        "{ Tuple2_double_double twiddleWithDFT;\n" +
          "\tdouble exponent = " + signString + "2.0 * (k * LPrevIter + j) * l / (pheight * LPrevIter);\n" +
          "\ttwiddleWithDFT._0 = cospi(exponent);\n" +
          "\ttwiddleWithDFT._1 = sinpi(exponent);\n" +
          "\treturn twiddleWithDFT;}",
        Seq(Int, Int, Int, Int, Int, Int),
        TupleType(Double, Double)
      )
    }*/
    val reorderedB =
    generate(LPrevIter, dFun(m => fun(IndexType(LPrevIter))(i =>
      generate(p, dFun(n => fun(IndexType(p))(j =>
        generate(p, dFun(o => fun(IndexType(p))(k => {
          val exponent =
            LiteralExpr(DoubleData(-2.0)) *
              (cast(double, j) * m + cast(double, i)) * cast(double, k) / (n * m)
        })))))))))

    val reorderedBT = ArrayType(LPrevIter, ArrayType(p, ArrayType(p, TupleType(float, float))))

    val modPReorder = join() o transpose() o split(p)
    val createY = transpose() o modPReorder o split(r)

    val fftiter =
      fun(reorderedBT)(reorderedB =>
        fun(ArrayType(N, TupleType(float, float)))(x =>
          join() o transpose() o map(join() o transpose()) o
            split(LPrevIter) o
              join() o mapPar(mapSeq(fun(yChunkWithBrow => {

                val yChunk = yChunkWithBrow._1
                val Brow = yChunkWithBrow._2
                mapSeq(fun(Bchunk => reduceSeq(cmultandsum, tuple(0.0f, 0.0f)) $ zip(yChunk, Bchunk)

                )) $ Brow

        }))) o map(fun(yChunkRow => zip(yChunkRow, reorderedB))) o map(transpose() o split(LPrevIter)) o createY $ x)
    )

    val phrase = TypeInference(fftiter, Map()).convertToPhrase
    val kernel = idealised.OpenMP.ProgramGenerator.makeCode(phrase)
    println(kernel.code)

    SyntaxChecker(kernel.code)
  }
}
