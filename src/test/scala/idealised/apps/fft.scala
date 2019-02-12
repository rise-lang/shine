package idealised.apps

import idealised.DPIA.Nat
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.Semantics._
import idealised.OpenMP.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.{->, Expr, IdentifierExpr, LiteralExpr}
import idealised.util.SyntaxChecker
import lift.arithmetic.{NamedVar, RangeAdd}
import opencl.executor.Execute
import org.scalatest._

class fft extends idealised.util.Tests {

  def createStockhamIterationLambda(p: Int, LPrevIter: Int, N: Int): Expr[DataType -> DataType] = {
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
          generate(p, dFun(_ => fun(IndexType(p))(k => {
            val exponentWoMinus2 = (toNatIdentifier(j) * m + toNatIdentifier(i)) * toNatIdentifier(k) / (n * m)
            val exponent = LiteralExpr(DoubleData(-2.0)) * cast(double, LiteralExpr(IndexData(exponentWoMinus2)))
            tuple(foreignFun(double, "callCos", (double, "x"), "{ return cospi(x); }", exponent),
              foreignFun(double, "callSin", (double, "x"), "{ return sinpi(x); }", exponent))
          })))))))))

    val reorderedBT = ArrayType(LPrevIter, ArrayType(p, ArrayType(p, TupleType(float, float))))

    val modPReorder = join() o transpose() o split(p)
    val createY = transpose() o modPReorder o split(r)

    fun(ArrayType(N, TupleType(float, float)))(x =>
      join() o transpose() o map(join() o transpose()) o
        split(LPrevIter) o
          join() o mapPar(mapSeq(fun(yChunkWithBrow => {

            val yChunk = yChunkWithBrow._1
            val Brow = yChunkWithBrow._2
            mapSeq(fun(Bchunk => reduceSeq(cmultandsum, tuple(0.0f, 0.0f)) $ zip(yChunk, Bchunk)

            )) $ Brow

    }))) o map(fun(yChunkRow => zip(yChunkRow, reorderedB))) o map(transpose() o split(LPrevIter)) o createY $ x)
  }

  val GOLD_STOCK_ITER_P2_LPREV4_N8: Array[Double] =
    Array((1.0, 0.0), (4.121320343559643, -2.121320343559643), (4.0, -5.0),
      (1.05025253169416733, -4.94974746830583267),
      (-1.0, 0.0), (-0.121320343559643, 2.121320343559543), (4.0, 5.0),
      (10.949747468305832671, 4.949747468305832671)).flatMap(t => List(t._1, t._2))

  val GOLD_FFT_8: Array[(Double, Double)] =
    Array((28.00000000000000, 28.00000000000000),
          (-13.65685424949238, 5.65685424949238),
          (-8.00000000000000, -0.00000000000000),
          (-5.65685424949238, -2.34314575050762),
          (-4.00000000000000, -4.00000000000000),
          (-2.34314575050762, -5.65685424949238),
          (0.00000000000000, -8.00000000000000),
          (5.65685424949238, -13.65685424949238))

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
            foreignFun(double, "callCos", (double, "x"), "{ return cos(x); }",
              cast(double, LiteralExpr(IndexData(toNatIdentifier(j)+n))) *
                cast(double, LiteralExpr(IndexData(toNatIdentifier(i)+m))))
            )))))))
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

    val fftiter = createStockhamIterationLambda(p, LPrevIter, N)
    val phrase = TypeInference(fftiter, Map()).convertToPhrase
    val kernel = idealised.OpenMP.ProgramGenerator.makeCode(phrase)
    println(kernel.code)

    SyntaxChecker(kernel.code)
  }

  /*
  test("single small FFT iteration computes correct result in OpenMP") {
    val p = 2
    val LPrevIter = 4
    val N = 8

    val stockhamIter = createStockhamIterationLambda(p, LPrevIter, N)

    val inputVec = Array.tabulate(N) {i => (1.0 * i, 0.0)}
    val (output, _) = Execute(32,32)[Array[Double]](stockhamIter, inputVec)

    output.size shouldBe GOLD_STOCK_ITER_P2_LPREV4_N8.size
    for (i <- 0 until output.size) output(i) should be (GOLD_STOCK_ITER_P2_LPREV4_N8(i) +- 1e-13)
  }
  */
}
