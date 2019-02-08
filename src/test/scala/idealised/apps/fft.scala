package idealised.apps

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.OpenMP.SurfaceLanguage.DSL._
import idealised.util.SyntaxChecker

class fft extends idealised.util.Tests {
  test("Correct FFT code can be generated.") {
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

    SyntaxChecker(
      //TODO remove from test
      //s"struct float_float { float _fst; float _snd; };" +
      kernel.code)
  }
}
