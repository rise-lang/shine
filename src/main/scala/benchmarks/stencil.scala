package benchmarks

import benchmarks.core.{Correctness, OpenCLBenchmark}
import benchmarks.core.OpenCLBenchmark.{DpiaProgram, Parameter}
import idealised.OpenCL.SurfaceLanguage.DSL.mapGlobal
import idealised.SurfaceLanguage.DSL.{DataExpr, fun, join, pad2D, reduceSeq, slide2D, _}
import idealised.SurfaceLanguage.Semantics.FloatData
import idealised.SurfaceLanguage.Types.{ArrayType, DataType, float}
import idealised.SurfaceLanguage.{->, Expr}
import idealised.utils.Time.ms
import idealised.utils.TimeSpan
import lift.arithmetic.{NamedVar, StartFromRange}

import scala.util.Random

object stencil {
  private case class StencilResult(name:String,
                                   inputSize:Int,
                                   stencilSize:Int,
                                   localSize:Int,
                                   globalSize:Int,
                                   code:String,
                                   runtimeMs:TimeSpan[ms],
                                   correctness:Correctness
                                  ) {

    def printoutText:String =
      s"name = $name, " +
      s"inputSize = $inputSize, " +
      s"stencilSize = $stencilSize, " +
      s"localSize = $localSize, " +
      s"globalSize = $globalSize, " +
      s"runtime = $runtimeMs," +
      s" correct = ${correctness.printoutText}"

    def printout():Unit =
      println(printoutText)
  }

  private object StencilResult {
    def toCsv(results:Seq[StencilResult]):String = {
      val header = "name;inputSize;stencilSize;localSize;globalSize;runtime;correct;"
      val lines = results.map(x =>
        Seq(x.name, x.inputSize, x.stencilSize, x.localSize, x.globalSize, x.runtimeMs, x.correctness.isCorrect)
        .map(_.toString).reduce(_ + ";" + _)
      )
      (Seq(header) ++ lines).reduce(_ ++ "\n" ++ _)
    }
  }

  private object BenchmarkStencil {
    val STENCIL_SIZE = "stencilSize"
  }

  private case class BenchmarkStencil() extends OpenCLBenchmark(verbose = false, runsPerProgram = 10) {
    import BenchmarkStencil._

    override type Input = Array[Array[Float]]

    override type Result = StencilResult

    private val add = fun(x => fun(y => x + y))

    //private val padSize = stencilSize/2

    private def stencil2Dbasic(paramMap:Map[String,Int]) = {
      val stencilSize = paramMap(STENCIL_SIZE)
      val padSize = stencilSize/2
      val N = NamedVar("N",StartFromRange(stencilSize))
      fun(ArrayType(N, ArrayType(N, float)))(input =>
        input :>>
          pad2D(N, padSize, padSize, FloatData(0.0f)) :>>
          slide2D(stencilSize, 1) :>>
          mapGlobal(0)(mapGlobal(1)(fun(nbh => join(nbh) :>> reduceSeq(add, 0.0f))))
      )
    }

    private def stencil2DPartitioned(paramMap:Map[String, Int]) = {
      val stencilSize = paramMap(STENCIL_SIZE)
      val padSize = stencilSize/2
      val N = NamedVar("N",StartFromRange(stencilSize*stencilSize*2))
      fun(ArrayType(N, ArrayType(N, float)))(input =>
        input :>>
          pad2D(N, padSize, padSize, FloatData(0.0f)) :>>
          slide2D(stencilSize, 1) :>>
          partition2D(padSize, N - stencilSize + 1)
          :>> depMapSeqUnroll(fun(xs => xs :>> mapGlobal(0)(depMapSeqUnroll(mapGlobal(1)(fun(nbh => join(nbh) :>> reduceSeq(add, 0.0f)))))))
      )
    }

    override def dpiaPrograms: Seq[DpiaProgram] = Seq(
      DpiaProgram("Pad on the fly", stencil2Dbasic),
      DpiaProgram("Pad with partition", stencil2DPartitioned)
    )

    override def makeOutput(name: String, paramMap:Map[String,Int], inputSize:Int, localSize: Int, globalSize: Int, code: String, runtimeMs: TimeSpan[ms], correctness: Correctness): StencilResult = {
      StencilResult(name, inputSize, paramMap(BenchmarkStencil.STENCIL_SIZE), localSize, globalSize, code, runtimeMs, correctness)
    }

    override def resultPrintout(result: StencilResult): Option[String] = Some(result.printoutText)

    final override protected def makeInput(inputSize:Int, random: Random): Array[Array[Float]] = Array.fill(inputSize)(Array.fill(inputSize)(random.nextFloat()))

    final override protected def runScalaProgram(input: Array[Array[Float]], paramMap:Map[String,Int]) = {
      scalaProgram(paramMap(STENCIL_SIZE))(input).flatten
    }

    private def tileStencil(input:Array[Array[Float]]):Float = {
      input.flatten.reduceOption(_ + _).getOrElse(0.0f)
    }

    final def scalaProgram(stencilSize:Int): Array[Array[Float]] => Array[Array[Float]] = (grid:Array[Array[Float]]) => {
      import idealised.utils.ScalaPatterns
      val padSize = stencilSize/2
      ScalaPatterns.slide2D(ScalaPatterns.pad2D(grid, padSize, 0.0f), stencilSize).map(_.map(tileStencil))
    }

    protected def tileStencil:Expr[DataType -> DataType] = {
      fun(xs => xs :>> join :>> reduceSeq(add, 0.0f))
    }
  }

  def main(args:Array[String]) = {
    explore()
  }

  private def explore():Unit = {
    val results = BenchmarkStencil().explore(
      Parameter("inputSize", Seq(2048, 4096, 8192, 16384)),
      Parameter("localSize", Seq(32, 64, 128, 256)),
      Set(Parameter(BenchmarkStencil.STENCIL_SIZE, Seq(2, 3, 4, 5, 6, 7, 8, 9, 10))),
      checkCorrectness = false
    )

    results.foreach(_.printout())
    val csv = StencilResult.toCsv(results)
    println(csv)
    import java.io.PrintWriter
    new PrintWriter("results.csv") { write(csv); close }
  }
}
