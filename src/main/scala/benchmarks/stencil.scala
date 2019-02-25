package benchmarks

import benchmarks.OpenCLBenchmark.DpiaProgram
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

    def printout():Unit =
      println(
        s"name = $name, " +
        s"inputSize = $inputSize, " +
          s"stencilSize = $stencilSize, " +
          s"localSize = $localSize, " +
          s"globalSize = $globalSize, " +
          s"runtime = $runtimeMs," +
          s" correct = $correctness"
      )
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

  private case class BenchmarkStencil(inputSize:Int, stencilSize:Int) extends OpenCLBenchmark(verbose = false) {
    override type Input = Array[Array[Float]]

    override type Result = StencilResult

    private val add = fun(x => fun(y => x + y))

    private val padSize = stencilSize/2

    private val stencil2Dbasic = {
      val N = NamedVar("N",StartFromRange(stencilSize))
      fun(ArrayType(N, ArrayType(N, float)))(input =>
        input :>>
          pad2D(N, padSize, padSize, FloatData(0.0f)) :>>
          slide2D(stencilSize, 1) :>>
          mapGlobal(0)(mapGlobal(1)(fun(nbh => join(nbh) :>> reduceSeq(add, 0.0f))))
      )
    }

    private val stencil2DPartitioned = {
      val N = NamedVar("N",StartFromRange(stencilSize*stencilSize*2))
      fun(ArrayType(N, ArrayType(N, float)))(input =>
        input :>>
          pad2D(N, padSize, padSize, FloatData(0.0f)) :>>
          slide2D(stencilSize, 1) :>>
          partition2D(padSize, N - stencilSize)
          :>> depMapSeqUnroll(fun(xs => xs :>> mapGlobal(0)(depMapSeqUnroll(mapGlobal(1)(fun(nbh => join(nbh) :>> reduceSeq(add, 0.0f)))))))
      )
    }

    override def dpiaPrograms: Seq[DpiaProgram] = Seq(
      DpiaProgram("Pad on the fly", stencil2Dbasic),
      DpiaProgram("Pad with partition", stencil2DPartitioned)
    )

    override def makeOutput(name: String, localSize: Int, globalSize: Int, code: String, runtimeMs: TimeSpan[ms], correctness: Correctness): StencilResult = {
      StencilResult(name, inputSize, stencilSize, localSize, globalSize, code, runtimeMs, correctness)
    }

    final override protected def makeInput(random: Random): Array[Array[Float]] = Array.fill(inputSize)(Array.fill(inputSize)(random.nextFloat()))

    final override protected def runScalaProgram(input: Array[Array[Float]]) = scalaProgram(input).flatten


    private def tileStencil(input:Array[Array[Float]]):Float = {
      input.flatten.reduceOption(_ + _).getOrElse(0.0f)
    }

    final def scalaProgram: Array[Array[Float]] => Array[Array[Float]] = (grid:Array[Array[Float]]) => {
      import idealised.utils.ScalaPatterns
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
    val results = (for {
      inputSize <- Seq(512, 1024, 2048, 4096)
      stencilSize <- Seq(4, 6, 8, 10)
      globalSize <- Seq(inputSize/2, inputSize)
      localSize <- Seq(16, 32, 64, 128, 256)
      if globalSize > localSize
    } yield {
      println(s"Benchmarking inputSize: $inputSize, stencilSize=$stencilSize, globalSize = $globalSize, localSize = $localSize")
      val results = BenchmarkStencil(inputSize, stencilSize).run(localSize, globalSize)
      results.foreach(_.printout())
      results
    }).flatten.groupBy(_.inputSize).mapValues(_.sortBy(_.name)).values.flatten.toSeq

    results.foreach(_.printout())
    val csv = StencilResult.toCsv(results)
    println(csv)
    import java.io.PrintWriter
    new PrintWriter("~/results.csv") { write(csv); close }
  }
}
