package idealised.apps

import idealised.OpenCL.Kernel
import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.DSL.{fun, _}
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{->, Expr}
import idealised.util.Tests
import lift.arithmetic.{ArithExpr, SizeVar, SteppedCase}

import scala.util.Random

class stencils extends Tests {
  val add = fun(x => fun(y => x + y))

  private case class Stencil1DResult(inputSize:Int,
                                     stencilSize:Int,
                                     tileSize:Int,
                                     localSize:Int,
                                     globalSize:Int,
                                     code:String,
                                     runtimeMs:Double,
                                     correct:Boolean
                                    ) {
    def printout():Unit =
      println(
        s"inputSize = $inputSize, " +
        s"stencilSize = $stencilSize, " +
        s"tileSize =$tileSize, " +
        s"localSize = $localSize, " +
        s"globalSize = $globalSize, " +
        s"code = $code, " +
        s"runtime = $runtimeMs," +
        s" correct = $correct"
      )
  }

  private trait StencilAlgorithm {
    def dpiaProgram:Expr[DataType -> DataType]
    def scalaProgram:Array[Float] => Array[Float]

    def inputSize:Int
    def stencilSize:Int
    def tileSize:Int


    private def compile(localSize:ArithExpr, globalSize:ArithExpr):Kernel = {
      idealised.OpenCL.KernelGenerator.makeCode(TypeInference(this.dpiaProgram, Map()).toPhrase, localSize, globalSize)
    }

    final def compileAndPrintCode(localSize:ArithExpr, globalSize:ArithExpr):Unit = {
      val kernel = this.compile(localSize, globalSize)
      println(kernel.code)
    }

    final def run(localSize:Int, globalSize:Int):Stencil1DResult = {
      opencl.executor.Executor.loadAndInit()

      val rand = new Random()
      val input = Array.tabulate(inputSize)(_ => rand.nextFloat())
      val scalaOutput = scalaProgram(input).toArray

      import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}

      val kernel = this.compile(localSize, globalSize)
      val kernelFun = kernel.as[ScalaFunction`(`Array[Float]`)=>`Array[Float]]
      val (kernelOutput, time) = kernelFun(input `;`)

      opencl.executor.Executor.shutdown()

      val correct = kernelOutput.zip(scalaOutput).forall{case (x,y) => Math.abs(x - y) < 0.01}
      Stencil1DResult(
        inputSize = inputSize,
        stencilSize = stencilSize,
        tileSize = tileSize,
        localSize = localSize,
        globalSize = globalSize,
        code = kernel.code,
        runtimeMs = time.value,
        correct = correct
      )
    }
  }

  private case class BasicStencil(inputSize:Int, stencilSize:Int, tileSize:Int) extends StencilAlgorithm {
    private val padSize = stencilSize/2

    override def dpiaProgram: Expr[DataType -> DataType] = {
      val N = SizeVar("N")
      fun(ArrayType(N, float))(input =>
        input :>> pad(padSize, padSize, 0.0f) :>> slide(stencilSize, 1) :>> mapGlobal(
          fun(nbh => reduceSeq(add, 0.0f, nbh)
          ))
      )
    }

    override def scalaProgram: Array[Float] => Array[Float] = (xs:Array[Float]) => {
      val pad = Array.fill(padSize)(0.0f)
      val paddedXs = pad ++ xs ++ pad
      paddedXs.sliding(stencilSize, 1).map(nbh => nbh.foldLeft(0.0f)(_ + _))
    }.toArray
  }

  private case class PartitionedStencil(inputSize:Int, stencilSize:Int, tileSize:Int) extends StencilAlgorithm {
    private val padSize = stencilSize/2

    override def dpiaProgram: Expr[DataType -> DataType] = {
      val N = SizeVar("N")
      fun(ArrayType(N, float))(input =>
        input :>> pad(padSize, padSize, 0.0f) :>>
          slide(stencilSize, 1) :>>
          partition(3, m => SteppedCase(m, Seq(padSize, N, padSize))) :>>
          depMapSeqUnroll(mapGlobal(fun(nbh => reduceSeq(add, 0.0f, nbh))))
      )
    }

    override def scalaProgram: Array[Float] => Array[Float] = (xs:Array[Float]) => {
      val pad = Array.fill(padSize)(0.0f)
      val paddedXs = pad ++ xs ++ pad
      paddedXs.sliding(stencilSize, 1).map(nbh => nbh.foldLeft(0.0f)(_ + _))
    }.toArray
  }


  test("Basic stencil") {
    assert(BasicStencil(1024, 5, 128).run(localSize = 1, globalSize = 1).correct)
  }

  test("Partitioned stencil") {
    assert(PartitionedStencil(1024, 5, 128).run(localSize = 1, globalSize = 1).correct)
  }
}
