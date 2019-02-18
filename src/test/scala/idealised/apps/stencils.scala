package idealised.apps

import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.DSL.{fun, _}
import idealised.SurfaceLanguage.Types._
import idealised.util.TestsWithExecutor
import lift.arithmetic.{ArithExpr, SizeVar}

import scala.util.Random

class stencils extends TestsWithExecutor {
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

  private def basicStencil(inputSize:Int, stencilSize:Int, tileSize:Int)(localSize:Int, globalSize:Int):Stencil1DResult = {
    val padSize = stencilSize/2

    val N = SizeVar("N")
    val f =fun(ArrayType(N, float))(input =>
      input :>> pad(padSize, padSize, 0.0f) :>> slide(stencilSize, 1) :>> mapGlobal(
        fun(nbh => reduceSeq(add, 0.0f, nbh)
        ))
    )

    val scalaFun = (xs:Array[Float]) => {
      val pad = Array.fill(padSize)(0.0f)
      val paddedXs = pad ++ xs ++ pad
      paddedXs.sliding(stencilSize, 1).map(nbh => nbh.foldLeft(0.0f)(_ + _))
    }
    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(f, Map()).toPhrase, localSize, globalSize)

    val rand = new Random()
    val input = Array.tabulate(inputSize)(_ => rand.nextFloat())
    val scalaOutput = scalaFun(input).toArray

    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}

    val kernelFun = kernel.as[ScalaFunction`(`Array[Float]`)=>`Array[Float]]
    val (kernelOutput, time) = kernelFun(input `;`)

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

  private def partitionedStencil(inputSize:Int, stencilSize:Int, tileSize:Int)(localSize:Int, globalSize:Int) = {
    val padSize = stencilSize/2

    val N = SizeVar("N")
    val f =fun(ArrayType(N, float))(input =>
      input :>> pad(padSize, padSize, 0.0f) :>>
        partition(3, m => ArithExpr.Math.PointwiseStepped(padSize, N, padSize)(m)) :>>
        depMapSeq(fun(chunk => slide(stencilSize, 1, chunk) :>> mapGlobal(fun(nbh => reduceSeq(add, 0.0f, nbh)))))
    )

    val scalaFun = (xs:Array[Float]) => {
      val pad = Array.fill(padSize)(0.0f)
      val paddedXs = Array(pad,xs,pad)
      paddedXs.flatMap(_.sliding(stencilSize, 1).map(nbh => nbh.foldLeft(0.0f)(_ + _)))
    }
    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(f, Map()).toPhrase, localSize, globalSize)

    val rand = new Random()
    val input = Array.tabulate(inputSize)(_ => rand.nextFloat())
    val scalaOutput = scalaFun(input).toArray

    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}

    val kernelFun = kernel.as[ScalaFunction`(`Array[Float]`)=>`Array[Float]]
    val (kernelOutput, time) = kernelFun(input `;`)

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

  test("Basic stencil") {
    basicStencil(1024, 5, 128)(1,1).printout()
  }

  test("Partitioned stencil") {
    partitionedStencil(1024, 5, 128)(1,1).printout()
  }
}
