package apps

import lift.core._
import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import lift.OpenCL.primitives._
import lift.core.HighLevelConstructs._
import util.gen
import idealised.utils.{Time, TimeSpan}

class convolution extends util.TestsWithExecutor {
  val id = fun(x => x)

  val dotElemWeights = fun((weights, elem) =>
    oclReduceSeqUnroll(AddressSpace.Private)(fun((acc, pair) => {
      val pixel = pair._1
      val weight = pair._2
      acc + (pixel * weight)
    }))(l(0.0f))(zip(join(elem))(weights))
  )

  val blurXTiled2D = nFun(n => fun(
    (n`.`n`.`float) ->: (17`.`float) ->: (n`.`n`.`float)
  )((matrix, weights) =>
    unslide2D o mapWorkGroup(1)(mapWorkGroup(0)(fun(tile =>
      mapLocal(1)(mapLocal(0)(dotElemWeights(weights)))
        o slide2D(1, 1, 17, 1)
        $ toLocal(mapLocal(1)(mapLocal(0)(id))(tile))
    ))) o slide2D(4, 4, 144, 128)
      o padClamp2D(0, 0, 8, 8) $ matrix
  ))

  def padEmpty(l: Nat, r: Nat): Expr = padClamp(l)(r)
  def unpadEmpty(l: Nat, r: Nat): Expr = implN(n => implDT(t =>
    drop(l) >> (take(n) :: ((n + r)`.`t) ->: (n`.`t))
  ))

  val blurYTiled2DTiledLoadingTransposed = nFun(n =>  fun(
    (n`.`n`.`float) ->: (17`.`float) ->: (n`.`n`.`float)
  )((matrix, weights) =>
    unslide2D o mapWorkGroup(1)(mapWorkGroup(0)(fun(tile =>
      mapLocal(1)(mapLocal(0)(dotElemWeights(weights)))
        // o unpadEmpty(0, 1)
        o slide2D(17, 1, 1, 1)
        o transpose o map(join) $ toLocal(transpose(tile)
          // |> map(padEmpty(0, 1))
          |> map(split(8))
          |> mapLocal(0)(mapSeqUnroll(mapLocal(1)(id)))
        )
    ))) o slide2D(80, 64, 16, 16)
      o padClamp2D(8, 8, 0, 0) $ matrix
  ))

  import idealised.OpenCL._
  val inputSize_small = 4096
  val inputSize_large = 8192

  def runOriginalKernel(name: String,
                        n: Int,
                        localSize: LocalSize,
                        globalSize: GlobalSize,
                        matrix: Array[Array[Float]],
                        weights: Array[Float]): (Array[Float], TimeSpan[Time.ms]) = {
    import opencl.executor._

    val code = util.readFile(s"src/test/scala/apps/originalLift/$name")
    val kernelJNI = Kernel.create(code, "KERNEL", "")

    val float_bytes = 4
    val output_bytes = n * n * float_bytes
    val g_out = GlobalArg.createOutput(output_bytes)
    val kernelArgs = Array[KernelArg](
      GlobalArg.createInput(matrix.flatten),
      GlobalArg.createInput(weights),
      g_out
    )

    val runtime = Executor.execute(kernelJNI,
      localSize.size.x.eval, localSize.size.y.eval, localSize.size.z.eval,
      globalSize.size.x.eval, globalSize.size.y.eval, globalSize.size.z.eval,
      kernelArgs
    )

    val output = g_out.asFloatArray()

    kernelArgs.foreach(_.dispose)
    kernelJNI.dispose()

    (output, TimeSpan.inMilliseconds(runtime))
  }

  def runKernel(k: KernelNoSizes,
                localSize: LocalSize,
                globalSize: GlobalSize,
                matrix: Array[Array[Float]],
                weights: Array[Float]): (Array[Float], TimeSpan[Time.ms]) = {
    val f = k.as[ScalaFunction `(`
      Array[Array[Float]] `,` Array[Float]
    `)=>` Array[Float]]
    f(localSize, globalSize)(matrix `,` weights)
  }

  test("convolution versions produce same results") {
    val N = inputSize_small // TODO: this is still big for a test
    val random = new scala.util.Random()
    val matrix = Array.fill(N, N)(random.nextFloat * 10.0f)
    val weights = Array.fill(17)(random.nextFloat)

    assert(N % 8 == 0)
    val lsX = LocalSize((16, 4))
    val gsX = GlobalSize((N / 8, N))
    val lsY = LocalSize((16, 8))
    val gsY = GlobalSize((N, N / 8))
    util.runsWithSameResult(Seq(
      ("originalX (CG017)", runOriginalKernel("CGO17_ConvolutionColumn_small.cl",
        N, lsX, gsX, matrix, weights)),
      ("dpiaX", runKernel(gen.OpenCLKernel(blurXTiled2D(N)),
        lsX, gsX, matrix, weights))
    ))
    util.runsWithSameResult(Seq(
      ("originalY (CG017)", runOriginalKernel("CGO17_ConvolutionRow_small.cl",
        N, lsY, gsY, matrix, weights)),
      ("dpiaY", runKernel(gen.OpenCLKernel(blurYTiled2DTiledLoadingTransposed(N)),
        lsY, gsY, matrix, weights))
    ))
  }
}
