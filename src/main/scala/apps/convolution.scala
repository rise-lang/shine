package apps

import rise.core._
import rise.core.DSL._
import rise.core.types._
import rise.core.TypeLevelDSL._
import rise.core.primitives._
import rise.OpenCL.DSL._
import rise.core.HighLevelConstructs._
import util.{Time, TimeSpan}

object convolution {
  private val id = fun(x => x)

  private val dotElemWeights = fun((weights, elem) =>
    oclReduceSeqUnroll(AddressSpace.Private)(fun((acc, pair) => {
      val pixel = pair._1
      val weight = pair._2
      acc + (pixel * weight)
    }))(l(0.0f))(zip(join(elem))(weights))
  )

  val blurXTiled2D: Expr = nFun(n => fun(
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

  val blurYTiled2DTiledLoadingTransposed: Expr = nFun(n =>  fun(
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

  def blurXTiled2D_sizes(n: Int): (LocalSize, GlobalSize) = {
    assert(n % 8 == 0)
    (LocalSize((16, 4)), GlobalSize((n / 8, n)))
  }

  def blurYTiled2DTiledLoadingTransposed_sizes(n: Int): (LocalSize, GlobalSize) = {
    assert(n % 8 == 0)
    (LocalSize((16, 8)), GlobalSize((n, n / 8)))
  }

  def runOriginalKernel(name: String,
                        n: Int,
                        localSize: LocalSize,
                        globalSize: GlobalSize,
                        matrix: Array[Array[Float]],
                        weights: Array[Float]): (Array[Float], TimeSpan[Time.ms]) = {
    import opencl.executor._

    val code = util.readFile(s"src/main/scala/apps/originalLift/$name")
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
}
