package apps

import rise.core.DSL._
import rise.core.DSL.Type._
import HighLevelConstructs._
import rise.core._
import rise.core.primitives._
import rise.core.types._
import rise.core.types.DataType._
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeqUnroll
import util.{Time, TimeSpan}
import shine.OpenCL.KernelExecutor._

object convolution {
  private val id = fun(x => x)

  private val dotElemWeights = fun((weights, elem) =>
    zip(join(elem))(weights) |>
    map(separableConvolution2D.mulT) |>
    reduce(add)(lf32(0.0f))
  )

  private val dotElemWeightsSeq = fun((weights, elem) =>
    oclReduceSeqUnroll(AddressSpace.Private)(fun((acc, pair) => {
      val pixel = pair.`1`
      val weight = pair.`2`
      acc + (pixel * weight)
    }))(lf32(0.0f))(zip(join(elem))(weights)))

  // FIXME: could not find original Lift expression, this is made up
  val blurXHighLevel: ToBeTyped[Expr] = depFun((n: Nat) => fun(
    (n `.` n `.` f32) ->: (17 `.` f32) ->: (n `.` n `.` f32)
  )((matrix, weights) =>
    matrix |> padClamp2D(0, 0, 8, 8) |> slide2D(1, 1, 17, 1) |>
    map(map(dotElemWeights(weights)))
  ))

  // FIXME: could not find original Lift expression, this is made up
  val blurYHighLevel: ToBeTyped[Expr] = depFun((n: Nat) => fun(
    (n `.` n `.` f32) ->: (17 `.` f32) ->: (n `.` n `.` f32)
  )((matrix, weights) =>
    matrix |> padClamp2D(8, 8, 0, 0) |> slide2D(17, 1, 1, 1) |>
    map(map(transpose >> dotElemWeights(weights)))
  ))

  val blurXTiled2D: ToBeTyped[Expr] = depFun((n: Nat) => fun(
    (n `.` n `.` f32) ->: (17 `.` f32) ->: (n `.` n `.` f32)
  )((matrix, weights) =>
    unslide2D o mapWorkGroup(1)(mapWorkGroup(0)(fun(tile =>
      mapLocal(1)(mapLocal(0)(dotElemWeightsSeq(weights)))
        o slide2D(1, 1, 17, 1)
        $ toLocal(mapLocal(1)(mapLocal(0)(id))(tile)))
    )) o slide2D(4, 4, 144, 128)
    o padClamp2D(0, 0, 8, 8) $ matrix
  ))

  val blurYTiled2DTiledLoadingTransposed: ToBeTyped[Expr] =
    depFun((n: Nat) => fun(
      (n `.` n `.` f32) ->: (17 `.` f32) ->: (n `.` n `.` f32)
    )((matrix, weights) =>
    unslide2D o mapWorkGroup(1)(mapWorkGroup(0)(fun(tile =>
      mapLocal(1)(mapLocal(0)(dotElemWeightsSeq(weights)))
        o slide2D(17, 1, 1, 1)
        o transpose o map(dropLast(1)) $ toLocal(
          transpose(tile)
          |> map(split(8))
          |> mapLocal(0)(mapSeqUnroll(mapLocal(1)(id)))
          |> map(join >> padEmpty(1))
        )
    ))) o slide2D(80, 64, 16, 16)
      o padClamp2D(8, 8, 0, 0) $ matrix
  ))

  import shine.OpenCL._

  object hosted {
    val blurXTiled2D: ToBeTyped[Expr] = depFun((n: Nat) => fun(
      (n`.`n`.`f32) ->: (17`.`f32) ->: (n`.`n`.`f32)
    )((matrix, weights) =>
      oclRun(LocalSize(16, 4), GlobalSize(n / 8, n))(convolution.blurXTiled2D(n)(matrix)(weights))
    ))

    val blurYTiled2DTiledLoadingTransposed: ToBeTyped[Expr] = depFun((n: Nat) => fun(
      (n`.`n`.`f32) ->: (17`.`f32) ->: (n`.`n`.`f32)
    )((matrix, weights) =>
      oclRun(LocalSize(16, 8), GlobalSize(n, n / 8))(
        convolution.blurYTiled2DTiledLoadingTransposed(n)(matrix)(weights))
    ))
  }

  def blurXTiled2DSizes(n: Int): (LocalSize, GlobalSize) = {
    assert(n % 8 == 0)
    (LocalSize((16, 4)), GlobalSize((n / 8, n)))
  }

  def blurYTiled2DTiledLoadingTransposedSizes(n: Int)
  : (LocalSize, GlobalSize) = {
    assert(n % 8 == 0)
    (LocalSize((16, 8)), GlobalSize((n, n / 8)))
  }

  def runOriginalKernel(
    name: String,
    n: Int,
    localSize: LocalSize,
    globalSize: GlobalSize,
    matrix: Array[Array[Float]],
    weights: Array[Float]
  ): (Array[Float], TimeSpan[Time.ms]) = {
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

  def runKernel(
    k: KernelNoSizes,
    localSize: LocalSize,
    globalSize: GlobalSize,
    matrix: Array[Array[Float]],
    weights: Array[Float]
  ): (Array[Float], TimeSpan[Time.ms]) = {
    val f = k.as[In `=` Array[Array[Float]] `,` Array[Float], Out[Array[Float]]]
    f(localSize, globalSize)(matrix `,` weights)
  }
}
