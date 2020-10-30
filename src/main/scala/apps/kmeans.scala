package apps

import rise.core._
import rise.core.TypedDSL._
import rise.core.primitives._
import rise.core.TypeLevelDSL._
import rise.core.types._
import rise.openCL.TypedDSL._
import rise.openCL.primitives.oclReduceSeq

object kmeans {
  private val update = fun(f32 ->: (f32 x f32) ->: f32)((dist, pair) =>
    dist + (pair._1 - pair._2) * (pair._1 - pair._2)
  )

  private val testF = foreignFun("test",
    Seq("dist", "tuple"),
    """{
      | float min_dist = tuple._fst;
      | int i = tuple._snd._fst;
      | int index = tuple._snd._snd;
      | if (dist < min_dist) {
      |   return (struct Record_float__int_int_){ dist, { i + 1 , i } };
      | } else {
      |   return (struct Record_float__int_int_){ min_dist, { i + 1, index } };
      | }
      }""".stripMargin,
    f32 ->: (f32 x (int x int)) ->: (f32 x (int x int)))

  private val select = fun(tuple => tuple._2._2)

  val kmeans: Expr = depFun((p: Nat) => depFun((c: Nat) => depFun((f: Nat) => fun(
    (f `.` p `.` f32) ->: (c `.` f `.` f32) ->: (p `.` int)
  )((features, clusters) =>
    features |> transpose |> mapGlobal(fun(feature =>
      clusters |> oclReduceSeq(AddressSpace.Private)(
        fun(tuple => fun(cluster => {
          val dist = zip(feature)(cluster) |>
            oclReduceSeq(AddressSpace.Private)(update)(l(0.0f))
          testF(dist)(tuple)
        }))
      )(
        pair(cast(l(3.40282347e+38)) :: f32)(pair(l(0))(l(0)))
      ) |> select
    ))
  ))))

  import shine.OpenCL._
  import util.{Time, TimeSpan}

  def runOriginalKernel(
    name: String,
    features: Array[Array[Float]],
    clusters: Array[Array[Float]]
  ): (Array[Int], TimeSpan[Time.ms]) = {
    import opencl.executor._

    val code = util.readFile(s"src/main/scala/apps/originalLift/$name")
    val kernelJNI = Kernel.create(code, "KERNEL", "")

    val C = clusters.length
    val F = features.length
    val P = features(0).length
    val localSize = LocalSize(128)
    val globalSize = GlobalSize(P)

    val int_bytes = 4
    val output_bytes = P * int_bytes
    val g_out = GlobalArg.createOutput(output_bytes)
    val kernelArgs = Array(
      GlobalArg.createInput(features.flatten),
      GlobalArg.createInput(clusters.flatten),
      g_out,
      ValueArg.create(C), ValueArg.create(F), ValueArg.create(P)
    )

    val runtime = Executor.execute(kernelJNI,
      localSize.size.x.eval, localSize.size.y.eval, localSize.size.z.eval,
      globalSize.size.x.eval, globalSize.size.y.eval, globalSize.size.z.eval,
      kernelArgs
    )

    val output = g_out.asIntArray()

    kernelArgs.foreach(_.dispose)
    kernelJNI.dispose()

    (output, TimeSpan.inMilliseconds(runtime))
  }

  def runKernel(
    k: KernelNoSizes,
    features: Array[Array[Float]],
    clusters: Array[Array[Float]]
  ): (Array[Int], TimeSpan[Time.ms]) = {
    val C = clusters.length
    val F = features.length
    val P = features(0).length
    val localSize = LocalSize(128)
    val globalSize = GlobalSize(P)

    val f = k.as[ScalaFunction `(`
      Int `,` Int `,` Int `,` Array[Array[Float]] `,` Array[Array[Float]]
      `)=>` Array[Int]]
    f(localSize, globalSize)(P `,` C `,` F `,` features `,` clusters)
  }
}
