package apps

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import lift.OpenCL.primitives._
import idealised.util.gen
import idealised.utils.{Time, TimeSpan}

class kmeans extends idealised.util.TestsWithExecutor {
  val update = fun(float ->: (float x float) ->: float)((dist, pair) =>
    dist + (pair._1 - pair._2) * (pair._1 - pair._2)
  )

  val testF = foreignFun("test", Seq("dist", "tuple"),
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
    float ->: (float x (int x int)) ->: (float x (int x int)))

  val select = fun(tuple => tuple._2._2)

  val kmeans = nFun(p => nFun(c => nFun(f => fun(
    (f`.`p`.`float) ->: (c`.`f`.`float) ->: (p`.`int)
  )((features, clusters) =>
    features |> transpose |> mapGlobal(fun(feature =>
      clusters |> oclReduceSeq(AddressSpace.Private)(fun(tuple => fun(cluster => {
        val dist = zip(feature)(cluster) |> oclReduceSeq(AddressSpace.Private)(update)(l(0.0f))
        testF(dist)(tuple)
      })))(pair(cast(l(3.40282347e+38)) :: float)(pair(l(0))(l(0)))) |>
      select
    ))
  ))))

  import idealised.OpenCL._
  val P = 1024
  val C = 5
  val F = 34

  val localSize = LocalSize(128)
  val globalSize = GlobalSize(P)

  def runOriginalKernel(name: String,
                        features: Array[Array[Float]],
                        clusters: Array[Array[Float]]): (Array[Int], TimeSpan[Time.ms]) = {
    import opencl.executor._

    val source = io.Source.fromFile(s"src/test/scala/apps/originalLift/$name")
    val code = try source.getLines.mkString("\n") finally source.close
    val kernelJNI = Kernel.create(code, "KERNEL", "")

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

  def runKernel(k: KernelNoSizes,
                features: Array[Array[Float]],
                clusters: Array[Array[Float]]): (Array[Int], TimeSpan[Time.ms]) = {
    val f = k.as[ScalaFunction `(`
      Int `,` Int `,` Int `,` Array[Array[Float]] `,` Array[Array[Float]]
    `)=>` Array[Int]]
    f(localSize, globalSize)(P `,` C `,` F `,` features `,` clusters)
  }

  test("kmeans versions produce same results") {
    val random = new scala.util.Random()
    val features = Array.fill(F, P)(random.nextFloat)
    val clusters = Array.fill(C, F)(random.nextFloat)

    val runs = Seq(
      ("original", runOriginalKernel("KMeans.cl", features, clusters)),
      ("dpia", runKernel(gen.OpenCLKernel(kmeans), features, clusters))
    )

    def check(a: Array[Int], b: Array[Int]): Unit = {
      a.length == b.length
      a.zip(b).foreach { case (a, b) => assert(a == b) }
    }

    runs.tail.foreach(r => check(r._2._1, runs.head._2._1))
    runs.foreach(r => println(s"${r._1} time: ${r._2._2}"))
  }
}
