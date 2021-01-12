package apps

import acoustic3D._
import shine.OpenCL._
import util.{Time, TimeSpan, gen}

class Acoustic3D extends test_util.TestsWithExecutor {
  private val N = 128
  private val M = 64
  private val O = 32

  private val localSize = LocalSize((32, 4))
  private val globalSize = GlobalSize((N, M))

  def runOriginalKernel(name: String,
                        mat1: Array[Array[Array[Float]]],
                        mat2: Array[Array[Array[Float]]]): (Array[Float], TimeSpan[Time.ms]) = {
    import opencl.executor._

    val code = util.readFile(s"src/main/scala/apps/originalLift/$name")
    val kernelJNI = Kernel.create(code, "KERNEL", "")

    val float_bytes = 4
    val output_bytes = O * N * M * float_bytes
    val g_out = GlobalArg.createOutput(output_bytes)
    val kernelArgs = Array(
      GlobalArg.createInput(mat1.flatten.flatten),
      GlobalArg.createInput(mat2.flatten.flatten),
      g_out,
      ValueArg.create(M), ValueArg.create(N), ValueArg.create(O)
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

  def runKernel(k: KernelExecutor.KernelNoSizes,
                mat1: Array[Array[Array[Float]]],
                mat2: Array[Array[Array[Float]]]): (Array[Float], TimeSpan[Time.ms]) = {
    val f = k.as[ScalaFunction `(`
      Int `,` Int `,` Int `,`
      Array[Array[Array[Float]]] `,`
      Array[Array[Array[Float]]]
      `)=>` Array[Float]]
    f(localSize, globalSize)(O `,` N `,` M `,` mat1 `,` mat2)
  }

  test("acoustic stencils produce same results") {
    val random = new scala.util.Random()
    val mat1 = Array.fill(O + 2, N + 2, M + 2)(random.nextFloat() * random.nextInt(1000))
    val mat2 = Array.fill(O + 2, N + 2, M + 2)(random.nextFloat() * random.nextInt(1000))

    test_util.runsWithSameResult(Seq(
      ("original", runOriginalKernel("acoustic3D.cl", mat1, mat2)),
      ("originalMSS", runOriginalKernel("acoustic3DMSS.cl", mat1, mat2)),
      ("dpia", runKernel(gen.opencl.kernel.fromExpr("stencil")(stencil), mat1, mat2)),
      ("dpiaMSS", runKernel(gen.opencl.kernel.fromExpr("stencilMSS")(stencilMSS), mat1, mat2))
    ))
  }
}
