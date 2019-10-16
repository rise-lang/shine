package apps

import mm._
import lift.core._
import idealised.OpenCL._
import util._

class mmCheck extends test_util.TestsWithExecutor {
  private val N = 64
  private val M = 128
  private val O = 128

  private def randGold(): (Array[Array[Float]], Array[Array[Float]], Array[Float]) = {
    val rand = new scala.util.Random
    val At = Array.fill(O, N)(rand.nextFloat * 10)
    val B = Array.fill(O, M)(rand.nextFloat * 10)
    val gold = computeGold(N, M, O, At, B).flatten
    (At, B, gold)
  }

  private def runOriginal(name: String,
                          localSize: LocalSize,
                          globalSize: GlobalSize,
                          At: Array[Array[Float]],
                          B: Array[Array[Float]]): Array[Float] = {
    import opencl.executor._

    val code = util.readFile(s"src/main/scala/apps/originalLift/$name")
    val kernelJNI = Kernel.create(code, "KERNEL", "")

    val float_bytes = 4
    val output_bytes = N * M * float_bytes
    val g_out = GlobalArg.createOutput(output_bytes)
    val kernelArgs = Array(
      GlobalArg.createInput(At.flatten),
      GlobalArg.createInput(B.flatten),
      g_out,
      ValueArg.create(O), ValueArg.create(N), ValueArg.create(M)
    )

    val runtime = Executor.execute(kernelJNI,
      localSize.size.x.eval, localSize.size.y.eval, localSize.size.z.eval,
      globalSize.size.x.eval, globalSize.size.y.eval, globalSize.size.z.eval,
      kernelArgs
    )

    val output = g_out.asFloatArray()

    kernelArgs.foreach(_.dispose)
    kernelJNI.dispose()

    println(s"${name} time: ${TimeSpan.inMilliseconds(runtime)}")
    output
  }

  private def runExpr(localSize: LocalSize,
                      globalSize: GlobalSize,
                      At: Array[Array[Float]],
                      B: Array[Array[Float]],
                      e: Expr): Array[Float] = {
    val kernel = gen.OpenCLKernel(e)
    val run = kernel.as[ScalaFunction `(`
      Int `,` Int `,` Int `,` Array[Array[Float]] `,` Array[Array[Float]]
      `)=>` Array[Float]]
    val (output, time) = run(localSize, globalSize)(N `,` M `,` O `,` At `,` B)
    println(s"time: $time")
    output
  }

  test("sequential matrix multiplication produces expected result") {
    val (at, b, gold) = randGold()
    val output = runExpr(LocalSize(1), GlobalSize(1), at, b, sequential)
    util.assertSame(output, gold, "output is different from gold")
  }

  test("amd matrix multiplication produces expected result") {
    val (at, b, gold) = randGold()
    val original = runOriginal("CGO17_MMAMD.cl",
      LocalSize((32, 8)), GlobalSize((M/4, N/8)), at, b)
    val output = runExpr(LocalSize((32, 8)), GlobalSize((M/4, N/8)), at, b, amd)
    util.assertSame(original, gold, "original is different from gold")
    util.assertSame(output, gold, "output is different from gold")
  }

  test("nvidia matrix multplication produces expected result") {
    val (at, b, gold) = randGold()
    val original = runOriginal("CGO17_MMNVIDIA.cl",
      LocalSize((32, 8)), GlobalSize((M/4, N/8)), at, b)
    val output = runExpr(LocalSize((32, 8)), GlobalSize((M/4, N/8)), at, b, nvidia)
    util.assertSame(original, gold, "original is different from gold")
    util.assertSame(output, gold, "output is different from gold")
  }
}
