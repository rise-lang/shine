package apps

import lift.core._
import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import lift.OpenCL.primitives._
import idealised.utils._
import util.gen

class mriQ extends util.TestsWithExecutor {
  private val phiMag = foreignFun("phiMag", Seq("phiR", "phiI"),
    "{ return phiR * phiR + phiI * phiI; }",
    float ->: float ->: float)

  private implicit class MultiInput(o: Type) {
    def `x3 ->:`(i: Type): FunType[Type, Type] =
      i ->: i ->: i ->: o
  }

  private val qFun = foreignFun("computeQ",
    Seq("sX", "sY", "sZ", "Kx", "Ky", "Kz", "PhiMag", "acc"),
    """{
      |  #define PIx2 6.2831853071795864769252867665590058f
      |  float expArg = PIx2 * (Kx * sX + Ky * sY + Kz * sZ);
      |  acc._fst = acc._fst + PhiMag * cos(expArg);
      |  acc._snd = acc._snd + PhiMag * sin(expArg);
      |  return acc;
      |}""".stripMargin,
    float `x3 ->:` float `x3 ->:` float ->: (float x float) ->: (float x float))

  val computePhiMag: Expr = nFun(k => fun(
    (k`.`float) ->: (k`.`float) ->: (k`.`float)
  )((phiR, phiI) =>
    mapGlobal(fun(t => phiMag(t._1)(t._2)))(zip(phiR)(phiI))
  ))

  val computeQ: Expr = nFun(k => nFun(x => fun(
    (x`.`float) `x3 ->:` (x`.`float) ->: (x`.`float) ->: (k`.`(float x float x float x float)) ->: (x`.`(float x float))
  )((x, y, z, Qr, Qi, kvalues) =>
    zip(x)(zip(y)(zip(z)(zip(Qr)(Qi)))) |>
    mapGlobal(fun(t =>
      toPrivate(t._1) |> let(fun(sX =>
        toPrivate(t._2._1) |> let(fun(sY =>
          toPrivate(t._2._2._1) |> let(fun(sZ =>
            kvalues |> oclReduceSeq(AddressSpace.Private)(fun((acc, p) =>
              qFun(sX)(sY)(sZ)(p._1._1._1)(p._1._1._2)(p._1._2)(p._2)(acc)
            ))(pair(t._2._2._2._1, t._2._2._2._2))
          ))
        ))
      ))
    ))
  )))

  import idealised.OpenCL._
  private val K = 32
  private val X = 32

  def runOriginalComputePhiMag(name: String,
                               localSize: LocalSize,
                               globalSize: GlobalSize,
                               phiR: Array[Float],
                               phiI: Array[Float]): (Array[Float], TimeSpan[Time.ms]) = {
    import opencl.executor._

    val code = util.readFile(s"src/test/scala/apps/originalLift/$name")
    val kernelJNI = Kernel.create(code, "KERNEL", "")

    val float_bytes = 4
    val output_bytes = K * float_bytes
    val g_out = GlobalArg.createOutput(output_bytes)
    val kernelArgs = Array(
      GlobalArg.createInput(phiR),
      GlobalArg.createInput(phiI),
      g_out,
      ValueArg.create(K)
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

  def runOriginalComputeQ(name: String,
                          localSize: LocalSize,
                          globalSize: GlobalSize,
                          x: Array[Float],
                          y: Array[Float],
                          z: Array[Float],
                          Qr: Array[Float],
                          Qi: Array[Float],
                          kvalues: Array[Float]): (Array[Float], TimeSpan[Time.ms]) = {
    import opencl.executor._

    val code = util.readFile(s"src/test/scala/apps/originalLift/$name")
    val kernelJNI = Kernel.create(code, "KERNEL", "")

    val float_bytes = 4
    val output_bytes = 2 * X * float_bytes
    val g_out = GlobalArg.createOutput(output_bytes)
    val kernelArgs = Array(
      ValueArg.create(K), ValueArg.create(0: Int),
      GlobalArg.createInput(x),
      GlobalArg.createInput(y),
      GlobalArg.createInput(z),
      GlobalArg.createInput(Qr),
      GlobalArg.createInput(Qi),
      GlobalArg.createInput(kvalues),
      g_out,
      ValueArg.create(X)
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

  def runComputePhiMag(k: KernelNoSizes,
                       localSize: LocalSize,
                       globalSize: GlobalSize,
                       phiR: Array[Float],
                       phiI: Array[Float]): (Array[Float], TimeSpan[Time.ms]) = {
    val f = k.as[ScalaFunction `(`
      Int `,` Array[Float] `,` Array[Float]
      `)=>` Array[Float]]
    f(localSize, globalSize)(K `,` phiR `,` phiI)
  }

  def runComputeQ(k: KernelNoSizes,
                  localSize: LocalSize,
                  globalSize: GlobalSize,
                  x: Array[Float],
                  y: Array[Float],
                  z: Array[Float],
                  Qr: Array[Float],
                  Qi: Array[Float],
                  kvalues: Array[Float]): (Array[Float], TimeSpan[Time.ms]) = {
    val f = k.as[ScalaFunction `(`
      Int `,` Int `,`
      Array[Float] `,` Array[Float] `,` Array[Float] `,`
      Array[Float] `,` Array[Float] `,` Array[Float]
      `)=>` Array[Float]]
    f(localSize, globalSize)(K `,` X `,` x `,` y `,` z `,` Qr `,` Qi `,` kvalues)
  }

  test("computePhiMag versions produce same results") {
    val random = new scala.util.Random()
    val phiR = Array.fill(K)(random.nextFloat)
    val phiI = Array.fill(K)(random.nextFloat)

    val localSize = LocalSize(1)
    val globalSize = GlobalSize(K)

    util.runsWithSameResult(Seq(
      ("original", runOriginalComputePhiMag("CGO17_ComputePhiMag.cl", localSize, globalSize, phiR, phiI)),
      ("dpia", runComputePhiMag(gen.OpenCLKernel(computePhiMag), localSize, globalSize, phiR, phiI))
    ))
  }

  test("computeQ versions produce same results") {
    val random = new scala.util.Random()
    val x = Array.fill(X)(random.nextFloat)
    val y = Array.fill(X)(random.nextFloat)
    val z = Array.fill(X)(random.nextFloat)
    val Qr = Array.fill(X)(random.nextFloat)
    val Qi = Array.fill(X)(random.nextFloat)
    val kvalues = Array.fill(4 * K)(random.nextFloat)

    val localSize = LocalSize(1)
    val globalSize = GlobalSize(X)

    util.runsWithSameResult(Seq(
      ("original", runOriginalComputeQ("CGO17_ComputeQ.cl", localSize, globalSize, x, y, z, Qr, Qi, kvalues)),
      ("dpia", runComputeQ(gen.OpenCLKernel(computeQ), localSize, globalSize, x, y, z, Qr, Qi, kvalues))
    ))
  }
}
