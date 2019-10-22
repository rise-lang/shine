package apps

import lift.core._
import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import lift.OpenCL.DSL._

object mriQ {
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
  import util.{Time, TimeSpan}

  def runOriginalComputePhiMag(name: String,
                               phiR: Array[Float],
                               phiI: Array[Float]): (Array[Float], TimeSpan[Time.ms]) = {
    import opencl.executor._

    val code = util.readFile(s"src/main/scala/apps/originalLift/$name")
    val kernelJNI = Kernel.create(code, "KERNEL", "")

    val K = phiR.length
    val localSize = LocalSize(1)
    val globalSize = GlobalSize(K)

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

  def runComputePhiMag(k: KernelNoSizes,
                       phiR: Array[Float],
                       phiI: Array[Float]): (Array[Float], TimeSpan[Time.ms]) = {
    val K = phiR.length
    val localSize = LocalSize(1)
    val globalSize = GlobalSize(K)

    val f = k.as[ScalaFunction `(`
      Int `,` Array[Float] `,` Array[Float]
      `)=>` Array[Float]]
    f(localSize, globalSize)(K `,` phiR `,` phiI)
  }

  def runOriginalComputeQ(name: String,
                          x: Array[Float],
                          y: Array[Float],
                          z: Array[Float],
                          Qr: Array[Float],
                          Qi: Array[Float],
                          kvalues: Array[Float]): (Array[Float], TimeSpan[Time.ms]) = {
    import opencl.executor._

    val code = util.readFile(s"src/main/scala/apps/originalLift/$name")
    val kernelJNI = Kernel.create(code, "KERNEL", "")

    val X = x.length
    assert(kvalues.length % 4 == 0)
    val K = kvalues.length / 4
    val localSize = LocalSize(1)
    val globalSize = GlobalSize(X)

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

  def runComputeQ(k: KernelNoSizes,
                  x: Array[Float],
                  y: Array[Float],
                  z: Array[Float],
                  Qr: Array[Float],
                  Qi: Array[Float],
                  kvalues: Array[Float]): (Array[Float], TimeSpan[Time.ms]) = {
    val X = x.length
    assert(kvalues.length % 4 == 0)
    val K = kvalues.length / 4
    val localSize = LocalSize(1)
    val globalSize = GlobalSize(X)

    val f = k.as[ScalaFunction `(`
      Int `,` Int `,`
      Array[Float] `,` Array[Float] `,` Array[Float] `,`
      Array[Float] `,` Array[Float] `,` Array[Float]
      `)=>` Array[Float]]
    f(localSize, globalSize)(K `,` X `,` x `,` y `,` z `,` Qr `,` Qi `,` kvalues)
  }
}
