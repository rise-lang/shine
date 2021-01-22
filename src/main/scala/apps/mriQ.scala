package apps

import rise.core._
import rise.core.DSL._
import rise.core.primitives.{let => _, _}
import rise.core.DSL.Type._
import rise.core.types._
import rise.openCL.TypedDSL._
import rise.openCL.primitives.oclReduceSeq

object mriQ {
  private val phiMag = foreignFun("phiMag",
    Seq("phiR", "phiI"),
    "{ return phiR * phiR + phiI * phiI; }",
    f32 ->: f32 ->: f32)

  implicit private class MultiInput(o: Type) {
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
    f32 `x3 ->:` f32 `x3 ->:` f32 ->: (f32 x f32) ->: (f32 x f32))

  val computePhiMag: Expr = depFun((k: Nat) => fun(
    (k `.` f32) ->: (k `.` f32) ->: (k `.` f32)
  )((phiR, phiI) =>
    mapGlobal(fun(t => phiMag(t._1)(t._2)))(zip(phiR)(phiI))
  ))

  val computeQ: Expr = depFun((k: Nat, x: Nat) => fun(
    (x `.` f32) `x3 ->:` (x `.` f32) ->: (x `.` f32) ->: (k `.` (f32 x f32 x f32 x f32)) ->: (x `.` (f32 x f32))
  )((x, y, z, Qr, Qi, kvalues) =>
    zip(x)(zip(y)(zip(z)(zip(Qr)(Qi)))) |>
    mapGlobal(fun(t =>
      let (toPrivate(t._1))
      be (sX =>
        let (toPrivate(t._2._1))
        be (sY =>
          let (toPrivate(t._2._2._1))
          be (sZ =>
            kvalues |> oclReduceSeq(AddressSpace.Private)(fun((acc, p) =>
              qFun(sX)(sY)(sZ)(p._1._1._1)(p._1._1._2)(p._1._2)(p._2)(acc)
            ))(makePair(t._2._2._2._1)(t._2._2._2._2))
          )
        )
      )
    ))
  ))

  import shine.OpenCL._
  import util.{Time, TimeSpan}

  def runOriginalComputePhiMag(
    name: String,
    phiR: Array[Float],
    phiI: Array[Float]
  ): (Array[Float], TimeSpan[Time.ms]) = {
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

  def runComputePhiMag(
    k: KernelExecutor.KernelNoSizes,
    phiR: Array[Float],
    phiI: Array[Float]
  ): (Array[Float], TimeSpan[Time.ms]) = {
    val K = phiR.length
    val localSize = LocalSize(1)
    val globalSize = GlobalSize(K)

    val f = k.as[ScalaFunction `(`
      Int `,` Array[Float] `,` Array[Float]
      `)=>` Array[Float]]
    f(localSize, globalSize)(K `,` phiR `,` phiI)
  }

  def runOriginalComputeQ(
    name: String,
    x: Array[Float],
    y: Array[Float],
    z: Array[Float],
    Qr: Array[Float],
    Qi: Array[Float],
    kvalues: Array[Float]
  ): (Array[Float], TimeSpan[Time.ms]) = {
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

  def runComputeQ(
    k: KernelExecutor.KernelNoSizes,
    x: Array[Float],
    y: Array[Float],
    z: Array[Float],
    Qr: Array[Float],
    Qi: Array[Float],
    kvalues: Array[Float]
  ): (Array[Float], TimeSpan[Time.ms]) = {
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
    f(localSize, globalSize)(
      K `,` X `,` x `,` y `,` z `,` Qr `,` Qi `,` kvalues
    )
  }
}
