package apps

import rise.core._
import rise.core.DSL._
import rise.core.primitives.{let => _, _}
import rise.core.DSL.Type._
import rise.core.types._
import rise.core.types.DataType._
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeq

object mriQ {
  private val phiMag = foreignFun("phiMag",
    Seq("phiR", "phiI"),
    "{ return phiR * phiR + phiI * phiI; }",
    f32 ->: f32 ->: f32)

  implicit private class MultiInput(o: ExprType) {
    def `x3 ->:`(i: ExprType): FunType[ExprType, ExprType] =
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

  // FIXME: could not find original Lift expression, this is made up
  val computePhiMagHighLevel: Expr = depFun((k: Nat) => fun(
    (k `.` f32) ->: (k `.` f32) ->: (k `.` f32)
  )((phiR, phiI) =>
    map(fun(t => phiMag(t.`1`)(t.`2`)))(zip(phiR)(phiI))
  ))

  val computePhiMagOclKnownSizes = util.gen.opencl.PhraseDepLocalAndGlobalSize(phrase => {
    import shine.DPIA
    import shine.OpenCL.{LocalSize, GlobalSize}

    val t = phrase.t.asInstanceOf[DPIA.`(nat)->:`[DPIA.Types.ExpType]]
    val k = t.x
    util.gen.opencl.LocalAndGlobalSize(LocalSize(256), GlobalSize(k))
  })

  val computePhiMagOcl: Expr = depFun((k: Nat) => fun(
    (k `.` f32) ->: (k `.` f32) ->: (k `.` f32)
  )((phiR, phiI) =>
    mapGlobal(fun(t => phiMag(t.`1`)(t.`2`)))(zip(phiR)(phiI))
  ))

  // FIXME: could not find original Lift expression, this is made up
  val computeQHighLevel: Expr = depFun((k: Nat, x: Nat) => fun(
    (x `.` f32) `x3 ->:` (x `.` f32) ->: (x `.` f32) ->: (k `.` (f32 x f32 x f32 x f32)) ->: (x `.` (f32 x f32))
  )((x, y, z, Qr, Qi, kvalues) =>
    zip(x)(zip(y)(zip(z)(zip(Qr)(Qi)))) |>
      map(fun(t =>
          kvalues |> reduceSeq(fun((acc, p) =>
            qFun(t.`1`)(t.`2`.`1`)(t.`2`.`2`.`1`)(p.`1`.`1`.`1`)(p.`1`.`1`.`2`)(p.`1`.`2`)(p.`2`)(acc)
          ))(makePair(t.`2`.`2`.`2`.`1`)(t.`2`.`2`.`2`.`2`))
      ))
  ))

  val computeQOclKnownSizes = util.gen.opencl.PhraseDepLocalAndGlobalSize(phrase => {
    import shine.DPIA
    import shine.OpenCL.{LocalSize, GlobalSize}

    val t = phrase.t.asInstanceOf[DPIA.`(nat)->:`[DPIA.`(nat)->:`[DPIA.Types.ExpType]]]
    val x = t.t.x
    util.gen.opencl.LocalAndGlobalSize(LocalSize(256 / 4), GlobalSize(x))
  })

  val computeQOcl: Expr = depFun((k: Nat, x: Nat) => fun(
    (x `.` f32) `x3 ->:` (x `.` f32) ->: (x `.` f32) ->: (k `.` (f32 x f32 x f32 x f32)) ->: (x `.` (f32 x f32))
  )((x, y, z, Qr, Qi, kvalues) =>
    zip(x)(zip(y)(zip(z)(zip(Qr)(Qi)))) |>
    mapGlobal(fun(t =>
      let (toPrivate(t.`1`))
      be (sX =>
        let (toPrivate(t.`2`.`1`))
        be (sY =>
          let (toPrivate(t.`2`.`2`.`1`))
          be (sZ =>
            kvalues |> oclReduceSeq(AddressSpace.Private)(fun((acc, p) =>
              qFun(sX)(sY)(sZ)(p.`1`.`1`.`1`)(p.`1`.`1`.`2`)(p.`1`.`2`)(p.`2`)(acc)
            ))(makePair(t.`2`.`2`.`2`.`1`)(t.`2`.`2`.`2`.`2`))
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
    val localSize = LocalSize(256)
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
    val localSize = LocalSize(256)
    val globalSize = GlobalSize(K)

    val f = k.as[In `=`
      Int `,` Array[Float] `,` Array[Float],
      Out[Array[Float]]]
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
    val localSize = LocalSize(256 / 4)
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
    val localSize = LocalSize(256 / 4)
    val globalSize = GlobalSize(X)

    val f = k.as[In `=`
      Int `,` Int `,`
      Array[Float] `,` Array[Float] `,` Array[Float] `,`
      Array[Float] `,` Array[Float] `,` Array[Float],
      Out[Array[Float]]]
    f(localSize, globalSize)(
      K `,` X `,` x `,` y `,` z `,` Qr `,` Qi `,` kvalues
    )
  }
}
