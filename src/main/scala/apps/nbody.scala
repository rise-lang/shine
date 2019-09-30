package apps

import lift.core._
import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import lift.OpenCL.primitives._

object nbody {
  private val id = fun(x => x)

  private val calcAcc = foreignFun("calcAcc",
    Seq("p1", "p2", "deltaT", "espSqr", "acc"),
    """{
      |  float4 r;
      |  r.xyz = p2.xyz - p1.xyz;
      |  float distSqr = r.x*r.x + r.y*r.y + r.z*r.z;
      |  float invDist = 1.0f / sqrt(distSqr + espSqr);
      |  float invDistCube = invDist * invDist * invDist;
      |  float s = invDistCube * p2.w;
      |  float4 res;
      |  res.xyz = acc.xyz + s * r.xyz;
      |  return res;
      |}
      |""".stripMargin,
    float4 ->: float4 ->: float ->: float ->: float4 ->: float4
  )
  private val update = foreignFun("update",
    Seq("pos", "vel", "deltaT", "acceleration"),
    """{
      |  float4 newPos;
      |  newPos.xyz = pos.xyz + vel.xyz * deltaT + 0.5f * acceleration.xyz * deltaT * deltaT;
      |  newPos.w = pos.w;
      |  float4 newVel;
      |  newVel.xyz = vel.xyz + acceleration.xyz * deltaT;
      |  newVel.w = vel.w;
      |  return (struct Record_float4_float4){ newPos, newVel };
      |}""".stripMargin,
    float4 ->: float4 ->: float ->: float4 ->: TupleType(float4, float4)
  )

  val amd: Expr = nFun(n => fun(
    (n`.`float4) ->: (n`.`float4) ->: float ->: float ->: (n`.`(float4 x float4))
  )((pos, vel, espSqr, deltaT) =>
    mapGlobal(fun(p1 =>
      update(fst(p1))(snd(p1))(deltaT) o
        oclReduceSeq(AddressSpace.Private)(fun((acc, p2) =>
          calcAcc(fst(p1))(p2)(deltaT)(espSqr)(acc)
        ))(vectorFromScalar(l(0.0f))) $ pos
    )) $ zip(pos)(vel)
  ))

  val tileX = 256
  val tileY = 1

  // TODO: compare generated code to original
  val nvidia: Expr = nFun(n => fun(
    (n`.`float4) ->: (n`.`float4) ->: float ->: float ->: (n`.`(float4 x float4))
  )((pos, vel, espSqr, deltaT) =>
    join o join o mapWorkGroup(1)(
      join o mapWorkGroup(0)(fun((tileX`.`(float4 x float4)) ->: (tileY`.`tileX`.`(float4 x float4)))(p1Chunk =>
        fun(tileX`.`(float4 x float4))(newP1Chunk =>
          mapLocal(1)(fun(tileX`.`float4)(bla =>
            mapLocal(0)(fun((float4 x float4) x float4)(p1 =>
              update(p1._1._1)(p1._1._2)(deltaT)(p1._2)
            ))(zip(newP1Chunk)(bla)))) o
            // TODO: is this the correct address space?
            oclReduceSeq(AddressSpace.Local)(
              fun(tileY`.`tileX`.`float4)(acc => fun(tileY`.`tileX`.`float4)(p2 =>
                let(fun((tileY`.`tileX`.`float4) ->: (tileY`.`tileX`.`float4))(p2Local =>
                  mapLocal(1)(fun(((tileX`.`float4) x (tileX`.`float4)) ->: (tileX`.`float4))(accDim2 =>
                    mapLocal(0)(fun(((float4 x float4) x float4) ->: float4)(p1 =>
                      oclReduceSeq(AddressSpace.Private)(fun(float4 ->: float4 ->: float4)((acc, p2) =>
                        calcAcc(p1._1._1)(p2)(deltaT)(espSqr)(acc)
                      ))(p1._2)(accDim2._1)
                    )) $ zip(newP1Chunk)(accDim2._2)
                  )) $ zip(p2Local)(acc)
                )) $ toLocal(mapLocal(1)(mapLocal(0)(id))(p2))
              )))(mapLocal(1)(mapLocal(0)(id))(generate(fun(_ => generate(fun(_ => vectorFromScalar(l(0.0f))))))))
            o split(tileY) o split(tileX) $ pos
          // TODO: toPrivate when it works..
        ) $ zip(toLocal(mapLocal(id)(unzip(p1Chunk)._1)))(unzip(p1Chunk)._2)
      )) o split(tileX)
    ) o split(n) $ zip(pos)(vel)
  ))

  import idealised.OpenCL._
  import util.{Time, TimeSpan}

  private val deltaT = 0.005f
  private val espSqr = 500.0f

  def runOriginalKernel(name: String,
                        localSize: LocalSize,
                        globalSize: GlobalSize,
                        pos: Array[Float],
                        vel: Array[Float]): (Array[Float], TimeSpan[Time.ms]) = {
    import opencl.executor._

    val code = util.readFile(s"src/main/scala/apps/originalLift/$name")
    val kernelJNI = Kernel.create(code, "KERNEL", "")

    assert(pos.length % 4 == 0)
    val N = pos.length / 4

    val float_bytes = 4
    val float4_bytes = 4 * float_bytes
    val output_bytes = N * 2 * float4_bytes
    val g_out = GlobalArg.createOutput(output_bytes)
    val kernelArgs = Array(
      GlobalArg.createInput(pos),
      GlobalArg.createInput(vel),
      ValueArg.create(espSqr), ValueArg.create(deltaT),
      g_out,
      ValueArg.create(N)
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
                pos: Array[Float],
                vel: Array[Float]): (Array[Float], TimeSpan[Time.ms]) = {
    assert(pos.length % 4 == 0)
    val N = pos.length / 4

    val f = k.as[ScalaFunction `(`
      Int `,` Array[Float] `,` Array[Float] `,` Float `,` Float
      `)=>` Array[Float]]
    f(localSize, globalSize)(N `,` pos `,` vel `,` espSqr `,` deltaT)
  }

}
