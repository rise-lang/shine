package apps

import rise.core._
import rise.core.dsl._
import rise.core.exprs.primitives._
import rise.core.dsl.Type._
import rise.core.exprs.Expr
import rise.core.types._
import rise.opencl.dsl._
import rise.opencl.primitives.oclReduceSeq

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
    vec(4, f32) ->: vec(4, f32) ->: f32 ->: f32 ->: vec(4, f32) ->: vec(4, f32)
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
    vec(4, f32) ->: vec(4, f32) ->: f32 ->: vec(4, f32) ->: PairType(vec(4, f32), vec(4, f32))
  )

  val amd: ToBeTyped[Expr] = depFun((n: Nat) => fun(
    (n`.`vec(4, f32)) ->: (n`.`vec(4, f32)) ->: f32 ->: f32 ->: (n`.`(vec(4, f32) x vec(4, f32)))
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
  val nvidia: ToBeTyped[Expr] = depFun((n: Nat) => fun(
    (n`.`vec(4, f32)) ->: (n`.`vec(4, f32)) ->: f32 ->: f32 ->: (n`.`(vec(4, f32) x vec(4, f32)))
  )((pos, vel, espSqr, deltaT) =>
    join o join o mapWorkGroup(1)(
      join o mapWorkGroup(0)(fun((tileX`.`(vec(4, f32) x vec(4, f32))) ->: (tileY`.`tileX`.`(vec(4, f32) x vec(4, f32))))(p1Chunk =>
        fun(tileX`.`(vec(4, f32) x vec(4, f32)))(newP1Chunk =>
          mapLocal(1)(fun(tileX`.`vec(4, f32))(bla =>
            mapLocal(0)(fun((vec(4, f32) x vec(4, f32)) x vec(4, f32))(p1 =>
              update(p1._1._1)(p1._1._2)(deltaT)(p1._2)
            ))(zip(newP1Chunk)(bla)))) o
            // TODO: is this the correct address space?
            oclReduceSeq(AddressSpace.Local)(
              fun(tileY`.`tileX`.`vec(4, f32))(acc => fun(tileY`.`tileX`.`vec(4, f32))(p2 =>
                let(
                  toLocal(mapLocal(1)(mapLocal(0)(id))(p2))
                )(fun(p2Local =>
                  mapLocal(1)(fun(((tileX`.`vec(4, f32)) x (tileX`.`vec(4, f32))) ->: (tileX`.`vec(4, f32)))(accDim2 =>
                    mapLocal(0)(fun(((vec(4, f32) x vec(4, f32)) x vec(4, f32)) ->: vec(4, f32))(p1 =>
                      oclReduceSeq(AddressSpace.Private)(fun(vec(4, f32) ->: vec(4, f32) ->: vec(4, f32))((acc, p2) =>
                        calcAcc(p1._1._1)(p2)(deltaT)(espSqr)(acc)
                      ))(p1._2)(accDim2._1)
                    )) $ zip(newP1Chunk)(accDim2._2)
                  )) $ zip(p2Local)(acc)
                ))
              )))(mapLocal(1)(mapLocal(0)(id))(generate(fun(_ => generate(fun(_ => vectorFromScalar(l(0.0f))))))))
            o split(tileY) o split(tileX) $ pos
          // TODO: toPrivate when it works..
        ) $ zip(toLocal(mapLocal(id)(unzip(p1Chunk)._1)))(unzip(p1Chunk)._2)
      )) o split(tileX)
    ) o split(n) $ zip(pos)(vel)
  ))

  import shine.OpenCL._
  import _root_.util.{Time, TimeSpan}

  private val deltaT = 0.005f
  private val espSqr = 500.0f

  def runOriginalKernel(name: String,
                        localSize: LocalSize,
                        globalSize: GlobalSize,
                        pos: Array[Float],
                        vel: Array[Float]): (Array[Float], TimeSpan[Time.ms]) = {
    import opencl.executor._

    val code = _root_.util.readFile(s"src/main/scala/apps/originalLift/$name")
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
