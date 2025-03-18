package apps

import rise.core._
import rise.core.DSL._
import rise.core.primitives.{let => _, _}
import rise.core.DSL.Type._
import rise.core.types._
import rise.core.types.DataType._
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeq

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

  private val calcAccScal = foreignFun("calcAcc",
    Seq("p1", "p2", "mass2", "espSqr"),
    """|{
       |  float rx = p1._fst - p2._fst;
       |  float ry = p1._snd._fst - p2._snd._fst;
       |  float rz = p1._snd._snd - p2._snd._snd;
       |  float distSqr = rx + ry + rz;
       |  float invDist = 1.0f / sqrt(distSqr + espSqr);
       |  float invDistCube = invDist * invDist * invDist;
       |  float s = invDistCube * mass;
       |  Tuple acc = {s * rx, s * ry, s * rz};
       |  return acc;
       |}
       | """.stripMargin,
    (f32 x (f32 x f32)) ->: (f32 x (f32 x f32)) ->: f32 ->: f32 ->:
    (f32 x (f32 x f32)))

  private val addScal = fun(
    (f32 x (f32 x f32)) ->: (f32 x (f32 x f32)) ->: (f32 x (f32 x f32))
  )((a, b) => {
    def r(f: ToBeTyped[Expr]) = f(a) + f(b)
    makePair(r(fst))(makePair(r(snd >> fst))(r(snd >> snd)))
  })

  private val updateScal = fun(
    ((f32 x (f32 x f32)) x (f32 x (f32 x f32))) ->:
    f32 ->: (f32 x (f32 x f32)) ->:
    ((f32 x (f32 x f32)) x (f32 x (f32 x f32)))
  )((xyzvXYZ, deltaT, acceleration) => {
    val xyz = fst(xyzvXYZ)
    val vXYZ = snd(xyzvXYZ)
    def p(f: ToBeTyped[Expr]): ToBeTyped[Expr] =
      f(xyz) + f(vXYZ) * deltaT + lf32(0.5f) * f(acceleration) * deltaT * deltaT
    def v(f: ToBeTyped[Expr]): ToBeTyped[Expr] =
      f(vXYZ) + f(acceleration) * deltaT
    makePair(
      makePair(p(fst))(makePair(p(snd >> fst))(p(snd >> snd))))(
      makePair(v(fst))(makePair(v(snd >> fst))(v(snd >> snd))))
  })

  // FIXME: signature does not match low-level versions
  // also, this code was not verified
  val nbodyHighLevel: ToBeTyped[Expr] = depFun((n: Nat) => fun(
    (n`.`f32) ->: (n`.`f32) ->: (n`.`f32) ->:
    (n`.`f32) ->: (n`.`f32) ->: (n`.`f32) ->:
    (n`.`f32) ->: f32 ->: f32 ->:
    (n`.`((f32 x (f32 x f32)) x (f32 x (f32 x f32))))
  )((x, y, z, velX, velY, velZ, mass, espSqr, deltaT) =>
    map(fun(p1 =>
      fun(acceleration => updateScal(p1, deltaT, acceleration)) o
      reduce(addScal)(makePair(lf32(0.0f))(makePair(lf32(0.0f))(lf32(0.0f)))) o
      map(fun(p2m =>
        calcAccScal(fst(p1), fst(p2m), snd(p2m), espSqr)
      )) $ zip(zip(x)(zip(y)(z)))(mass)
    )) $ zip(zip(x)(zip(y)(z)))(zip(velX)(zip(velY)(velZ)))
  ))

  val nbodyAMDKnownSizes = util.gen.opencl.PhraseDepLocalAndGlobalSize(phrase => {
    import shine.DPIA
    import shine.OpenCL.{LocalSize, GlobalSize}

    val t = phrase.t.asInstanceOf[DPIA.`(nat)->:`[DPIA.Types.ExpType]]
    val n = t.x
    util.gen.opencl.LocalAndGlobalSize(LocalSize(128), GlobalSize(n))
  })

  val nbodyAMD: ToBeTyped[Expr] = depFun((n: Nat) => fun(
    (n`.`vec(4, f32)) ->: (n`.`vec(4, f32)) ->: f32 ->: f32 ->: (n`.`(vec(4, f32) x vec(4, f32)))
  )((pos, vel, espSqr, deltaT) =>
    mapGlobal(fun(p1 =>
      update(fst(p1))(snd(p1))(deltaT) o
      oclReduceSeq(AddressSpace.Private)(fun((acc, p2) =>
        calcAcc(fst(p1))(p2)(deltaT)(espSqr)(acc)
      ))(vectorFromScalar(lf32(0.0f))) $ pos
    )) $ zip(pos)(vel)
  ))

  val tileX = 256
  val tileY = 1

  val nbodyNVIDIAKnownSizes = util.gen.opencl.PhraseDepLocalAndGlobalSize(phrase => {
    import shine.DPIA
    import shine.OpenCL.{LocalSize, GlobalSize}

    val t = phrase.t.asInstanceOf[DPIA.`(nat)->:`[DPIA.Types.ExpType]]
    val n = t.x
    util.gen.opencl.LocalAndGlobalSize(LocalSize((tileX, tileY)), GlobalSize((n, tileY)))
  })

  // TODO: compare generated code to original
  val nbodyNVIDIA: ToBeTyped[Expr] = depFun((n: Nat) => fun(
    (n`.`vec(4, f32)) ->: (n`.`vec(4, f32)) ->: f32 ->: f32 ->: (n`.`(vec(4, f32) x vec(4, f32)))
  )((pos, vel, espSqr, deltaT) =>
    join o join o mapWorkGroup(1)(
      join o mapWorkGroup(0)(fun((tileX`.`(vec(4, f32) x vec(4, f32))) ->: (tileY`.`tileX`.`(vec(4, f32) x vec(4, f32))))(p1Chunk =>
        fun(tileX`.`(vec(4, f32) x vec(4, f32)))(newP1Chunk =>
          mapLocal(1)(fun(tileX`.`vec(4, f32))(bla =>
            mapLocal(0)(fun((vec(4, f32) x vec(4, f32)) x vec(4, f32))(p1 =>
              update(p1.`1`.`1`)(p1.`1`.`2`)(deltaT)(p1.`2`)
            ))(zip(newP1Chunk)(bla)))) o
            // FIXME: there seems to be a bug in AdjustArraySizesForAllocations
            oclReduceSeq(AddressSpace.Private)(
              fun(tileY`.`tileX`.`vec(4, f32))(acc => fun(tileY`.`tileX`.`vec(4, f32))(p2 =>
                let (toLocal(mapLocal(1)(mapLocal(0)(id))(p2)))
                be (p2Local =>
                  mapLocal(1)(fun(((tileX`.`vec(4, f32)) x (tileX`.`vec(4, f32))) ->: (tileX`.`vec(4, f32)))(accDim2 =>
                    mapLocal(0)(fun(((vec(4, f32) x vec(4, f32)) x vec(4, f32)) ->: vec(4, f32))(p1 =>
                      oclReduceSeq(AddressSpace.Private)(fun(vec(4, f32) ->: vec(4, f32) ->: vec(4, f32))((acc, p2) =>
                        calcAcc(p1.`1`.`1`)(p2)(deltaT)(espSqr)(acc)
                      ))(p1.`2`)(accDim2.`1`)
                    )) $ zip(newP1Chunk)(accDim2.`2`)
                  )) $ zip(p2Local)(acc)
                )
              )))(mapLocal(1)(mapLocal(0)(id))(generate(fun(_ => generate(fun(_ => vectorFromScalar(lf32(0.0f))))))))
            o split(tileY) o split(tileX) $ pos
        ) $ zip(toPrivate(mapLocal(id)(unzip(p1Chunk).`1`)))(unzip(p1Chunk).`2`)
      )) o split(tileX)
    ) o split(n) $ zip(pos)(vel)
  ))

  import shine.OpenCL._
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

  def runKernel(k: KernelExecutor.KernelNoSizes,
                localSize: LocalSize,
                globalSize: GlobalSize,
                pos: Array[Float],
                vel: Array[Float]): (Array[Float], TimeSpan[Time.ms]) = {
    assert(pos.length % 4 == 0)
    val N = pos.length / 4

    val f = k.as[In `=`
      Int `,` Array[Float] `,` Array[Float] `,` Float `,` Float,
      Out[Array[Float]]]
    f(localSize, globalSize)(N `,` pos `,` vel `,` espSqr `,` deltaT)
  }

}
