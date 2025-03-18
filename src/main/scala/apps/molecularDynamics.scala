package apps

import rise.core._
import rise.core.DSL._
import rise.core.primitives.{let => _, _}
import rise.core.DSL.Type._
import rise.core.types._
import rise.core.types.DataType._
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeq

object molecularDynamics {
  private val mdCompute = foreignFun("updateF",
    Seq("f", "ipos", "jpos", "cutsq", "lj1", "lj2"),
    """{
      |  // Calculate distance
      |  float delx = ipos.x - jpos.x;
      |  float dely = ipos.y - jpos.y;
      |  float delz = ipos.z - jpos.z;
      |  float r2inv = delx*delx + dely*dely + delz*delz;
      |  // If distance is less than cutoff, calculate force
      |  if (r2inv < cutsq) {
      |    r2inv = 1.0f/r2inv;
      |    float r6inv = r2inv * r2inv * r2inv;
      |    float forceC = r2inv*r6inv*(lj1*r6inv - lj2);
      |    f.x += delx * forceC;
      |    f.y += dely * forceC;
      |    f.z += delz * forceC;
      |  }
      |  return f;
      }""".stripMargin,
    vec(4, f32) ->: vec(4, f32) ->: vec(4, f32) ->:
      f32 ->: f32 ->: f32 ->:
      vec(4, f32))

  // FIXME: could not find original Lift expression, this is made up
  val shocHighLevel: Expr = depFun((n: Nat, m: Nat) => fun(
    (n`.`vec(4, f32)) ->: (m`.`n`.`IndexType(n)) ->:
      f32 ->: f32 ->: f32 ->:
      (n`.`vec(4, f32))
  )((particles, neighbourIds, cutsq, lj1, lj2) =>
    zip(particles)(transpose(neighbourIds)) |>
      map(fun { p =>
        val particle = p.`1`
        gather(p.`2`)(particles) |>
        reduce(fun(force => fun(n =>
          mdCompute(force)(particle)(n)(cutsq)(lj1)(lj2)
        )))(vectorFromScalar(lf32(0.0f)))
      })
  ))

  val shocOclKnownSizes = util.gen.opencl.PhraseDepLocalAndGlobalSize(phrase => {
    import shine.DPIA
    import shine.OpenCL.{LocalSize, GlobalSize}

    val t = phrase.t.asInstanceOf[DPIA.`(nat)->:`[DPIA.Types.ExpType]]
    val n = t.x
    util.gen.opencl.LocalAndGlobalSize(LocalSize(128), GlobalSize(n))
  })

  val shocOcl: Expr = depFun((n: Nat, m: Nat) => fun(
    (n`.`vec(4, f32)) ->: (m`.`n`.`IndexType(n)) ->:
      f32 ->: f32 ->: f32 ->:
      (n`.`vec(4, f32))
    )((particles, neighbourIds, cutsq, lj1, lj2) =>
      zip(particles)(transpose(neighbourIds)) |>
      split(128) |>
      mapWorkGroup(
        mapLocal(fun(p =>
          let (toPrivate(p.`1`))
          be (particle =>
            gather(p.`2`)(particles) |>
            oclReduceSeq(AddressSpace.Private)(fun(force => fun(n =>
              mdCompute(force)(particle)(n)(cutsq)(lj1)(lj2)
            )))(vectorFromScalar(lf32(0.0f)))
          )
        ))
      ) |> join
  ))

  def buildNeighbourList(
    position: Array[(Float, Float, Float, Float)],
    maxNeighbours: Int
  ): Array[Array[Int]] = {
    val neighbourList = Array.ofDim[Int](position.length, maxNeighbours)

    for (i <- position.indices) {
      var currDist = scala.collection.mutable.ArrayBuffer[(Int, Float)]()

      for (j <- position.indices) {
        if (i != j) {
          val ipos = position(i)
          val jpos = position(j)

          val delx = ipos._1 - jpos._1
          val dely = ipos._2 - jpos._2
          val delz = ipos._3 - jpos._3

          val distIJ = delx * delx + dely * dely + delz * delz
          currDist += ((j, distIJ))
        }
      }

      currDist = currDist.sortBy(x => x._2)

      for (j <- 0 until maxNeighbours) {
        neighbourList(i)(j) = currDist(j)._1
      }
    }

    neighbourList
  }

  import shine.OpenCL._
  import util.{Time, TimeSpan}

  private val cutsq = 16.0f
  private val lj1 = 1.5f
  private val lj2 = 2.0f

  def runOriginalKernel(
    name: String,
    particles: Array[Float],
    neighbours: Array[Array[Int]]
  ): (Array[Float], TimeSpan[Time.ms]) = {
    import opencl.executor._

    val code = util.readFile(s"src/main/scala/apps/originalLift/$name")
    val kernelJNI = Kernel.create(code, "KERNEL", "")

    assert(particles.length % 4 == 0)
    val N = particles.length / 4
    val M = neighbours.length
    val localSize = LocalSize(128)
    val globalSize = GlobalSize(N)

    val float_bytes = 4
    val float4_bytes = 4 * float_bytes
    val output_bytes = N * float4_bytes
    val g_out = GlobalArg.createOutput(output_bytes)
    val kernelArgs = Array(
      GlobalArg.createInput(particles),
      GlobalArg.createInput(neighbours.flatten),
      ValueArg.create(cutsq), ValueArg.create(lj1), ValueArg.create(lj2),
      g_out,
      ValueArg.create(M), ValueArg.create(N)
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

  def runKernel(
    k: KernelExecutor.KernelNoSizes,
    particles: Array[Float],
    neighbours: Array[Array[Int]]
  ): (Array[Float], TimeSpan[Time.ms]) = {
    assert(particles.length % 4 == 0)
    val N = particles.length / 4
    val M = neighbours.length
    val localSize = LocalSize(128)
    val globalSize = GlobalSize(N)

    val f = k.as[In `=`
      Int `,` Int `,`
      Array[Float] `,` Array[Array[Int]] `,`
      Float `,` Float `,` Float,
      Out[Array[Float]]]
    f(localSize, globalSize)(
      N `,` M `,` particles `,` neighbours `,` cutsq `,` lj1 `,` lj2
    )
  }
}
