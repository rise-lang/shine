package apps

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import lift.OpenCL.primitives._
import util.gen
import idealised.utils.{Time, TimeSpan}

class molecularDynamics extends util.TestsWithExecutor {
  val mdCompute = foreignFun("updateF",
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
      float4 ->: float4 ->: float4 ->: float ->: float ->: float ->: float4)

  val shoc = nFun(n => nFun(m => fun(
    (n`.`float4) ->: (m`.`n`.`IndexType(n)) ->: float ->: float ->: float ->: (n`.`float4)
  )((particles, neighbourIds, cutsq, lj1, lj2) =>
    zip(particles)(transpose(neighbourIds)) |>
    split(128) |>
    mapWorkGroup(
      mapLocal(fun(p =>
        toPrivate(p._1) |> let(fun(particle =>
          gather(p._2)(particles) |>
          oclReduceSeq(AddressSpace.Private)(fun(force => fun(n =>
            mdCompute(force)(particle)(n)(cutsq)(lj1)(lj2)
          )))(vectorFromScalar(l(0.0f)))
        ))
      ))
    ) |> join
  )))

  def buildNeighbourList(position: Array[(Float, Float, Float, Float)], maxNeighbours: Int): Array[Array[Int]] = {
    val neighbourList = Array.ofDim[Int](position.length, maxNeighbours)

    for (i <- position.indices) {
      var currDist = List[(Int, Float)]()

      for (j <- position.indices) {
        if (i != j) {
          val ipos = position(i)
          val jpos = position(j)

          val delx = ipos._1 - jpos._1
          val dely = ipos._2 - jpos._2
          val delz = ipos._3 - jpos._3

          val distIJ = delx * delx + dely * dely + delz * delz
          currDist =  (j, distIJ) :: currDist
        }
      }

      currDist = currDist.sortBy(x => x._2)

      for (j <- 0 until maxNeighbours) {
        neighbourList(i)(j) = currDist(j)._1
      }
    }

    neighbourList
  }

  import idealised.OpenCL._
  val N = 1024
  val M = 128
  val cutsq = 16.0f
  val lj1 = 1.5f
  val lj2 = 2.0f

  val localSize = LocalSize(128)
  val globalSize = GlobalSize(N)

  def runOriginalKernel(name: String,
                        particles: Array[Float],
                        neighbours: Array[Array[Int]]): (Array[Float], TimeSpan[Time.ms]) = {
    import opencl.executor._

    val source = io.Source.fromFile(s"src/test/scala/apps/originalLift/$name")
    val code = try source.getLines.mkString("\n") finally source.close
    val kernelJNI = Kernel.create(code, "KERNEL", "")

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

  def runKernel(k: KernelNoSizes,
                particles: Array[Float],
                neighbours: Array[Array[Int]]): (Array[Float], TimeSpan[Time.ms]) = {
    val f = k.as[ScalaFunction `(`
      Int `,` Int `,` Array[Float] `,` Array[Array[Int]] `,` Float `,` Float `,` Float
      `)=>` Array[Float]]
    f(localSize, globalSize)(N `,` M `,` particles `,` neighbours `,` cutsq `,` lj1 `,` lj2)
  }

  test("molecular dynamics versions produce same results") {
    val random = new scala.util.Random()
    val particles = Array.fill(N * 4)(random.nextFloat() * 20.0f)
    val particlesTuple = particles.sliding(4, 4).map { case Array(a, b, c, d) => (a, b, c, d) }.toArray
    val neighbours = buildNeighbourList(particlesTuple, M).transpose

    util.runsWithSameResult(Seq(
      ("original", runOriginalKernel("MolecularDynamics.cl", particles, neighbours)),
      ("dpia", runKernel(gen.OpenCLKernel(shoc), particles, neighbours))
    ))
  }
}
