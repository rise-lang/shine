package apps

import lift.core._
import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import lift.core.HighLevelConstructs._
import lift.OpenCL.primitives._
import idealised.utils._
import util.gen

class acoustic3D extends util.TestsWithExecutor {
  private val getNumNeighbours = foreignFun("idxF", Seq("i", "j", "k", "m", "n", "o"),
    """{
      |  int count = 6;
      |  if (i == (m - 1) || i == 0){ count--; }
      |  if (j == (n - 1) || j == 0){ count--; }
      |  if (k == (o - 1) || k == 0){ count--; }
      |  return count;
      |}""".stripMargin,
    int ->: int ->: int ->: int ->: int ->: int ->: int)

  private val generateNumNeighbours = nFun(o => nFun(n => nFun(m =>
    generate(fun(k =>
      generate(fun(j =>
        generate(fun(i =>
          getNumNeighbours(cast(i))(cast(j))(cast(k))
            (cast(m: Expr))(cast(n: Expr))(cast(o: Expr))))))))
  )))

  private val getCF = foreignFun("getCF", Seq("neigh", "cfB", "cfI"),
    "{ if (neigh < 6) { return cfB; } else { return cfI; } }",
    int ->: float ->: float ->: float)

  private val SR = 441.0f
  private val alpha = 0.005f
  private val c = 344.0f
  private val NF = 4410
  private val k = 1.0f / SR
  private val h = Math.sqrt(3.0f) * c * k
  private val lambda = c * k / h

  private val loss1 = 1.0f / (1.0f + lambda * alpha)
  private val loss2 = 1.0f - lambda * alpha

  private val l2 = l(((c * c * k * k) / (h * h)).toFloat)
  private val cf1 = Array(loss1.toFloat, 1.0f).map(l)
  private val cf21 = Array(loss2.toFloat, 1.0f).map(l)

  private val sz: Nat = 3
  private val st: Nat = 1

  val acoustic: Expr = fun(
    (3`.`3`.`3`.`TupleType(float, TupleType(float, int))) ->: float
  )(tile => {
    val x = tile `@` lidx(1, 3) `@` lidx(1, 3) `@` lidx(1, 3) |> snd |> snd
    val cf = toPrivate(getCF(x)(cf1(0))(cf1(1)))
    val cf2 = toPrivate(getCF(x)(cf21(0))(cf21(1)))
    val maskedValStencil = l2

    def t(i: Int, j: Int, k: Int) =
      tile `@` lidx(i, 3) `@` lidx(j, 3) `@` lidx(k, 3) |> snd |> fst

    val stencil = toPrivate(
      t(0, 1, 1) + t(1, 0, 1) + t(1, 1, 0) + t(1, 1, 2) + t(1, 2, 1) + t(2, 1, 1))

    val valueMat1 = tile `@` lidx(1, 3) `@` lidx(1, 3) `@` lidx(1, 3) |> fst
    val valueMask = cast(x) :: float

    ((t(1, 1, 1) * (l(2.0f) - (valueMask * l2))) +
      ((stencil * maskedValStencil) - (valueMat1 * cf2))
    ) * cf
  })

  private val zip3D: Expr = zipND(3)

  val stencil: Expr = nFun(o => nFun(n => nFun(m => fun(
    ((o + 2)`.`(n + 2)`.`(m + 2)`.`float) ->:
    ((o + 2)`.`(n + 2)`.`(m + 2)`.`float) ->:
    (o`.`n`.`m`.`float)
  )((mat1, mat2) =>
    mapGlobal(2)(mapGlobal(1)(mapGlobal(0)(acoustic)))
      o slide3D(sz)(st)
      $ zip3D(mat1)(zip3D(mat2)(generateNumNeighbours(o+2)(n+2)(m+2)))
  ))))

  val id = fun(x => x)
  val stencilMSS: Expr = nFun(o => nFun(n => nFun(m => fun(
    ((o + 2)`.`(n + 2)`.`(m + 2)`.`float) ->:
    ((o + 2)`.`(n + 2)`.`(m + 2)`.`float) ->:
    (o`.`n`.`m`.`float)
  )((mat1, mat2) =>
    transpose o map(transpose) o transpose o
    mapGlobal(0)(mapGlobal(1)(
      oclSlideSeq(slideSeq.Values)(AddressSpace.Private)(sz)(st)(mapSeqUnroll(mapSeqUnroll(id)))(acoustic)
        o transpose o map(transpose)
    )) o transpose o slide2D(sz, st) o map(transpose) o transpose
      $ zip3D(mat1)(zip3D(mat2)(generateNumNeighbours(o+2)(n+2)(m+2)))
  ))))

  import idealised.OpenCL._
  private val N = 128
  private val M = 64
  private val O = 32
  private val localSize = LocalSize((32, 4))
  private val globalSize = GlobalSize((N, M))

  def runOriginalKernel(name: String,
                        mat1: Array[Array[Array[Float]]],
                        mat2: Array[Array[Array[Float]]]): (Array[Float], TimeSpan[Time.ms]) = {
    import opencl.executor._

    val source = io.Source.fromFile(s"src/test/scala/apps/originalLift/$name")
    val code = try source.getLines.mkString("\n") finally source.close
    val kernelJNI = Kernel.create(code, "KERNEL", "")

    val float_bytes = 4
    val output_bytes = O * N * M * float_bytes
    val g_out = GlobalArg.createOutput(output_bytes)
    val kernelArgs = Array(
      GlobalArg.createInput(mat1.flatten.flatten),
      GlobalArg.createInput(mat2.flatten.flatten),
      g_out,
      ValueArg.create(M), ValueArg.create(N), ValueArg.create(O)
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
                mat1: Array[Array[Array[Float]]],
                mat2: Array[Array[Array[Float]]]): (Array[Float], TimeSpan[Time.ms]) = {
    val f = k.as[ScalaFunction `(`
      Int `,` Int `,` Int `,`
      Array[Array[Array[Float]]] `,`
      Array[Array[Array[Float]]]
      `)=>` Array[Float]]
    f(localSize, globalSize)(O `,` N `,` M `,` mat1 `,` mat2)
  }

  test("acoustic stencils produce same results") {
    val random = new scala.util.Random()
    val mat1 = Array.fill(O + 2, N + 2, M + 2)(random.nextFloat * random.nextInt(1000))
    val mat2 = Array.fill(O + 2, N + 2, M + 2)(random.nextFloat * random.nextInt(1000))

    util.runsWithSameResult(Seq(
      ("original", runOriginalKernel("acoustic3D.cl", mat1, mat2)),
      ("originalMSS", runOriginalKernel("acoustic3DMSS.cl", mat1, mat2)),
      ("dpia", runKernel(gen.OpenCLKernel(stencil), mat1, mat2)),
      ("dpiaMSS", runKernel(gen.OpenCLKernel(stencilMSS), mat1, mat2))
    ))
  }
}
