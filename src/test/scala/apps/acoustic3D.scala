package apps

import lift.core._
import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import idealised.OpenCL._
import lift.OpenCL.primitives._
import lift.core.HighLevelConstructs._
import idealised.utils._
import idealised.util.gen

class acoustic3D extends idealised.util.TestsWithExecutor {
  val getNumNeighbours = foreignFun("idxF", Seq("i", "j", "k", "m", "n", "o"),
    """{
      |  int count = 6;
      |  if (i == (m - 1) || i == 0){ count--; }
      |  if (j == (n - 1) || j == 0){ count--; }
      |  if (k == (o - 1) || k == 0){ count--; }
      |  return count;
      |}""".stripMargin,
    int ->: int ->: int ->: int ->: int ->: int ->: int)

  val generateNumNeighbours = nFun(o => nFun(n => nFun(m =>
    generate(fun(k =>
      generate(fun(j =>
        generate(fun(i =>
          getNumNeighbours(cast(i))(cast(j))(cast(k))
            (cast(m: Expr))(cast(n: Expr))(cast(o: Expr))))))))
  )))

  val getCF = foreignFun("getCF", Seq("neigh", "cfB", "cfI"),
    "{ if (neigh < 6) { return cfB; } else { return cfI; } }",
    int ->: float ->: float ->: float)

  val SR = 441.0f
  val alpha = 0.005f
  val c = 344.0f
  val NF = 4410
  val k = 1.0f / SR
  val h = Math.sqrt(3.0f) * c * k
  val lambda = c * k / h

  val loss1 = 1.0f / (1.0f + lambda * alpha)
  val loss2 = 1.0f - lambda * alpha

  val l2 = l(((c * c * k * k) / (h * h)).toFloat)
  val cf1 = Array(loss1.toFloat, 1.0f).map(l)
  val cf21 = Array(loss2.toFloat, 1.0f).map(l)

  val sz: Nat = 3
  val st: Nat = 1

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

  val zip3D: Expr = zipND(3)

  val stencil: Expr = nFun(o => nFun(n => nFun(m => fun(
    ((o + 2)`.`(n + 2)`.`(m + 2)`.`float) ->:
    ((o + 2)`.`(n + 2)`.`(m + 2)`.`float) ->:
    (o`.`n`.`m`.`float)
  )((mat1, mat2) =>
    mapGlobal(2)(mapGlobal(1)(mapGlobal(0)(acoustic)))
      o slide3D(sz)(st)
      $ zip3D(mat1)(zip3D(mat2)(generateNumNeighbours(o+2)(n+2)(m+2)))
  ))))

  val stencilMSS: Expr = nFun(o => nFun(n => nFun(m => fun(
    ((o + 2)`.`(n + 2)`.`(m + 2)`.`float) ->:
    ((o + 2)`.`(n + 2)`.`(m + 2)`.`float) ->:
    (o`.`n`.`m`.`float)
  )((mat1, mat2) =>
    transpose o map(transpose) o transpose o
    mapGlobal(0)(mapGlobal(1)(
      // TODO: mapSeqSlide
      mapSeq(acoustic) o slide(sz)(st)
        o transpose o map(transpose)
    )) o transpose o slide2D(sz)(st) o map(transpose) o transpose
      $ zip3D(mat1)(zip3D(mat2)(generateNumNeighbours(o+2)(n+2)(m+2)))
  ))))

  val N = 128
  val M = 64
  val O = 32
  val localSize = LocalSize((32, 4, 1))
  val globalSize = GlobalSize((N, M, 1))

  test("stencil compiles to syntactically correct OpenCL") {
    gen.OpenCLKernel(stencil)
  }

  def runOriginalKernel(name: String,
                        mat1: Array[Array[Array[Float]]],
                        mat2: Array[Array[Array[Float]]]): (Array[Float], TimeSpan[Time.ms]) = {
    import opencl.executor._

    val source = io.Source.fromFile(s"src/test/scala/apps/$name")
    val code = try source.getLines.mkString("\n") finally source.close
    val kernelJNI = Kernel.create(code, "KERNEL", "")

    val float_bytes = 4
    val output_bytes = O * N * M * float_bytes
    val g_out = GlobalArg.createOutput(output_bytes)
    val g_mat1 = GlobalArg.createInput(mat1.flatten.flatten)
    val g_mat2 = GlobalArg.createInput(mat2.flatten.flatten)
    val kernelArgs = Array(
      g_mat1, g_mat2, g_out,
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
    val mat1 = Array.fill(O + 2, N + 2, M + 2)(random.nextInt(1000).toFloat)
    val mat2 = Array.fill(O + 2, N + 2, M + 2)(random.nextInt(1000).toFloat)
    val (out1, t1) = runOriginalKernel("oldAcoustic3D.cl", mat1, mat2)
    val (out2, t2) = runOriginalKernel("oldAcoustic3DMSS.cl", mat1, mat2)
    val (out3, t3) = runKernel(gen.OpenCLKernel(stencil), mat1, mat2)
    val (out4, t4) = runKernel(gen.OpenCLKernel(stencilMSS), mat1, mat2)

    def check(a: Array[Float], b: Array[Float]): Unit =
      a.zip(b).foreach { case (a, b) => assert(Math.abs(a - b) < 0.01) }

    check(out1, out2)
    check(out1, out3)
    check(out1, out4)
    println(s"t1: $t1")
    println(s"t2: $t2")
    println(s"t3: $t3")
    println(s"t4: $t4")
  }
}
