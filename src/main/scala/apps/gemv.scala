package apps

import rise.core._
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.primitives.{let => _, _}
import rise.core.types._
import rise.core.types.DataType._
import HighLevelConstructs.reorderWithStride

object gemv {
  // we can use implicit type parameters and type annotations to specify the function type of mult
  val mult = impl{ dt: DataType => fun(x => x.`1` * x.`2`) :: ((dt x dt) ->: dt) }
  val add = fun(x => fun(y => x + y))
  val scal = impl { n: Nat =>
    fun(xs => fun(a =>
      map(fun(x => a * x))(xs)
    )) :: (ArrayType(n, f32) ->: f32 ->: ArrayType(n, f32))
  }
  val scalSeq = impl { n: Nat =>
    fun(xs => fun(a =>
      mapSeq(fun(x => a * x))(xs)
    )) :: (ArrayType(n, f32) ->: f32 ->: ArrayType(n, f32))
  }
  val dot = separableConvolution2D.dot
  val dotSeq = separableConvolution2D.dotSeq

  val gemvHighLevel = depFun((n: Nat, m: Nat) => fun(
    (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
      (m`.`f32)
  )((mat, xs, ys, alpha, beta) =>
    zip(map(fun(row => alpha * dot(row, xs)))(mat))(scal(ys, beta)) |>
    map(fun(x => x.`1` + x.`2`))
  ))

  val gemvSequential = depFun((n: Nat, m: Nat) => fun(
    (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
      (m`.`f32)
  )((mat, xs, ys, alpha, beta) =>
    toMem(zip(mapSeq(fun(row => alpha * dotSeq(row, xs)))(mat))(scalSeq(ys, beta))) |>
    mapSeq(fun(x => x.`1` + x.`2`))
  ))

  object ocl {
    import rise.openCL.DSL._
    import rise.openCL.primitives.{mapWorkGroup => _, mapLocal => _, _}

    val gemvBlastN = depFun((n: Nat, m: Nat) => fun(
      (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
        (m`.`f32)
    )((mat, xs, ys, alpha, beta) =>
      join o mapWorkGroup(fun(matChunk => // matChunk: 64.(n.f32 x f32)
        fun(y => mapLocal(fun(x =>
          fst(x) * alpha + snd(x) * beta
        )) $ zip(y)(map(snd) $ matChunk)) o
        // TODO: check address space
        oclReduceSeq(AddressSpace.Private)(fun((acc, next) => // next: 64.64.f32 x 64.f32
          let (toLocal(mapLocal(fun(x => x))(snd(next))))
          be (localX => // localX: 64.f32
            mapLocal(fun(x => // x: f32 x 64.f32
              // TODO: check address space
              oclReduceSeq(AddressSpace.Private)(fun((acc2, next2) => // next2: (f32 x f32)
                acc2 + fst(next2) * snd(next2)
              ))(fst(x)) $ zip(snd(x))(localX)
            )) $ zip(acc)(fst(next)))
        ))(mapLocal(fun(x => x))(generate(fun(_ => lf32(0.0f))) :: (64`.`f32))) $
          zip(transpose o map(split(64) o fst) $ matChunk)(split(64) $ xs)
      )) o split(64) $ zip(mat)(ys)
    ))

    val gemvBlastT = depFun((n: Nat, m: Nat) => fun(
      (n`.`m`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
        (m`.`f32)
    )((mat, xs, ys, alpha, beta) =>
      gemvBlastN(n)(m)(transpose(mat))(xs)(ys)(alpha)(beta)
    ))

    val gemvFused = depFun((n: Nat, m: Nat) => fun(
      (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
        (m`.`f32)
    )((mat, xs, ys, alpha, beta) =>
      zip(mat)(ys) |>
        mapWorkGroup(fun(t =>
          zip(xs)(t.`1`) |>
            split(n) |>
            toLocalFun(mapLocal(
              oclReduceSeq(AddressSpace.Private)(fun(a => fun(x => mult(x) + a)))(lf32(0.0f))
            )) |>
            mapLocal(fun(x => (alpha * x) + (t.`2` * beta)))
        )) |> join
    ))

    val gemvFusedAMD = depFun((n: Nat, m: Nat) => fun(
      (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->: (m`.`f32)
    )((mat, xs, ys, alpha, beta) =>
      zip(mat)(ys) |>
        mapWorkGroup(fun(t =>
          zip(xs)(t.`1`) |>
            reorderWithStride(128) |>
            split(n /^ 128) |>
            toLocalFun(mapLocal(
              oclReduceSeq(AddressSpace.Private)(fun(a => fun(x => mult(x) + a)))(lf32(0.0f))
            )) |>
            split(128) |>
            toLocalFun(mapLocal(oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f)))) |>
            mapLocal(fun(x => (alpha * x) + (t.`2` * beta)))
        )) |> join
    ))

    val gemvKeplerBest = depFun((n: Nat, m: Nat) => fun(
      (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
        (m`.`f32)
    )((mat, xs, ys, alpha, beta) =>
      zip(mat)(ys) |>
        mapWorkGroup(fun(t =>
          zip(xs)(t.`1`) |>
            reorderWithStride(128) |>
            split(n /^ 128) |>
            toLocalFun(mapLocal(
              oclReduceSeq(AddressSpace.Private)(fun(a => fun(x => mult(x) + a)))(lf32(0.0f))
            )) |>
            toLocalFun(oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))) |>
            fun(x => (alpha * x) + (t.`2` * beta))
        ))
    ))
  }

  object omp {
    import rise.openMP.primitives._

    val gemvFused = depFun((n: Nat, m: Nat) => fun(
      (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
        (m`.`f32)
    )((mat, xs, ys, alpha, beta) =>
      zip(mat)(ys) |>
        mapPar(fun(t =>
          zip(xs)(t.`1`) |>
            split(n) |>
            toMemFun(mapSeq(reduceSeq(fun(a => fun(x => mult(x) + a)))(lf32(0.0f)))) |>
            mapSeq(fun(x => (alpha * x) + (t.`2` * beta)))
        )) |> join
    ))
  }

  import shine.OpenCL._
  import shine.DPIA
  import util._

  val cgo17_localSize: LocalSize = LocalSize(64)
  val gemvBlastKnowSizes: gen.opencl.PhraseDepLocalAndGlobalSize =
    gen.opencl.PhraseDepLocalAndGlobalSize(phrase => {
      val t = phrase.t.asInstanceOf[DPIA.`(nat)->:`[DPIA.`(nat)->:`[DPIA.Types.ExpType]]]
      val m = t.t.x
      gen.opencl.LocalAndGlobalSize(cgo17_localSize, GlobalSize(m))
    })

  def runOriginal(name: String,
                  mat: Array[Array[Float]],
                  xs: Array[Float],
                  ys: Array[Float],
                  alpha: Float,
                  beta: Float): (Array[Float], TimeSpan[Time.ms]) = {
    import opencl.executor._

    val N = xs.length
    val M = ys.length
    val localSize = cgo17_localSize
    val globalSize = GlobalSize(M)

    val code = util.readFile(s"src/main/scala/apps/originalLift/$name")
    val kernelJNI = Kernel.create(code, "KERNEL", "")

    val float_bytes = 4
    val output_bytes = M * float_bytes
    val g_out = GlobalArg.createOutput(output_bytes)
    val kernelArgs = Array(
      GlobalArg.createInput(mat.flatten),
      GlobalArg.createInput(xs),
      GlobalArg.createInput(ys),
      ValueArg.create(alpha),
      ValueArg.create(beta),
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

  def runKernel(kernel: KernelExecutor.KernelNoSizes,
                mat: Array[Array[Float]],
                xs: Array[Float],
                ys: Array[Float],
                alpha: Float,
                beta: Float): (Array[Float], TimeSpan[Time.ms]) = {
    val N = xs.length
    val M = ys.length
    val localSize = cgo17_localSize
    val globalSize = GlobalSize(M)

    val run = kernel.as[In `=`
      Int `,` Int `,` Array[Array[Float]] `,`
      Array[Float] `,` Array[Float] `,` Float `,` Float, Out[Array[Float]]]
    run(localSize, globalSize)(N `,` M `,` mat `,` xs `,` ys `,` alpha `,` beta)
  }
}
