package apps

import rise.core._
import rise.core.DSL._
import rise.core.primitives.{let => _, _}
import rise.core.DSL.Type._
import rise.core.types._
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeq
import reflect.Selectable.reflectiveSelectable

object mm {
  private val id = fun(x => x)
  private val mulT = separableConvolution2D.mulT
  private val dot = separableConvolution2D.dot
  private val dotSeq = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))
  ))

  // the first matrix input is transposed

  val mmHighLevel: ToBeTyped[Expr] = depFun((n: Nat, m: Nat, o: Nat) => fun(
    (o`.`n`.`f32) ->: (o`.`m`.`f32) ->: (n`.`m`.`f32)
  )((at, b) =>
    transpose(at) |> map(fun(aRow =>
      transpose(b) |> map(fun(bCol =>
        dot(aRow)(bCol)
      ))
    ))
  ))

  val mmSequential: ToBeTyped[Expr] = depFun((n: Nat, m: Nat, o: Nat) => fun(
    (o`.`n`.`f32) ->: (o`.`m`.`f32) ->: (n`.`m`.`f32)
  )((at, b) =>
    transpose(at) |> mapSeq(fun(aRow =>
      transpose(b) |> mapSeq(fun(bCol =>
        dotSeq(aRow)(bCol)
      ))
    ))
  ))

  val mmAMD: ToBeTyped[Expr] = {
    val v3 = 4
    val v4 = 8
    val vw = 4

    depFun((n: Nat, m: Nat, o: Nat) => fun(
      (o`.`n`.`f32) ->: (o`.`m`.`f32) ->: (n`.`m`.`f32)
    )((at, b) =>
      split(v4)(transpose(at)) |> // Mo.Mi.o.f
      mapGlobal(1)(fun(v4`.`o`.`f32)(p3 =>
        split(v3)(transpose(b)) |> // No.Ni.o.f
        mapGlobal(0)(fun(v3`.`o`.`f32)(p4 =>
          zip(transpose(p3))(transpose(p4)) |> // o.(Mi.f x Ni.f)
          oclReduceSeq(AddressSpace.Private)(fun((p6, p7) =>
            let (toPrivate(makePair(mapSeq(id)(fst(p7)))(
              asScalar o mapSeq(id) o asVectorAligned(vw) $ snd(p7))))
            be (x =>
              mapSeq(fun(p8 =>
                mapSeq(fun(p9 =>
                  fst(p9) + (snd(p8) * snd(p9))
                ))(zip(fst(p8))(snd(x)))
              ))(zip(p6)(fst(x)))
            )
          ))(mapSeq(mapSeq(id))(generate(fun(_ => generate(fun(_ => lf32(0.0f)))))) :: (v4`.`v3`.`f32)) |> //
          mapSeq(asScalar o mapSeq(id) o asVector(vw)) |>
          transpose // v3.v4.f
        )) |> join |> transpose
      )) |> join
    ))
  }

  val mmNVIDIA: ToBeTyped[Expr] = {
    val v3 = 4
    val v4 = 8
    val v5 = 64
    val v6 = 128
    val v7 = 128
    val v8 = 16

    depFun((n: Nat, m: Nat, o: Nat) => fun(
      (o`.`n`.`f32) ->: (o`.`m`.`f32) ->: (n`.`m`.`f32)
    )((at, b) =>
      at |>
      map(split(v5)) |> split(v8) |> // O'.v8.N'.v5.f
      map(transpose) |> transpose |> // N'.O'.v8.v5.f
      mapWorkGroup(1)(fun(p2 =>
        b |>
        map(split(v7)) |> split(v8) |> // O'.v8.M'.v7.f
        map(transpose) |> transpose |> // M'.O'.v8.v7.f
        mapWorkGroup(0)(fun(p3 =>
          zip(p2)(p3) |> // O'.(v8.v5.f x v8.v7.f)
          oclReduceSeq(AddressSpace.Local)(fun((p13, p14) =>
            // (v5/^v4).(v7/^v3).v4.v3.f x (v8.v5.f x v8.v7.f)
            let (toLocal(makePair(
              fst(p14) |> join |> split(v6) |> // ((v8 x v5) /^ v6).v6.f
                mapLocal(1)(asScalar o mapLocal(0)(id) o asVectorAligned(4)) |>
                join |> split(v5)
              )( // v8.v5.f
              snd(p14) |> // v8.v7.f
                mapLocal(1)(asScalar o mapLocal(0)(id) o asVectorAligned(4))
              )))
            be (p15 =>
              zip(p13)(split(v4)(transpose(fst(p15)))) |> // (v5/^v4).((v7/^v3).v4.v3.f x v4.v8.f)
              mapLocal(1)(fun(p16 =>
                zip(fst(p16))(split(v3)(transpose(snd(p15)))) |> // (v7/^v3).(v4.v3.f x v3.v8.f)
                mapLocal(0)(fun(p17 =>
                zip(transpose(snd(p16)))(transpose(snd(p17))) |> // v8.(v4.f x v3.f)
                  oclReduceSeq(AddressSpace.Private)(fun((p19, p20) =>
                    // v4.v3.f x (v4.f x v3.f)
                    let (toPrivate(makePair(mapSeq(id)(fst(p20)))(mapSeq(id)(snd(p20)))))
                    be (p21 =>
                      zip(p19)(fst(p21)) |> // v4.(v3.f x f)
                      mapSeq(fun(p22 =>
                        zip(fst(p22))(snd(p21)) |> // v3.(f x f)
                        mapSeq(fun(p23 =>
                          fst(p23) + (snd(p22) * snd(p23))
                        ))
                      ))
                    )
                  ))(fst(p17) // v4.v3.f
                    |> mapSeq(mapSeq(id)) // TODO: think about that
                  ) |> mapSeq(mapSeq(id)) // TODO: think about that
                ))
              ))
            )
          ))(
            generate(fun(_ =>
              generate(fun( _ =>
                generate(fun(_ =>
                  generate(fun(_ => lf32(0.0f))))))))) |>
            mapLocal(1)(mapLocal(0)(mapSeq(mapSeq(id))))
          ) |> // (v5/^v4).(v7/^v3).v4.v3.f
          mapLocal(1)(mapLocal(0)(mapSeq(asScalar o mapSeq(id) o asVector(4)))) |>
          map(transpose) |> join |> map(join) |> transpose // v7.v5.f
        )) |> join |> transpose // v5.M.f
      )) |> join // N.M.f
    ))
  }

  def computeGold(n: Int, m: Int, o: Int,
                  At: Array[Array[Float]],
                  B: Array[Array[Float]]): Array[Array[Float]] = {
    val output = Array.ofDim[Float](n, m)

    for (i <- 0 until n) {
      for (j <- 0 until m) {
        var sum = 0.0f
        for (k <- 0 until o) {
          sum += At(k)(i) * B(k)(j)
        }

        output(i)(j) = sum
      }
    }

    output
  }

  import shine.OpenCL._
  import util._

  def runOriginal(name: String,
                  localSize: LocalSize,
                  globalSize: GlobalSize,
                  At: Array[Array[Float]],
                  B: Array[Array[Float]]): (Array[Float], TimeSpan[Time.ms]) = {
    import opencl.executor._

    val O = At.length
    val N = At(0).length
    val M = B(0).length

    val code = util.readFile(s"src/main/scala/apps/originalLift/$name")
    val kernelJNI = Kernel.create(code, "KERNEL", "")

    val float_bytes = 4
    val output_bytes = N * M * float_bytes
    val g_out = GlobalArg.createOutput(output_bytes)
    val kernelArgs = Array(
      GlobalArg.createInput(At.flatten),
      GlobalArg.createInput(B.flatten),
      g_out,
      ValueArg.create(O), ValueArg.create(N), ValueArg.create(M)
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
                localSize: LocalSize,
                globalSize: GlobalSize,
                At: Array[Array[Float]],
                B: Array[Array[Float]]): (Array[Float], TimeSpan[Time.ms]) = {
    val O = At.length
    val N = At(0).length
    val M = B(0).length

    val run = kernel.as[Args `(`
      Int `,` Int `,` Int `,` Array[Array[Float]] `,` Array[Array[Float]], Array[Float]]
    run(localSize, globalSize)(N `,` M `,` O `,` At `,` B)
  }
}
