package apps

import idealised.OpenCL.{AddressSpace => _, _}
import lift.arithmetic.Cst
import lift.core.HighLevelConstructs.reorderWithStride
import lift.core._
import lift.core.DSL._
import lift.core.primitives._
import lift.OpenCL.primitives._
import lift.core.types._
import util.{Time, TimeSpan, gen}

object sgemm {
  // we can use implicit type parameters and type annotations to specify the function type of mult
  val mult  = implDT(dt => fun(x => x._1 * x._2) :: ((dt x dt) ->: dt))
  val add   = fun(x => fun(y => x + y))
  val scal  = implN(n => fun(xs => fun(a => mapSeq(fun(x => a * x), xs))) :: (ArrayType(n, float) ->: float ->: ArrayType(n, float)))
  val dot = fun(x => foreignFun("dot", float4 ->: float4 ->: float)(x._1, x._2))
  def id: Expr = fun(x => x)

  object c {
    val sequential =
      nFun((n, m, k) =>
        fun((n`.`k`.`float) ->: (k`.`m`.`float) ->: (n`.`m`.`float) ->: float ->: float ->: (n`.`m`.`float))
        ((a, b, c, alpha, beta) =>

          zip(a, c) |> mapSeq(fun(ac =>
            zip(transpose(b), ac._2) |> mapSeq(fun(bc =>
              zip(ac._1, bc._1) |>
                reduceSeq(fun( (acc, y) => acc + (y._1 * y._2)), l(0.0f)) |>
                fun(x => (x * alpha) + (beta * bc._2))
            ))
          ))
        )
      )
  }

  val sequential =
    nFun((n, m, k) =>
      fun((m`.`k`.`float) ->: (k`.`n`.`float) ->: (m`.`n`.`float) ->: float ->: float ->: (m`.`n`.`float))
      ((a, b, c, alpha, beta) =>

        zip(a, c) |> mapSeq(fun(ac =>
          zip(transpose(b), ac._2) |> mapSeq(fun(bc =>
            zip(ac._1, bc._1) |>
              oclReduceSeq(AddressSpace.Private)(fun( (acc, y) => acc + (y._1 * y._2)), l(0.0f)) |>
              fun(x => (x * alpha) + (beta * bc._2))
          ))
        ))
      )
    )

  val mali_GEMM = {
    val p1: Nat = 2
    val p2: Nat = 2
    val p3: Nat = 4
    val vw: Nat = 4

    val write_zeros = implN(n => implN(m =>
      generate(fun(IndexType(m))(_ => generate(fun(IndexType(n))(_ => l(0.0f)))))
      |> mapSeq(mapSeq(id))))


    nFun((n, m, k) =>
      fun((m `.` k `.` float) ->: (n `.` k `.` float) ->: (m `.` n `.` float) ->: float ->: float ->: (m `.` n `.` float))
      /*
       * The matrix B is assumed to be transposed already!
       */
      ((A, B, C, alpha, beta) =>

        zip(split(p2)(A), split(p2)(C)) |>
          mapGlobal(0)(fun(ac =>
            zip(split(p2)(B), split(p1)(transpose(ac._2))) |>
              mapGlobal(1)(fun(bc =>
                zip(split(p3)(transpose(ac._1)), split(p3)(transpose(bc._1))) |>
                  oclReduceSeq(AddressSpace.Private)(fun((p67, p236) =>
                    zip(p67, transpose(p236._1)) |>
                      mapSeq(fun(p54 =>
                        zip(p54._1, transpose(p236._2)) |>
                          mapSeq(fun(p157 =>
                            zip(asVectorAligned(vw)(p54._2), asVectorAligned(vw)(p157._2)) |>
                              mapSeq(fun(x => p157._1 + dot(x)))
                          )) |> join
                      ))
                  ), write_zeros) |>
                  fun(p235 =>
                    zip(p235, transpose(bc._2)) |>
                      mapSeq(fun(p237 =>
                        zip(p237._1, p237._2) |>
                          mapSeq(fun(p64 => (p64._1 * alpha) + (p64._2 * beta)))
                      ))
                  ) |> transpose
              )) |> join |> transpose
          )) |> join
      )
    )
  }

  val keplerBest = {
    val v3: Nat = 128
    val v4: Nat = 4
    val v5: Nat = 8
    val v6: Nat = 64
    val v7: Nat = 8

    def tile: Expr = nFun(s1 => nFun(s2 =>
      map(map(transpose) o split(s2) o transpose) o split(s1) ))

    val zeros = nFun(n1 => nFun(n2 => nFun(n3 => nFun(n4 =>
      generate(fun(IndexType(n4))(_ =>
        generate(fun(IndexType(n3))(_ =>
          generate(fun(IndexType(n2))(_ =>
            generate(fun(IndexType(n1))(_ => l(0.0f)))))))))))))

    def tile2: Expr = nFun(s1 => nFun(s2 => implN(n1 => implN(n2 => fun(ArrayType(n1, ArrayType(n2, float)))(x =>
      transpose (map (transpose) (split (s1) (map (split (s2)) (x))))  )))))

    def redOp: Expr = fun((8`.`32`.`8`.`4`.`float) ->: ( (8`.`64`.`float) x (8`.`128`.`float) ) ->: (8`.`32`.`8`.`4`.`float) )((p14, p15) =>
      p15 |> (fun(p29 =>
        zip (p29._1) (p29._2)
          //TODO? In contrast to old code. This creates a struct of arrays.
          |> toLocalFun(mapLocal(1) (fun(p31 => pair (mapLocal(0) (id) (p31._1)) (mapLocal(0) (id) (p31._2)) )))
          |> unzip
        //TODO think about let much more. Is this even correct? unzip is no wrapped in toLocalFun
      )) |> let(fun(p16 =>
        zip (p14) (split (v5) (transpose (p16._1)))
          |> mapLocal(1) (fun(p17 =>
          zip (p17._1) (split (v4) (reorderWithStride (v3/v4) (transpose (p16._2))))
            |> mapLocal(0) (fun(p18 =>
            zip (transpose (p17._2)) (transpose (p18._2))
              |> oclReduceSeq (AddressSpace.Private) (fun( (p20, p21) =>
              //TODO maybe this subexpression could be much cleaner more efficient and express the same
              // It is possible that the toPrivate around pair leads to less loops together with let. Is this even correct now?
              pair (toPrivate(mapSeq (id) (p21._1))) (toPrivate(mapSeq (id) (p21._2)))
                |> let(fun(p22 =>
                  //TODO something goes horribly wrong in the following part.
                  //Two additional loops are generated and unexpected values are being accessed
                  //It looks like CSE or better Let is needed.
                  zip (p20) (p22._1) |> mapSeq (fun(p23 =>
                    zip (p23._1) (p22._2) |> mapSeq (fun(p24 =>
                      p24._1 + (p23._2 * p24._2) )) )) )))) (p18._1 |> mapSeq (mapSeq (fun(x => x))) )
              |> mapSeq (mapSeq (fun(x => x)))
          ))
        ))
      )))

    nFun((n, m, k) =>
      fun((k`.`m`.`float) ->: (k`.`n`.`float) ->: (m`.`n`.`float) ->: float ->: float ->: (m`.`n`.`float))
      /*
       * The matrix A is probably assumed to be transposed already.
       * Wrong results for input sizes smaller than: M = 64, N = 128, K = 64
       */
      ((A, B, C, alpha, beta) =>
        zip (tile2 (v7) (v6) (A)) (tile (v6) (v3) (C))
        |> mapWorkGroup(1)(
          //BEGIN mapWorkGroup(1) Function
          fun(p2 =>
          zip (tile2 (v7) (v3) (B)) (p2._2)
          |> mapWorkGroup(0)(
            //BEGIN mapWorkGroup(0) Function
            fun(p3 =>
            zip (p2._1) (p3._1)
            |> oclReduceSeq (AddressSpace.Private) (redOp)
              (zeros (v4) (v5) (v3 * Cst(1) /^ v4) (v6 * Cst(1) /^ v5)
                |> mapLocal(1) (mapLocal(0) (mapSeq (mapSeq (id)))))
            //mapSeq was removed because reduce does not wrap reduced results in arrays anymore
            |> fun(x =>
              zip (x) (split (v5) (p3._2))
                //TODO Should write intermediate results into private mem? What exactly does old Lift code mean here?
                |> mapLocal(1) (fun(y =>
                zip (y._1) (split (v4) (reorderWithStride (v3/v4) (transpose (y._2)))) |> mapLocal(0) (fun(z =>
                  zip (z._1) (transpose (z._2)) |> mapSeq (fun(a =>
                    zip (a._1) (a._2) |> mapSeq (fun(x =>
                      (x._1 * alpha) + (x._2 * beta) )))))))))
            |> map (fun(p4 => p4
              |> map (transpose)
              |> join
              |> transpose
              |> map (reorderWithStride (v3 / v4))
            )) |> join |> transpose
          )
          //END mapWorkGroup(0) Function
          ) |> join |> transpose
        )
       //END mapWorkGroup(1) Function
        )  |> join
      ))
  }

  def print2DArray(m: Array[Array[Float]]): Unit = {
    m.foreach( r => {
      println(r.map(x => f"$x%4.0f").reduce(_ + " " + _))
    } )
  }

  def print1DArray(m: Array[Float]): Unit = {
    println(m.map(x => f"$x%4.0f").reduce(_ + " " + _))
  }

  def computeSGEMMGold(A: Array[Array[Float]],
                       B: Array[Array[Float]],
                       C: Array[Array[Float]],
                       alpha: Float,
                       beta: Float) :  Array[Array[Float]] = {
    val aCols = A(0).length
    val aRows = A.length
    val bCols = B(0).length
    val res =  Array.ofDim[Float](aRows, bCols)

    if (A.head.length != B.length)
      throw new IllegalArgumentException

    @inline def computeRow(row: Int): Unit = {
      // while statements are much faster than for statements
      var col = 0
      while(col < bCols) { var i = 0; var sum = 0.0f
        while(i < aCols) {
          sum += A(row)(i) * B(i)(col)
          i += 1
        }

        res(row)(col) =  alpha * sum + C(row)(col) * beta
        col += 1
      }
    }

    (0 until aRows).par.foreach( computeRow )

    res
  }

  def runOriginalSgemm(fileName: String,
                       localSize: LocalSize,
                       globalSize: GlobalSize,
                       A: Array[Array[Float]],
                       B: Array[Array[Float]],
                       C: Array[Array[Float]],
                       alpha: Float,
                       beta: Float,
                       M: Int, N: Int, K: Int,
                       isMaliKernel: Boolean = false): (Array[Float], TimeSpan[Time.ms]) = {
    import opencl.executor._

    val code = util.readFile(s"src/main/scala/apps/originalLift/$fileName")
    val kernelJNI = Kernel.create(code, "KERNEL", "")

    val float_bytes = 4
    val output_bytes = N * M * float_bytes
    val g_out = GlobalArg.createOutput(output_bytes)

    val tmpBuff1 = GlobalArg.createOutput(output_bytes)
    val tmpBuff2 = GlobalArg.createOutput(output_bytes)

    val kernelArgs =
      if (!isMaliKernel)
        Array(
          GlobalArg.createInput(A.flatten),
          GlobalArg.createInput(B.flatten),
          GlobalArg.createInput(C.flatten),
          ValueArg.create(alpha),
          ValueArg.create(beta),
          g_out,
          ValueArg.create(K), ValueArg.create(M), ValueArg.create(N))
    else
        Array(
          GlobalArg.createInput(A.flatten),
          GlobalArg.createInput(B.flatten),
          GlobalArg.createInput(C.flatten),
          ValueArg.create(alpha),
          ValueArg.create(beta),
          g_out,
          tmpBuff1,
          tmpBuff2,
          ValueArg.create(K), ValueArg.create(M), ValueArg.create(N))

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

  def runSgemmKernel(kernel: KernelWithSizes,
                     A: Array[Array[Float]],
                     B: Array[Array[Float]],
                     C: Array[Array[Float]],
                     alpha: Float, beta: Float,
                     M: Int, N: Int, K: Int): (Array[Float], TimeSpan[Time.ms]) = {

    val runKernel = kernel.as[ScalaFunction `(`
      Int `,` Int `,` Int `,`
      Array[Array[Float]] `,`
      Array[Array[Float]] `,`
      Array[Array[Float]] `,`
      Float `,`
      Float `)=>` Array[Float]]

    runKernel(N `,` M `,` K `,` A `,` B `,` C `,` alpha `,` beta)
  }
}
