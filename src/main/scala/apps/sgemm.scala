package apps

import shine.OpenCL.{AddressSpace => _, _}
import arithexpr.arithmetic.Cst
import rise.core.DSL.HighLevelConstructs.reorderWithStride
import rise.core._
import rise.core.DSL._
import rise.core.primitives.{let => _, _}
import rise.core.DSL.Type._
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeq
import rise.core.types._
import rise.core.types.DataType._
import util.{Time, TimeSpan}
import scala.collection.parallel.CollectionConverters._

object sgemm {
  // we can use implicit type parameters and type annotations to specify the function type of mult
  val mult: ToBeTyped[Expr] = impl{ dt: DataType => fun(x => x.`1` * x.`2`) :: ((dt x dt) ->: dt) }
  val add: ToBeTyped[Expr] = fun(x => fun(y => x + y))
  val scal: ToBeTyped[Expr] = impl{ n: Nat =>
    fun(xs => fun(a => mapSeq(fun(x => a * x))(xs))) :: (ArrayType(n, f32) ->: f32 ->: ArrayType(n, f32)) }
  val dot: ToBeTyped[Expr] = fun(x => foreignFun("dot", vec(4, f32) ->: vec(4, f32) ->: f32)(x.`1`, x.`2`))
  def id: ToBeTyped[Expr] = fun(x => x)

  object c {
    val sequential: ToBeTyped[Expr] =
      depFun((n: Nat, m: Nat, k: Nat) =>
        fun((n`.`k`.`f32) ->: (k`.`m`.`f32) ->: (n`.`m`.`f32) ->: f32 ->: f32 ->: (n`.`m`.`f32))
        ((a, b, c, alpha, beta) =>

          zip(a)(c) |> mapSeq(fun(ac =>
            zip(transpose(b))(ac.`2`) |> mapSeq(fun(bc =>
              zip(ac.`1`)(bc.`1`) |>
                reduceSeq(fun( (acc, y) => acc + (y.`1` * y.`2`)))(lf32(0.0f)) |>
                fun(x => (x * alpha) + (beta * bc.`2`))
            ))
          ))
        )
      )
  }

  val sequential: ToBeTyped[Expr] =
    depFun((n: Nat, m: Nat, k: Nat) =>
      fun((m`.`k`.`f32) ->: (k`.`n`.`f32) ->: (m`.`n`.`f32) ->: f32 ->: f32 ->: (m`.`n`.`f32))
      ((a, b, c, alpha, beta) =>

        zip(a)(c) |> mapSeq(fun(ac =>
          zip(transpose(b))(ac.`2`) |> mapSeq(fun(bc =>
            zip(ac.`1`)(bc.`1`) |>
              oclReduceSeq(AddressSpace.Private)(fun( (acc, y) => acc + (y.`1` * y.`2`)))(lf32(0.0f)) |>
              fun(x => (x * alpha) + (beta * bc.`2`))
          ))
        ))
      )
    )

  val mali_GEMM: ToBeTyped[Expr] = {
    val p1: Nat = 2
    val p2: Nat = 2
    val p3: Nat = 4
    val vw: Nat = 4

    val write_zeros = impl{ n: Nat => impl{ m: Nat =>
      generate(fun(IndexType(m))(_ =>
        generate(fun(IndexType(n))(_ => lf32(0.0f))))) |> mapSeq(mapSeq(id)) }}


    depFun((n: Nat, m: Nat, k: Nat) =>
      fun((m `.` k `.` f32) ->: (n `.` k `.` f32) ->: (m `.` n `.` f32) ->: f32 ->: f32 ->: (m `.` n `.` f32))
      /*
       * The matrix B is assumed to be transposed already!
       */
      ((A, B, C, alpha, beta) =>

        zip(split(p2)(A))(split(p2)(C)) |>
          mapGlobal(0)(fun(ac =>
            zip(split(p2)(B))(split(p1)(transpose(ac.`2`))) |>
              mapGlobal(1)(fun(bc =>
                zip(split(p3)(transpose(ac.`1`)))(split(p3)(transpose(bc.`1`))) |>
                  oclReduceSeq(AddressSpace.Private)(fun((p67, p236) =>
                    zip(p67)(transpose(p236.`1`)) |>
                      mapSeq(fun(p54 =>
                        zip(p54.`1`)(transpose(p236.`2`)) |>
                          mapSeq(fun(p157 =>
                            zip(asVectorAligned(vw)(p54.`2`))(asVectorAligned(vw)(p157.`2`)) |>
                              mapSeq(fun(x => p157.`1` + dot(x)))
                          )) |> join
                      ))
                  ), write_zeros) |>
                  fun(p235 =>
                    zip(p235)(transpose(bc.`2`)) |>
                      mapSeq(fun(p237 =>
                        zip(p237.`1`)(p237.`2`) |>
                          mapSeq(fun(p64 => (p64.`1` * alpha) + (p64.`2` * beta)))
                      ))
                  ) |> transpose
              )) |> join |> transpose
          )) |> join
      )
    )
  }

  val keplerBest: ToBeTyped[Expr] = {
    val v3: Nat = 128
    val v4: Nat = 4
    val v5: Nat = 8
    val v6: Nat = 64
    val v7: Nat = 8

    def tile: ToBeTyped[Expr] = depFun((s1: Nat, s2: Nat) =>
      map(map(transpose) o split(s2) o transpose) o split(s1) )

    val zeros = depFun((n1: Nat, n2: Nat, n3: Nat, n4: Nat) =>
      generate(fun(IndexType(n4))(_ =>
        generate(fun(IndexType(n3))(_ =>
          generate(fun(IndexType(n2))(_ =>
            generate(fun(IndexType(n1))(_ => lf32(0.0f))))))))))

    def tile2: ToBeTyped[Expr] = depFun((s1: Nat, s2: Nat) => impl{ n1: Nat => impl{ n2: Nat => fun(ArrayType(n1, ArrayType(n2, f32)))(x =>
      transpose (map (transpose) (split (s1) (map (split (s2)) (x))))  ) }})

    def redOp: ToBeTyped[Expr] = fun((8`.`32`.`8`.`4`.`f32) ->: ( (8`.`64`.`f32) x (8`.`128`.`f32) ) ->: (8`.`32`.`8`.`4`.`f32) )((p14, p15) =>
      let(p15 |> fun(p29 =>
          zip (p29.`1`) (p29.`2`)
            |> toLocalFun(mapLocal(1) (fun(p31 => makePair (mapLocal(0) (id) (p31.`1`)) (mapLocal(0) (id) (p31.`2`)) )))
            |> unzip
        ))
      be (p16 =>
        zip (p14) (split (v5) (transpose (p16.`1`)))
          |> mapLocal(1) (fun(p17 =>
          zip (p17.`1`) (split (v4) (reorderWithStride (v3/v4) (transpose (p16.`2`))))
            |> mapLocal(0) (fun(p18 =>
            zip (transpose (p17.`2`)) (transpose (p18.`2`))
              |> oclReduceSeq (AddressSpace.Private) (fun( (p20, p21) =>
                let (makePair (toPrivate(mapSeq (id) (p21.`1`))) (toPrivate(mapSeq (id) (p21.`2`))))
                be (fun(p22 =>
                    zip (p20) (p22.`1`) |> mapSeq (fun(p23 =>
                      zip (p23.`1`) (p22.`2`) |> mapSeq (fun(p24 =>
                        p24.`1` + (p23.`2` * p24.`2`) )) )) ))
              )) (p18.`1` |> mapSeq (mapSeq (fun(x => x))) )
              |> mapSeq (mapSeq (fun(x => x)))
          ))
        ))
      ))

    depFun((n: Nat, m: Nat, k: Nat) =>
      fun((k`.`m`.`f32) ->: (k`.`n`.`f32) ->: (m`.`n`.`f32) ->: f32 ->: f32 ->: (m`.`n`.`f32))
      /*
       * The matrix A is assumed to be transposed already.
       */
      ((A, B, C, alpha, beta) =>
        zip (tile2 (v7) (v6) (A)) (tile (v6) (v3) (C))
        |> mapWorkGroup(1)(
          //BEGIN mapWorkGroup(1) Function
          fun(p2 =>
          zip (tile2 (v7) (v3) (B)) (p2.`2`)
          |> mapWorkGroup(0)(
            //BEGIN mapWorkGroup(0) Function
            fun(p3 =>
            zip (p2.`1`) (p3.`1`)
            |> oclReduceSeq (AddressSpace.Private) (redOp)
              (zeros (v4) (v5) (v3 * Cst(1) /^ v4) (v6 * Cst(1) /^ v5)
                |> mapLocal(1) (mapLocal(0) (mapSeq (mapSeq (id)))))
            //mapSeq was removed because reduce does not wrap reduced results in arrays anymore
            |> fun(x =>
              zip (x) (split (v5) (p3.`2`))
                |> mapLocal(1) (fun(y =>
                zip (y.`1`) (split (v4) (reorderWithStride (v3/v4) (transpose (y.`2`)))) |> mapLocal(0) (fun(z =>
                  zip (z.`1`) (transpose (z.`2`)) |> mapSeq (fun(a =>
                    zip (a.`1`) (a.`2`) |> mapSeq (fun(x =>
                      (x.`1` * alpha) + (x.`2` * beta) )))))))))
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

  def runSgemmKernel(kernel: KernelExecutor.KernelWithSizes,
                     A: Array[Array[Float]],
                     B: Array[Array[Float]],
                     C: Array[Array[Float]],
                     alpha: Float, beta: Float,
                     M: Int, N: Int, K: Int): (Array[Float], TimeSpan[Time.ms]) = {

    val runKernel = kernel.as[In `=`
      Int `,` Int `,` Int `,`
      Array[Array[Float]] `,`
      Array[Array[Float]] `,`
      Array[Array[Float]] `,`
      Float `,`
      Float, Out[Array[Float]]]

    runKernel(N `,` M `,` K `,` A `,` B `,` C `,` alpha `,` beta)
  }
}
