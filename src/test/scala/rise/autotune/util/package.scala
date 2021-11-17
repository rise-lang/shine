package rise.autotune

import apps.mm.mmNVIDIAWithParams
import apps.separableConvolution2D.weightsSeqVecUnroll
import arithexpr.arithmetic._
import rise.core.DSL.HighLevelConstructs.{slideVectors, tileShiftInwards}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.primitives.{let => _, _}
import rise.core.types.DataType._
import rise.core.types.{NatIdentifier, _}
import rise.openCL.DSL._
import shine.OpenCL.{GlobalSize, LocalSize}

package object util {

  object expressions {

    object convolution {

      val convolution: ToBeTyped[Expr] =
      // tileShiftInwards should constrain n >= tile
      // slideVectors and slide should constrain tile % vec = 0
        tuningParam("vec", RangeAdd(1, 32, 1), (vec: Nat) =>
          tuningParam("tile", RangeAdd(4, 32, 1), (tile: Nat) =>
            depFun(RangeAdd(1, PosInf, vec), (n: Nat) =>
              fun(3 `.` f32)(weights =>
                fun(((n + 2) `.` f32) ->: (n `.` f32))(input =>
                  input |> tileShiftInwards(tile)(mapWorkGroup(0)(
                    slideVectors(vec) >> slide(3)(vec) >>
                      mapLocal(0)(weightsSeqVecUnroll(weights)) >>
                      asScalar
                  ))
                )))))

      val convolutionOcl: ToBeTyped[Expr] =
      // tileShiftInwards should constrain n >= tile
      // slideVectors and slide should constrain tile % vec = 0
        tuningParam("vec", RangeAdd(1, 32, 1), (vec: Nat) =>
          tuningParam("tile", RangeAdd(4, 32, 1), (tile: Nat) =>
            depFun(RangeAdd(1, PosInf, vec), (n: Nat) =>
              fun(3 `.` f32)(weights =>
                fun(((n + 2) `.` f32) ->: (n `.` f32))(input =>
                  oclRun(LocalSize(1), GlobalSize(32))(
                    input |> tileShiftInwards(tile)(mapWorkGroup(0)(
                      slideVectors(vec) >> slide(3)(vec) >>
                        mapLocal(0)(weightsSeqVecUnroll(weights)) >>
                        asScalar
                    ))
                  ))))))

      val convolutionOclGsLsWrap: Expr =
        tuningParam("ls0", (ls0: Nat) => tuningParam("ls1", (ls1: Nat) =>
          tuningParam("gs0", (gs0: Nat) => tuningParam("gs1", (gs1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(convolution)
          ))))

      val convolutionOclGsLs: ToBeTyped[Expr] = {
        // tileShiftInwards should constrain n >= tile
        // slideVectors and slide should constrain tile % vec = 0
        tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
          tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
            tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
              tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
                tuningParam("vec", RangeAdd(1, 32, 1), (vec: Nat) =>
                  tuningParam("tile", RangeAdd(4, 1024, 1), (tile: Nat) =>
                    depFun(RangeAdd(1, PosInf, vec), (n: Nat) =>
                      fun(3 `.` f32)(weights =>
                        fun(((n + 2) `.` f32) ->: (n `.` f32))(input =>
                          oclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(
                            input |> tileShiftInwards(tile)(mapWorkGroup(0)(
                              slideVectors(vec) >> slide(3)(vec) >>
                                mapLocal(0)(weightsSeqVecUnroll(weights)) >>
                                asScalar
                            ))
                          ))))))))))
      }
    }

    object scal {

      val scalSJ: ToBeTyped[Expr] =
        depFun((n: Nat) => fun(n `.` f32)(input => fun(f32)(alpha =>
          oclRun(LocalSize(1), GlobalSize(32))(
            input |> split(4) |> mapGlobal(0)(fun(x => alpha * x)) |> join)
        )))

      val scalOcl: ToBeTyped[Expr] =
        depFun((n: Nat) => fun(n `.` f32)(input => fun(f32)(alpha =>
          oclRun(LocalSize(1), GlobalSize(32))(
            input |> mapGlobal(0)(fun(x => alpha * x)))
        )))

      val scal: ToBeTyped[Expr] =
        depFun((n: Nat) => fun(n `.` f32)(input => fun(f32)(alpha =>
          input |> mapGlobal(0)(fun(x => alpha * x)))
        ))
    }

    object mm {

      val mmKernel: ToBeTyped[Expr] =
        tuningParam("v3", RangeAdd(1,1024,1), (v3: Nat) =>
          tuningParam("v4", RangeAdd(1,1024,1), (v4: Nat) =>
            tuningParam("v5", RangeAdd(1,1024,1), (v5: Nat) =>
              tuningParam("v6", RangeAdd(1,1024,1), (v6: Nat) =>
                tuningParam("v7", RangeAdd(1,1024,1), (v7: Nat) =>
                  tuningParam("v8", RangeAdd(1,1024,1), (v8: Nat) =>
                    mmNVIDIAWithParams(v3, v4, v5, v6, v7, v8)
                  ))))))

    val mmOclGsLsWrap: ToBeTyped[Expr] =
      tuningParam("ls0", (ls0: Nat) =>
        tuningParam("ls1", (ls1: Nat) =>
          tuningParam("gs0", (gs0: Nat) =>
            tuningParam("gs1", (gs1: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(mmKernel)
            ))))

    val mmOclGsLsWrapRanges: ToBeTyped[Expr] =
      tuningParam("ls0", RangeAdd(1, 1024, 1), (ls0: Nat) =>
        tuningParam("ls1", RangeAdd(1, 1024, 1), (ls1: Nat) =>
          tuningParam("gs0", RangeAdd(1, 1024, 1), (gs0: Nat) =>
            tuningParam("gs1", RangeAdd(1, 1024, 1), (gs1: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(mmKernel)
            ))))
    }
  }

  object hostcode {

    val convolution: Int => HostCode = N => {

      // scalastyle:off
      val init: Int => String = (N) => {
        s"""
           |const int N = ${N};
           |
           |Buffer input = createBuffer(ctx, N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
           |Buffer output = createBuffer(ctx, N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);
           |
           |float* in = hostBufferSync(ctx, input, N * sizeof(float), HOST_WRITE);
           |for (int i = 0; i < N; i++) {
           |  in[i] = 1;
           |}
           |
           |""".stripMargin
      }

      val compute =
        s"""
           |fun_run(ctx, &fun, output, N, input, input);
           |""".stripMargin

      val finish =
        s"""
           |// TODO: could check output here
           |
           |destroyBuffer(ctx, input);
           |destroyBuffer(ctx, output);
           |""".stripMargin
      // scalastyle:on

      HostCode(init(N), compute, finish)
    }

    val scal: Int => HostCode = N => {

      // scalastyle:off
      val init: Int => String = (N) => {
        s"""
           |const int N = ${N};
           |
           |Buffer input = createBuffer(ctx, N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
           |Buffer output = createBuffer(ctx, N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);
           |
           |float* in = hostBufferSync(ctx, input, N * sizeof(float), HOST_WRITE);
           |for (int i = 0; i < N; i++) {
           |  in[i] = 1;
           |}
           |
           |// synchronize before entering timed section
           |deviceBufferSync(ctx, input, N * sizeof(float), DEVICE_READ);
           |
           |""".stripMargin
      }

      val compute =
        s"""
           |fun_run(ctx, &fun, output, input, 4);
           |""".stripMargin

      val finish =
        s"""
           |  float* out = hostBufferSync(ctx, output, N * sizeof(float), HOST_READ);
           |  for (int i = 0; i < N; i++) {
           |    if (out[i] != 4) {
           |       exit(EXIT_FAILURE);
           |    }
           |  }
           |
           |destroyBuffer(ctx, input);
           |destroyBuffer(ctx, output);
           |""".stripMargin
      // scalastyle:on

      HostCode(init(N), compute, finish)
    }

    val mm: (Int, Int, Int) => HostCode = (N, M, O) => {

      // scalastyle:off
      val init: (Int, Int, Int) => String = (N, M, O) => {
        s"""
           |const int N = ${N};
           |const int M = ${M};
           |const int O = ${O};
           |
           |srand(time(NULL));
           |
           |Buffer inputA = createBuffer(ctx, N * M * sizeof(float), HOST_WRITE | DEVICE_READ);
           |Buffer inputB = createBuffer(ctx, M * O * sizeof(float), HOST_WRITE | DEVICE_READ);
           |Buffer outputC = createBuffer(ctx, N * O *  sizeof(float), HOST_READ | DEVICE_WRITE);
           |
           |float* inA = hostBufferSync(ctx, inputA, N * M * sizeof(float), HOST_WRITE);
           |for (int i = 0; i < N * M ; i++) {
           |  inA[i] = (float)(rand());
           |}
           |
           |float* inB = hostBufferSync(ctx, inputB, M * O * sizeof(float), HOST_WRITE);
           |for (int i = 0; i < M * O; i++) {
           |  inB[i] = (float)(rand());
           |}
           |
           |""".stripMargin
      }

      val compute =
        s"""
           |fun_run(ctx, &fun, outputC, M, N, O, inputA, inputB);
           |""".stripMargin

      val finish =
        s"""
           |// TODO: could check output here
           |
           |destroyBuffer(ctx, inputA);
           |destroyBuffer(ctx, inputB);
           |destroyBuffer(ctx, outputC);
           |""".stripMargin
      // scalastyle:on

      HostCode(init(N, M, O), compute, finish)
    }
  }
}
