import apps.mm.mmAMDWithParamsHighLevel
import rise.core.DSL.{fun, lf32}
import rise.core.primitives._
import rise.core.types.DataType.{ArrayType, DataTypeIdentifier, f32}
import rise.elevate.rules.lowering.lowerToC
import rise.elevate.strategies.traversal.everywhere
import rise.elevate.rules.algorithmic.fuseReduceMap
import elevate.core._
import elevate.heuristic_search.util.{Solution, hashSolution}
import rise.autotune.HostCode
import rise.core.equality.{exprAlphaEq, typeAlphaEq, typeErasure}
import rise.core.{App, DepApp, DepLambda, Expr, Identifier, Lambda, Literal, Opaque, Primitive, TypeAnnotation, TypeAssertion}
import rise.elevate.Rise
import rise.elevate.rules.traversal.alternative
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.normalForm.DFNF
import rise.elevate.strategies.tiling.tile
import rise.elevate.strategies.traversal._

import java.io.{File, FileOutputStream, PrintWriter}
import java.io.{File, FileInputStream, FileReader}
import rise.core.DSL.ToBeTyped
import rise.core.Expr

import scala.reflect.runtime._
import scala.tools.reflect.ToolBox
import apps.tvmGemm
import apps.tvmGemm.{innermost, outermost}
import exploration.runner.DebugExecutor
import exploration.strategies.blockingExploration
import rise.autotune.{AutoTuningError, EXECUTION_ERROR, HostCode, Timeouts}
import rise.core.Expr
import rise.core.types.{Nat, TuningParameter}
import util.{assertSame, gen}

import scala.collection.immutable
import scala.collection.immutable.Map
//import exploration.{ExecutorConfig, MetaheuristicConfig, runner, uniqueFilename}
import elevate.heuristic_search.ExplorationResult
import exploration._
import explorations.explorationTutorial.mm
import rise.elevate.strategies.normalForm.DFNF
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.heuristic_search.util._
import elevate.macros.RuleMacro.rule
import exploration.neighborhoods._
import exploration.runner.{CExecutor, checkExpressionC}
import rise.core.DSL.{fun, lf32}
import rise.core.primitives.{add, fst, map, reduce, snd, transpose, zip}
import rise.core.types.DataType.{ArrayType, f32}
import rise.elevate.{NormalizedThen, Rise, tunable}
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.algorithmic.reorder
import rise.elevate.strategies.algorithmic.reorder2
import rise.elevate.strategies.lowering._
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.predicate._
import rise.elevate.strategies.tiling._
import rise.elevate.strategies.traversal
import rise.elevate.strategies.traversal._
import rise.autotune.ExecutionResult
import rise.core.DSL._
import rise.core.DSL.HighLevelConstructs.reorderWithStride
import rise.core.types.AddressSpace._
import rise.core.types.DataType._
//import rise.core.types._


import shine.DPIA.Types.ExpType
import rise.elevate.Rise
import shine.OpenCL.{GlobalSize, LocalSize}
import rise.core._
import rise.core.types._
import rise.core.DSL._
import rise.core.primitives._
import Type._
import HighLevelConstructs.reorderWithStride
import rise.autotune
import rise.autotune.{HostCode, Median, Timeouts, tuningParam, wrapOclRun}
import util.{SyntaxChecker, gen}
import rise.elevate.rules.traversal.default._
import rise.openCL.DSL.{mapGlobal, mapLocal, mapWorkGroup, toGlobal, toLocalFun}
import rise.openCL.primitives.{oclIterate, oclReduceSeq}
import shine.OpenCL.KernelExecutor.KernelNoSizes.fromKernelModule
import util.gen.c.function
import apps.separableConvolution2D.mulT
import rise.core.DSL.HighLevelConstructs.reorderWithStride
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.primitives.{let => _, _}
import rise.core.types.DataType._
import rise.core.types._

import scala.util.Random
import apps.mv.ocl._
import arithexpr.arithmetic.RangeMul
import rise.autotune
import rise.autotune._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.types._
import shine.OpenCL.{GlobalSize, LocalSize}
import apps.mm.mmNVIDIAWithParams
import arithexpr.arithmetic.{RangeAdd, RangeMul}
import elevate.core.strategies.traversal.topDown
import rise.elevate.Rise
import rise.elevate.rules.algorithmic.fuseReduceMap
import rise.elevate.strategies.traversal._

import scala.collection.immutable
import scala.language.postfixOps
import exploration.runner.CExecutor
import exploration.strategies.simpleStrategiesGPU
import rise.core.types.{Nat, TuningParameter}
import rise.elevate.rules.lowering.reduceOCL

import apps.mm.mmAMDWithParams
import rise.autotune._

import arithexpr.arithmetic.{RangeAdd, RangeMul}

package object explorations {

  object scal {

    val inputSize = 1 << 25
    //    val inputSize = 1 << 20

    val expression: Expr =
      depFun((n: Nat) => fun(ArrayType(n, f32))(input => fun(f32)(alpha =>
        input |> map(fun(x => alpha * x))
      )))

    val expert: Expr = depFun((n: Nat) => fun(ArrayType(n, f32))(input => fun(f32)(alpha =>
      input |>
        split(1024) |>
        mapWorkGroup(
          split(32) >>
            mapLocal(rise.core.primitives.mapSeqUnroll(fun(x => alpha * x))) >>
            join
        ) |> join
    )))

    // hostcode
    val init: Int => String = N => {
      s"""
         |const int N = ${N};
         |
         |srand(time(NULL));
         |
         |Buffer input = createBuffer(ctx, N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
         |Buffer output = createBuffer(ctx, N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);
         |
         |float* m = hostBufferSync(ctx, input, N * sizeof(float), HOST_WRITE);
         |for (int i = 0; i < N; i++) {
         |  // m[i] = (float)(rand()) * 10.0f;
         |  m[i] = 1.0f;
         |}
         |
         |int alpha = 10;
         |
         |// synchronize before entering timed section
         |deviceBufferSync(ctx, input, N * sizeof(float), DEVICE_READ);
         |waitFinished(ctx);
         |""".stripMargin
    }

    val compute =
      s"""
         |fun_run(ctx, &fun, output, N, input, alpha);
         |waitFinished(ctx);
         |""".stripMargin

    val finish =
      s"""
         |
         |float* outputScal = hostBufferSync(ctx, output, N * sizeof(float), HOST_READ);
         |for(int i = 0; i < N; i++){
         |  if(outputScal[i] != 10){
         |    return 1;
         |  }
         |}
         |
         |destroyBuffer(ctx, input);
         |destroyBuffer(ctx, output);
         |""".stripMargin


    val hostCode = HostCode(
      init = init(inputSize),
      compute = compute,
      finish = finish,
    )

  }


  object kmeans {
    val inputSize: Int = 1 << 19

    val p = inputSize
    val c = 10
    val f = 34


    val expression: Expr = apps.kmeans.kmeansHighLevel

    val expert: Expr = apps.kmeans.kmeansOcl

    // scalastyle:off
    val init: (Int, Int, Int) => String = (p, c, f) => {
      s"""
         |
         |#include <math.h>
         |#include <stdlib.h>
         |#include <stdio.h>
         |
         |  const int P = ${p};
         |  const int C = ${c};
         |  const int F = ${f};
         |  srand(time(NULL));
         |  Buffer features = createBuffer(ctx, F * P * sizeof(float), HOST_WRITE | DEVICE_READ);
         |  Buffer clusters = createBuffer(ctx, C * F * sizeof(float), HOST_WRITE | DEVICE_READ);
         |  Buffer output = createBuffer(ctx, P * sizeof(float), HOST_READ | DEVICE_WRITE);
         |
         |  float* in_features = hostBufferSync(ctx, features, F * P * sizeof(float), HOST_WRITE);
         |  for (int i = 0; i < F * P ; i++) {
         |      // in_features[i] = (float)(rand() % 100);
         |      in_features[i] = (float)(i+1);
         |  }
         |
         |  float* in_clusters = hostBufferSync(ctx, clusters, C * F * sizeof(float), HOST_WRITE);
         |    for (int i = 0; i < F * P ; i++) {
         |      // in_clusters[i] = (float)(rand() % 100);
         |        in_features[i] = (float)(i+1);
         |    }
         |
         |  deviceBufferSync(ctx, features, F * P * sizeof(float), DEVICE_READ);
         |  deviceBufferSync(ctx, clusters, C * F * sizeof(float), DEVICE_READ);
         |
         |// init checking
         |  FILE *fptr;
         |  if ((fptr = fopen("kmeans_${p}_${c}_${f}.csv","r")) == NULL){
         |    return 133;
         |  }
         |  float* gold = (float*)malloc(P * sizeof(float));
         |  for(int i = 0; i<P; i++){
         |    fscanf(fptr, "%f,", &gold[i]);
         |   }
         |  fclose(fptr);
         |
         |""".stripMargin
    }
    val compute =
      s"""
         |    fun_init_run(ctx, output, P, C, F, features, clusters);
         |
         |    float* out = hostBufferSync(ctx, output, P * sizeof(float), HOST_READ);
         |    for(int i = 0; i < P; i++){
         |      if(fabsf(out[i] - gold[i]) > 0.001){
         |            return 132;
         |        }
         |    }
         |
         |""".stripMargin


    val compute_gold_finish =
      s"""
         |        float* out = hostBufferSync(ctx, output, P * sizeof(float), HOST_READ);
         |
         |         FILE *fptr;
         |         if ((fptr = fopen("kmeans_${p}_${c}_${f}.csv","w")) == NULL){
         |                 return 133;
         |            }
         |
         |             for(int i = 0; i < P - 1; i++){
         |               fprintf(fptr, "%f,", out[i]);
         |             }
         |
         |             fprintf(fptr, "%f", out[P-1]);
         |
         |  fclose(fptr);
         |
      """.stripMargin

    val finish =
      s"""
         |  destroyBuffer(ctx, features);
         |  destroyBuffer(ctx, clusters);
         |  destroyBuffer(ctx, output);
         |""".stripMargin
    // scalastyle:on

    val hostCode = HostCode(
      init = init(p, c, f),
      compute = compute,
      finish = finish,
    )

    val hostCode_gold = HostCode(
      init = init(p, c, f),
      compute = compute_gold_finish,
      finish = finish,
    )

    def compute_gold(): Unit = {

      val expression = wrapOclRun(localSize = LocalSize(256), globalSize = GlobalSize(p))(apps.kmeans.kmeansOcl)

      rise.autotune.execution.execute(expression = expression,
        hostCode = hostCode,
        timeouts = Timeouts(codegenerationTimeout = 10000, compilationTimeout = 10000, executionTimeout = 10000),
        executionIterations = 10,
        speedupFactor = 10.0,
        execution = rise.autotune.Median
      )
    }
  }

  object asum {

    def inputT(n: Nat) = ArrayType(n, f32)

    val abs =
      depFun((t: DataType) => foreignFun("my_abs", scala.collection.immutable.Seq("y"), "{ return fabs(y); }", t ->: t))
    val fabs = abs(f32)
    val add = fun(x => fun(a => x + a))

    //    val high_level = depFun((n: Nat) =>
    //      fun(inputT(n))(input => input |> map(fabs) |> reduceSeq(add)(lf32(0.0f)))
    //    )


    val inputSize: Int = 2 << 24

    val expert = depFun((n: Nat) =>
      fun(inputT(n))(input =>
        input |>
          split(2048 * 128) |>
          mapWorkGroup(
            reorderWithStride(128) >>
              split(2048) >>
              mapLocal(
                oclReduceSeq(AddressSpace.Private)(
                  fun(a => fun(x => abs(f32)(x) + a))
                )(lf32(0.0f))
              )
          ) |> join
      )
    )

    // don't fuse map and reduce
    val asum_optimized: ToBeTyped[Expr] = {
      tuningParam("sp0", RangeMul(1, inputSize, 2), (sp0: Nat) =>
        tuningParam("sp1", RangeMul(1, inputSize, 2), (sp1: Nat) =>
          tuningParam("stride", RangeMul(1, inputSize, 2), (stride: Nat) =>
            depFun((n: Nat) =>
              fun(inputT(n))(input =>
                input |>
                  split(sp0) |>
                  map(
                    reorderWithStride(stride) >>
                      split(sp1) >>

                      // is this 2-Dimensional?
                      map(
                        reduce(
                          fun(a => fun(x => abs(f32)(x) + a))
                        )(lf32(0.0f))
                      )
                  ) |> join
              )
            ))))
    }


    // kind of high-level
    val nvidiaDerived1: ToBeTyped[Expr] = {
      tuningParam("sp0", RangeMul(1, inputSize, 2), (sp0: Nat) =>
        tuningParam("sp1", RangeMul(1, inputSize, 2), (sp1: Nat) =>
          tuningParam("stride", RangeMul(1, inputSize, 2), (stride: Nat) =>
            depFun((n: Nat) =>
              fun(inputT(n))(input =>
                input |>
                  split(sp0) |>
                  map(
                    reorderWithStride(stride) >>
                      split(sp1) >>
                      map(
                        reduce(
                          fun(a => fun(x => abs(f32)(x) + a))
                        )(lf32(0.0f))
                      )
                  ) |> join
              )
            ))))
    }


    // scalastyle:off
    val init: (Int) => String = (N) => {
      s"""
         |  const int N = ${N};
         |  srand(time(NULL));
         |  Buffer input = createBuffer(ctx, N * sizeof(float), HOST_WRITE | DEVICE_READ);
         |  Buffer output = createBuffer(ctx, 1 * sizeof(float), HOST_READ | DEVICE_WRITE);
         |
         |  float* in = hostBufferSync(ctx, input, N * sizeof(float), HOST_WRITE);
         |  for (int i = 0; i < N ; i++) {
         |    in[i] = (float)(1);
         |  }
         |
         |  deviceBufferSync(ctx, input, N * sizeof(float), DEVICE_READ);
         |""".stripMargin
    }
    val compute =
      s"""
         |    fun_init_run(ctx, output, N, input);
         |""".stripMargin

    val finish =
      s"""
         |  // could add error checking
         | // deviceBufferSync(ctx, output, N * sizeof(float), HOST_READ | DEVICE_WRITE);
         |  //float* out = hostBufferSync(ctx, output, N * sizeof(float), HOST_READ | DEVICE_WRITE);
         |  //printf("N: %d \\n", N);
         |  //for (int i = 0; i < N ; i++) {
         |  //printf("in: %f \\n", in[i]);
         |  //printf("out: %f \\n", out[i]);
         | //}
         |
         |
         |
         |  destroyBuffer(ctx, input);
         |  destroyBuffer(ctx, output);
         |""".stripMargin
    // scalastyle:on

    val hostCode = HostCode(
      init = init(inputSize),
      compute = compute,
      finish = finish,
    )

  }

  object mm {

    val N: Int = 1024
    //    val N: Int = 128
    //
    val expressionAMD: ToBeTyped[Expr] =
      tuningParam("v3", RangeAdd(1, 1024, 1), default = 4, (v3: Nat) =>
        tuningParam("v4", RangeAdd(1, 1024, 1), default = 8, (v4: Nat) =>
          tuningParam("vw", RangeAdd(1, 1024, 1), default = 4, (vw: Nat) =>
            mmAMDWithParamsHighLevel(v3: Nat, v4: Nat, vw: Nat)
          )))


    val expression: Rise = //infer(
      fun(ArrayType(N, ArrayType(N, f32)))(a =>
        fun(ArrayType(N, ArrayType(N, f32)))(b =>
          a |> map(fun(ak =>
            transpose(b) |> map(fun(bk =>
              zip(ak)(bk) |>
                map(fun(x => fst(x) * snd(x))) |>
                reduce(add)(lf32(0.0f))
            ))
          ))
        ))


    val expert: Rise = //infer(
      fun(ArrayType(N, ArrayType(N, f32)))(a =>
        fun(ArrayType(N, ArrayType(N, f32)))(b =>
          a |> mapGlobal(0)(fun(ak =>
            transpose(b) |> map(fun(bk =>
              zip(ak)(bk) |>
                mapGlobal(1)(fun(x => fst(x) * snd(x))) |>
                oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))
            ))
          ))
        ))

    // scalastyle:off
    val init: (Int, Int, Int) => String = (N, M, O) => {
      s"""
         |
         |#include <stdio.h>
         |#include <stdlib.h>
         |
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
         |  inA[i] = (float)((i % 100) + 1);
         |}
         |
         |float* inB = hostBufferSync(ctx, inputB, M * O * sizeof(float), HOST_WRITE);
         |for (int i = 0; i < M * O; i++) {
         |  inB[i] = (float)((i % 100) + 1);
         |}
         |
         |// init checking
         |FILE *fptr;
         |
         |if ((fptr = fopen("mm_${N}_${M}_${O}.csv","r")) == NULL){
         |  return 133;
         |}
         |float gold[N*O];
         |
         |for(int i = 0; i<N*O; i++){
         |  fscanf(fptr, "%f,", &gold[i]);
         |}
         |fclose(fptr);
         |
         |""".stripMargin
    }

    val compute: String =
      s"""
         |    fun_run(ctx, &fun, outputC, inputA, inputB);
         |
         |    float* outC = hostBufferSync(ctx, outputC, N * O * sizeof(float), HOST_READ);
         |    for(int i = 0; i < N*O; i++){
         |      if(outC[i] != gold[i]){
         |          return 132;
         |      }
         |    }
         |
         |""".stripMargin

    val finish: String = {
      s"""
         |
         |  destroyBuffer(ctx, inputA);
         |  destroyBuffer(ctx, inputB);
         |  destroyBuffer(ctx, outputC);
         |""".stripMargin
    }

    val hostCode = HostCode(
      init = init(N, N, N),
      compute = compute,
      finish = finish,
    )

    def generate_and_store_gold(N: Int) = {

      //    Array.
      val A: Array[Int] = Range(0, N * N).toArray.map(i => (i % 100) + 1)
      val B: Array[Int] = Range(0, N * N).toArray.map(i => (i % 100) + 1)

      val C = Array.fill(N * N)(0)

      for (i <- 0 until N) {
        for (j <- 0 until N) {
          var sum = 0
          for (k <- 0 until N) {
            sum += A(i * N + k) * B(k * N + j)
          }
          C(i * N + j) = sum
        }
      }
      // write C to file
      val result = C.mkString(",")

      util.writeToPath(s"mm_${N}_${N}_${N}.csv", result)
    }
  }

  object acoustic {

    // check if result is correct!
    //    val N = 2048
    //    val M = 1024
    //    val O = 32

    val N = 128
    val M = 64
    val O = 32

    val expression: Rise = apps.stencil.acoustic3D.stencilMSS_high_level
    val expert: Rise = apps.stencil.acoustic3D.stencilMSS

    // host code
    // scalastyle:off
    val init: (Int, Int, Int) => String = (O, N, M) => {
      s"""
         |#include <stdio.h>
         |#include <stdlib.h>
         |#include <math.h>
         |
         |const int O = ${O};
         |const int N = ${N};
         |const int M = ${M};
         |
         |srand(time(NULL));
         |
         |Buffer mat1 = createBuffer(ctx, (O+2) * (N+2) * (M+2) * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
         |Buffer mat2 = createBuffer(ctx, (O+2) * (N+2) * (M+2) * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
         |Buffer output = createBuffer(ctx, O * N * M * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);
         |
         |float* m1 = hostBufferSync(ctx, mat1, (O+2) * (N+2) * (M+2) * sizeof(float), HOST_WRITE);
         |for (int i = 0; i < (O+2) * (N+2) * (M+2); i++) {
         |  m1[i] = (float)(i%100);
         |}
         |float* m2 = hostBufferSync(ctx, mat2, (O+2) * (N+2) * (M+2) * sizeof(float), HOST_WRITE);
         |for (int i = 0; i < (O+2) * (N+2) * (M+2); i++) {
         |  m2[i] = (float)(i%100);
         |}
         |
         |// synchronize before entering timed section
         |deviceBufferSync(ctx, mat1, (O+2) * (N+2) * (M+2) * sizeof(float), DEVICE_READ);
         |deviceBufferSync(ctx, mat2, (O+2) * (N+2) * (M+2) * sizeof(float), DEVICE_READ);
         |waitFinished(ctx);
         |
         |// init checking
         |FILE *fptr;
         |
         |if ((fptr = fopen("acoustic_${N}_${M}_${O}.csv","r")) == NULL){
         |  return 133;
         |}
         |float gold[N*O*M];
         |
         |for(int i = 0; i<N*O*M; i++){
         |  fscanf(fptr, "%f,", &gold[i]);
         |}
         |
         |fclose(fptr);
         |

         |""".stripMargin
    }

    val compute =
      s"""
         |fun_run(ctx, &fun, output, O, N, M, mat1, mat2);
         |
         |waitFinished(ctx);
         |// check gold
         |float* out = hostBufferSync(ctx, output, O * N * M * sizeof(float), HOST_READ);
         |for(int i = 0; i < N*O*M; i++){
         |  if(fabsf(out[i] - gold[i]) > 0.001){
         |    return 132;
         |  }
         |}
         |
         |""".stripMargin


    val finish_generate_gold =
      s"""
         |// TODO: could check output here
         |// use given gold expression?
         |	float* out = hostBufferSync(ctx, output, O * N * M * sizeof(float), HOST_READ);
         |
         |FILE *fptr;
         |if ((fptr = fopen("acoustic_${N}_${M}_${O}.csv","w")) == NULL){
         |        return 133;
         |   }
         |
         |    for(int i = 0; i < (O * N * M) - 1; i++){
         |      fprintf(fptr, "%f,", out[i]);
         |    }
         |
         |      fprintf(fptr, "%f", out[(O*N*M)-1]);
         |
         |fclose(fptr);
         |
         |destroyBuffer(ctx, mat1);
         |destroyBuffer(ctx, mat2);
         |destroyBuffer(ctx, output);
         |""".stripMargin


    val finish =
      s"""
         |// TODO: could check output here
         |// use given gold expression?
         |
         |destroyBuffer(ctx, mat1);
         |destroyBuffer(ctx, mat2);
         |destroyBuffer(ctx, output);
         |""".stripMargin
    // scalastyle:on

    val hostCode = HostCode(
      init = init(O, N, M),
      compute = compute,
      finish = finish
    )

    // todo add gold and checking
    def generate_and_store_gold(N: Int) = {

      //      //    Array.
      //      val A: Array[Int] = Range(0, N * N).toArray.map(i => (i % 100) + 1)
      //      val B: Array[Int] = Range(0, N * N).toArray.map(i => (i % 100) + 1)
      //
      //      val C = Array.fill(N * N)(0)
      //
      //      for (i <- 0 until N) {
      //        for (j <- 0 until N) {
      //          var sum = 0
      //          for (k <- 0 until N) {
      //            sum += A(i * N + k) * B(k * N + j)
      //          }
      //          C(i * N + j) = sum
      //        }
      //      }
      //      // write C to file
      //      val result = C.mkString(",")
      //
      //      util.writeToPath(s"mm_${N}_${N}_${N}.csv", result)
    }

  }

  object harris {

    val hMod: Int = 128
    val wMod: Int = 256

    val expression: Rise = apps.harrisCornerDetectionHalide.harris(hMod = hMod, wMod = wMod).toExpr

    // scalastyle:off
    val init: (Int, Int) => String = (Ho, Wo) => {
      s"""
         |const int Hi = ${Ho + 4};
         |const int Wi = ${Wo};
         |const int Ho = ${Ho};
         |const int Wo = ${Wo};
         |
         |srand(time(NULL));
         |
         |Buffer input = createBuffer(ctx, 3 * Hi * Wi * sizeof(float), HOST_WRITE | DEVICE_READ);
         |Buffer output = createBuffer(ctx, Ho * Wo *  sizeof(float), HOST_READ | DEVICE_WRITE);
         |
         |float* in = hostBufferSync(ctx, input, 3 * Hi * Wi * sizeof(float), HOST_WRITE);
         |for (int i = 0; i < 3 * Hi * Wi; i++) {
         |  in[i] = (float)((i + 179) % 256) / 25.6f;
         |}
         |""".stripMargin
    }

    val compute =
      s"""
         |fun_run(ctx, &fun, output, Ho, Wo, input);
         |""".stripMargin

    val finish =
      s"""
         |// TODO: could check output here
         |
         |destroyBuffer(ctx, input);
         |destroyBuffer(ctx, output);
         |""".stripMargin
    // scalastyle:on

    val hostCode = HostCode(
      init = init(hMod, wMod),
      compute = compute,
      finish = finish
    )
  }
}
