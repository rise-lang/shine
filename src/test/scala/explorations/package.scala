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

package object explorations {

  object mm {

    val N: Int = 1024
    //    val N: Int = 128
    //
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

    val N = 128
    val M = 64
    val O = 32

    val expression: Rise = apps.stencil.acoustic3D.stencilMSS_high_level

    // host code
    // scalastyle:off
    val init: (Int, Int, Int) => String = (O, N, M) => {
      s"""
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
         |""".stripMargin
    }

    val compute =
      s"""
         |fun_run(ctx, &fun, output, O, N, M, mat1, mat2);
         |waitFinished(ctx);
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


}
