package explorations

import apps.tvmGemm.{innermost, outermost}
import arithexpr.arithmetic.RangeMul
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.heuristic_search.util.Solution
import elevate.macros.RuleMacro.rule
import exploration.neighborhoods._
import exploration.runner.{AutoTuningExecutor, CExecutor}
import exploration.{ExecutorConfig, MetaheuristicConfig, NeighborhoodConfig}
import rise.autotune
import rise.autotune.{HostCode, Tuner, tuningParam, wrapOclRun}
import rise.core.DSL.{fun, lf32}
import rise.core.primitives._
import rise.core.types.DataType.{ArrayType, f32}
import rise.elevate.rules.algorithmic.{joinSplit, _}
import rise.elevate.rules.lowering._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.algorithmic.reorder
import rise.elevate.strategies.lowering._
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.predicate._
import rise.elevate.strategies.tiling._
import rise.elevate.strategies.traversal._
import rise.elevate.{NormalizedThen, Rise, tunable}
import rise.core._
import rise.core.DSL._
import rise.core.primitives.{let => _, _}
import rise.core.DSL.Type._
import rise.core.types._
import rise.core.types.DataType._
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeq
import shine.OpenCL.{GlobalSize, LocalSize}

import scala.collection.mutable.ListBuffer

class mmGPU_exploration extends test_util.Tests {

  // define expression
  val N = 128
  //  val N = 4

  val mm: Rise = //infer(
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

  val compute =
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

  val finish: (Int, Int, Int) => String = (N, M, O) => {
    s"""
       |
       |  destroyBuffer(ctx, inputA);
       |  destroyBuffer(ctx, inputB);
       |  destroyBuffer(ctx, outputC);
       |""".stripMargin
  }
  // scalastyle:on

  // this could be the goal
  //  val mmAMD: ToBeTyped[Expr] = {
  //    val v3 = 4
  //    val v4 = 8
  //    val vw = 4
  //
  //    depFun((n: Nat, m: Nat, o: Nat) => fun(
  //      (o `.` n `.` f32) ->: (o `.` m `.` f32) ->: (n `.` m `.` f32)
  //    )((at, b) =>
  //      split(v4)(transpose(at)) |> // Mo.Mi.o.f
  //        mapGlobal(1)(fun(v4 `.` o `.` f32)(p3 =>
  //          split(v3)(transpose(b)) |> // No.Ni.o.f
  //            mapGlobal(0)(fun(v3 `.` o `.` f32)(p4 =>
  //              zip(transpose(p3))(transpose(p4)) |> // o.(Mi.f x Ni.f)
  //                oclReduceSeq(AddressSpace.Private)(fun((p6, p7) =>
  //                  let(toPrivate(makePair(mapSeq(id)(p7._1))(
  //                    asScalar o mapSeq(id) o asVectorAligned(vw) $ p7._2)))
  //                    be (x =>
  //                    mapSeq(fun(p8 =>
  //                      mapSeq(fun(p9 =>
  //                        p9._1 + (p8._2 * p9._2)
  //                      ))(zip(p8._1)(x._2))
  //                    ))(zip(p6)(x._1))
  //                    )
  //                ))(mapSeq(mapSeq(id))(generate(fun(_ => generate(fun(_ => lf32(0.0f)))))) :: (v4 `.` v3 `.` f32)) |> //
  //                mapSeq(asScalar o mapSeq(id) o asVector(vw)) |>
  //                transpose // v3.v4.f
  //            )) |> join |> transpose
  //        )) |> join
  //    ))
  //  }
  //

  //  default lowering that always wins
  val lowering0: Strategy[Rise] =
    addCopiesForUnfusedReduceOcl `@` everywhere

  val lowering1: Strategy[Rise] = rise.elevate.rules.lowering.reduceSeq `@` everywhere

  val lowering2: Strategy[Rise] =
    rise.elevate.rules.lowering.reduceOCL()

  val lowering3: Strategy[Rise] =
    rise.elevate.rules.lowering.mapGlobal(0) `@` topDown[Rise]

  val lowering4: Strategy[Rise] =
    rise.elevate.rules.lowering.mapGlobal(1) `@` topDown[Rise]

  val lowering5: Strategy[Rise] =
    rise.elevate.rules.lowering.mapSeqCompute() `@` normalize[Rise]

  val lowering: Strategy[Rise] =
    lowering0 `;` // add copies if necessary
      lowering1 `;` // reduce -> reduceSeq
      lowering2 `;` // reduceSeq -> reduceOcl
      //      lowering3 `;` // map -> map global 0 (topdown/outermost)
      //      lowering4 `;` // map -> mapGlobal 1 (topdown/outermost)
      lowering5 // map (compute) -> mapSeq

  // @rule broken here? why?
  @rule def splitJoinRule: Strategy[Rise] = tunable(splitJoin)

  // reverse rules
  val rules: scala.collection.immutable.Seq[Strategy[Rise]] = {
    scala.collection.immutable.Seq(
      //      elevate.core.strategies.basic.id[Rise], // id by default (allow expression itself to be in neighborhood)
      fuseReduceMap, // fusion
      reduceMapFission(), // fission
      splitJoinRule, // split join
      joinSplit, // split join reverse
      rise.elevate.rules.lowering.mapGlobal(0),
      rise.elevate.rules.lowering.mapGlobal(1),
      rise.elevate.rules.lowering.mapGlobalReverse,
      rise.elevate.rules.lowering.mapWorkGroup(0),
      rise.elevate.rules.lowering.mapWorkGroup(1),
      rise.elevate.rules.lowering.mapWorkGroupReverse,
      rise.elevate.rules.lowering.mapLocal(0),
      rise.elevate.rules.lowering.mapLocal(1),
      rise.elevate.rules.lowering.mapLocalReverse,
      rise.elevate.rules.lowering.mapSeqCompute(),
      rise.elevate.rules.lowering.mapSeqReverse,
    )
  }

  test("test lowering") {

    val lowered = lowering.apply(mm).get
    println("lowered: \n" + lowered)

    val tuner = Tuner(
      hostCode = HostCode(init(N, N, N), compute, finish(N, N, N)),
      inputSizes = scala.collection.immutable.Seq(N, N, N),
      samples = 20,
      hmConstraints = true,
      name = "test",
      output = "autotuning",
      saveToFile = true
    )

    val ocl: Rise =
      tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
        tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
          tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
            tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(lowered)
            ))))

    val result = autotune.search(tuner)(ocl)

    result.samples.foreach(elem => {
      println(elem.parameters)
      println(elem.runtime)
    }
    )
  }

  test("tune expert") {

    val lowering: Strategy[Rise] =
      lowering0 `;` // add copies if necessary
        lowering1 `;` // reduce -> reduceSeq
        lowering2 `;` // reduceSeq -> reduceOcl
        lowering3 `;` // map -> map global 0 (topdown/outermost)
        lowering4 `;` // map -> mapGlobal 1 (topdown/outermost)
        lowering5 // map (compute) -> mapSeq

    val lowered = lowering.apply(mm).get
    println("lowered: \n" + lowered)

    val tuner = Tuner(
      hostCode = HostCode(init(N, N, N), compute, finish(N, N, N)),
      inputSizes = scala.collection.immutable.Seq(N, N, N),
      samples = 100,
      hmConstraints = true,
      name = "test",
      output = "autotuning",
      saveToFile = true
    )

    val ocl: Rise =
      tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
        tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
          tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
            tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(lowered)
            ))))

    val result = autotune.search(tuner)(ocl)

    result.samples.foreach(elem => {
      println(elem.parameters)
      println(elem.runtime)
    }
    )

    val best = autotune.getBest(result.samples)
    println("best: " + best)
  }

  test("create output file") {
    // compute mm result
    //    val N = 4
    val N = 128
    //    val N = 1024

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
    //    //
    //    println("A: ")
    //    for (i <- 0 until N) {
    //      for (j <- 0 until N) {
    //        print(" " + A(i * N + j) + " ")
    //      }
    //      println()
    //    }
    //    println()
    //
    //    println("B: ")
    //    for (i <- 0 until N) {
    //      for (j <- 0 until N) {
    //        print(" " + B(i * N + j) + " ")
    //      }
    //      println()
    //    }
    //    println()
    //
    //    println("C: ")
    //    for (i <- 0 until N) {
    //      for (j <- 0 until N) {
    //        print(" " + C(i * N + j) + " ")
    //      }
    //      println()
    //    }


    // write C to file
    val result = C.mkString(",")

    util.writeToPath(s"mm_${N}_${N}_${N}.csv", result)

  }

  test("mmGPU - explore maps") {

    //    val e = arrayPacking.apply(mm).get

    val ii = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "IterativeImprovement",
        depth = 10,
        //        samples = 50,
        //        repeat = 1
      )
    )

    val randomGraph = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "RandomGraph",
        depth = 30, // is this ignored?
        samples = 30, // does not scale well
        repeat = 5
      )
    )

    val tabuSearchPlain = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "TabuSearchPlain",
        depth = 7, // is this ignored?
        samples = 100,
        repeat = 3
      )
    )

    val simulatedAnnealingPlain = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "SimulatedAnnealingPlain",
        depth = 7, // is this ignored?
        samples = 100,
        repeat = 3
      )
    )

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Exhaustive",
        depth = 7, // is this ignored? -> maybe not for tree window slide
        samples = 30,
        repeat = 5
      )
    )

    // the local search we want to use
    val localSearchGraph = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "LocalSearchGraph",
        depth = 10, // seems to be ignored
        samples = 100, // not ignored
        repeat = 5
      )
    )

    val localSearchExp = scala.collection.immutable.Seq(
      //      random,
      //      tabuSearchPlain,
      //      simulatedAnnealingPlain,
      //      exhaustive,
      exhaustive,
      //      localSearchGraph,
      randomGraph
      //       random?
      // tabu search? -> something more advanced
    )

    val executor = ExecutorConfig(
      name = "AutoTuning",
      iterations = 5,
      threshold = 10,
      samples = 10
    )

    val experiment = scala.collection.immutable.Seq(
      //      randomGraph,
      //      localSearchGraph,
      //      ii
      //            exhaustive
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "mmGPU_maps",
      output = "/home/jo/development/rise-lang/shine/experiments/exploration/dodekarch/mmGPU",
      inputSize = N,
      metaheuristics = Right(localSearchExp),
      executor = executor,
      lowering = lowering,
      strategies = rules, // is this ignored here?
      hostCode = Some(HostCode(init(N, N, N), compute, finish(N, N, N))),
      //      neighborhoodConfig = NeighborhoodConfig(neighborhood = NTreeLeafsWindowChoice),
      neighborhoodConfig = NeighborhoodConfig(neighborhood = NGraphChoice),
      rewriteFunction = None,
      //      rewriteFunction = Some(exploration.rewriter.everywhere.rewriteFunction(rules)),
      //      rewriteFunction = Some(
      //        exploration.rewriter.everywhere.neighbourhoodWide(
      //          strategies = parallel_fine_light,
      //          slideWindow = 20
      //        )
      //      )
      //      normalForm = Some(DFNF()),
      normalForm = None,
      importExport = None,
      expert = Some(0.009632),
      overwrite = false
    )

    val result = exploration.explore(explorer)(mm)

  }


}
