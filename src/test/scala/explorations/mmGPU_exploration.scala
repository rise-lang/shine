package explorations

import apps.tvmGemm.{innermost, outermost}
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.heuristic_search.util.Solution
import elevate.macros.RuleMacro.rule
import exploration.runner.CExecutor
import exploration.{ExecutorConfig, MetaheuristicConfig}
import rise.core.DSL.{fun, lf32}
import rise.core.primitives._
import rise.core.types.DataType.{ArrayType, f32}
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.algorithmic.reorder
import rise.elevate.strategies.lowering._
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.predicate._
import rise.elevate.strategies.tiling._
import rise.elevate.strategies.traversal._
import rise.elevate.{NormalizedThen, Rise}


import rise.core._
import rise.core.DSL._
import rise.core.primitives.{let => _, _}
import rise.core.DSL.Type._
import rise.core.types._
import rise.core.types.DataType._
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeq

class mmGPU_exploration extends test_util.Tests {
  //
  //
  //  private val id = fun(x => x)
  //  private val mulT = separableConvolution2D.mulT
  //  private val dot = separableConvolution2D.dot
  //  private val dotSeq = fun(a => fun(b =>
  //    zip(a)(b) |> map(mulT) |> oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))
  //  ))
  //
  //
  //  // define expression
  //  val N = 512
  //
  //  val mm: Rise = //infer(
  //    fun(ArrayType(N, ArrayType(N, f32)))(a =>
  //      fun(ArrayType(N, ArrayType(N, f32)))(b =>
  //        a |> map(fun(ak =>
  //          transpose(b) |> map(fun(bk =>
  //            zip(ak)(bk) |>
  //              map(fun(x => fst(x) * snd(x))) |>
  //              reduce(add)(lf32(0.0f))
  //          ))
  //        ))
  //      ))
  //
  //
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
  //
  //  val e0: Rise = apps.mm.mmNVIDIA
  //  val e1: Rise = apps.mm.mmAMD
  //
  //
  //  // define strategies here
  //  val map_strategies: Set[Strategy[Rise]] = {
  //    Set(
  //      rise.elevate.rules.lowering.unroll, // todo think about this
  //      rise.elevate.rules.lowering.mapGlobal(0),
  //      rise.elevate.rules.lowering.mapGlobal(1),
  //      rise.elevate.rules.lowering.mapWorkGroup(0),
  //      rise.elevate.rules.lowering.mapWorkGroup(1),
  //      rise.elevate.rules.lowering.mapLocal(0),
  //      rise.elevate.rules.lowering.mapLocal(1),
  //      rise.elevate.rules.lowering.mapSeqCompute()
  //    )
  //  }
  //  // todo think about constraints for memory maps
  //
  //
  //  test("mmGPU - execute versions") {
  //    // which version to use
  //    // how to execute version, include auto tuning?
  //    // make GPU executor using host-code (copied from auto-tuning?)
  //
  //
  //    // execute array packing (start) and par (goal) versions
  //
  //    //    val mm_arrayPacking = arrayPacking.apply(mm).get
  //    //    val mm_arrayPacking_lowered = lowerToC.apply(mm_arrayPacking).get
  //    //
  //    //    val mm_par = par.apply(mm).get
  //    //
  //    //    val executor = CExecutor(
  //    //      lowering = lowerToC,
  //    //      goldExpression = mm_arrayPacking_lowered,
  //    //      inputSize = N,
  //    //      saveToDisk = false
  //    //    )
  //    //
  //    //    val arrayPackingResult = executor.execute(Solution[Rise](mm_arrayPacking, scala.collection.immutable.Seq(arrayPacking)))
  //    //    println("arrayPacking: " + arrayPackingResult)
  //    //    assert(arrayPackingResult.performance.isDefined)
  //    //
  //    //    val parResult = executor.execute(Solution[Rise](mm_par, scala.collection.immutable.Seq(par)))
  //    //    println("par: " + parResult)
  //    //    assert(parResult.performance.isDefined)
  //
  //  }
  //
  //
  //  test("mmCPU - explore maps") {
  //
  //    //    val e = arrayPacking.apply(mm).get
  //
  //    val ii = scala.collection.immutable.Seq(
  //      MetaheuristicConfig(
  //        heuristic = "IterativeImprovement",
  //        depth = 4,
  //        iteration = 1
  //      )
  //    )
  //
  //    val random = scala.collection.immutable.Seq(
  //      MetaheuristicConfig(
  //        heuristic = "Random",
  //        depth = 14,
  //        iteration = 1
  //      )
  //    )
  //
  //    val autotuner = scala.collection.immutable.Seq(
  //      MetaheuristicConfig(
  //        heuristic = "autotuner",
  //        depth = 13,
  //        iteration = 1
  //      )
  //    )
  //
  //    val executor = ExecutorConfig(
  //      name = "C",
  //      iterations = 1,
  //      threshold = 1000
  //    )
  //
  //    // setup explorer config
  //    val explorer = exploration.Explorer(
  //      name = "mmGPU_maps",
  //      output = "/home/jo/development/experiments/exploration/dodekarch/",
  //      inputSize = N,
  //      metaheuristics = random,
  //      executor = executor,
  //      lowering = exploration.strategies.blockingExploration.lowering,
  //      strategies = exploration.strategies.blockingExploration.strategies,
  //      rewriteFunction = Some(exploration.rewriter.everywhere.rewriteFunction(map_strategies)),
  //      normalForm = Some(DFNF()),
  //      importExport = Some(exploration.explorationUtil.IO.importExport)
  //    )
  //
  //    //    val explorationResult = exploration.explore(explorer)(e)
  //
  //    //    println("explorationResult: " + explorationResult)
  //  }
  //

}
