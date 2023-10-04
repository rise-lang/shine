package apps.autotuning

import apps.tvmGemm.{innermost, outermost, par}
import elevate.core._
import elevate.core.strategies.traversal._
import elevate.heuristic_search.util.Solution
import rise.autotune
import rise.autotune._
import rise.core.DSL._
import rise.core._
import rise.core.primitives._
import rise.core.types.DataType._
import rise.elevate._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.algorithmic.reorder
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.predicate._
import rise.elevate.strategies.tiling._
import rise.elevate.strategies.traversal._

import scala.collection.immutable
import scala.language.postfixOps
import exploration.runner.CExecutor
import rise.core.types.{Nat, TuningParameter}


class mmCPU extends test_util.Tests {

  // tvm gemm
  val N = 1024

  val mm: Expr = //infer(
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

  // -- BASELINE ---------------------------------------------------------------

  val baseline: Strategy[Rise] = DFNF()(default.RiseTraversable) `;`
    (fuseReduceMap `@` topDown[Rise])

  // -- BLOCKING ---------------------------------------------------------------

  val isFullyAppliedReduce: Strategy[Rise] = isApplied(isApplied(isApplied(isReduce)))

  val isFullyAppliedMap: Strategy[Rise] = isApplied(isApplied(isMap))

  //  // -- LOOP PERMUTATION -------------------------------------------------------
  //
  //  val loopPerm: Strategy[Rise] = baseline `;`
  //    (tile() `@` outermost(mapNest(2))) `;;`
  //    (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
  //    (tunable("split", splitStrategy) `@` innermost(isFullyAppliedReduce)) `;;`
  //    reorder(List(1, 2, 5, 3, 6, 4)) `;;`
  //    (tunable("vec", vectorize) `@` innermost(isFullyAppliedMap))
  //
  //  val mm2: Rise = loopPerm.apply(mm).get

  val mm_par = par.apply(mm)
  val gold = lowerToC.apply(mm_par.get).get

  val inject2: (Expr, Map[String, Int], Map[String, List[Int]]) => Either[String, Expr] = (e, tuningParameterMap, permutationMap) => {

    val tileX = tuningParameterMap("tuned_tile8575")
    val tileY = tuningParameterMap("tuned_tile8576")
    val blockedReduce = tuningParameterMap("tuned_blockedReduce8361")
    val reordering = permutationMap("tuned_reorder").map(x => x + 1)
    val vec = tuningParameterMap("tuned_vec8362")
    val arrayPackingSplit = tuningParameterMap("tuned_arrayPackingSplit8359")
    val arrayPackingVec = tuningParameterMap("tuned_arrayPackingVec8360")

    //    val tileX = 32
    //    val tileY = 32
    //    val split = 4
    //    val vec = 32

    val isTransposedB: Strategy[Rise] = isApplied(isTranspose)
    val permuteB: Strategy[Rise] =
      splitJoin2(arrayPackingSplit) `;` DFNF() `;` argument(idAfter) `;`
        topDown(liftId()) `;` topDown(createTransposePair) `;` RNF() `;`
        argument(argument(idAfter)) `;` elevate.core.strategies.basic.normalize.apply(liftId()) `;`
        topDown(idToCopy)

    def inLambda(s: Strategy[Rise]): Strategy[Rise] =
      isLambda `;` ((e: Rise) => body(inLambda(s))(e)) <+ s

    val packB: Strategy[Rise] = arrayPackingVec match {
      case 1 =>
        rise.elevate.strategies.lowering.storeInMemory(isTransposedB,
          permuteB
        ) `@` inLambda
      case _ =>
        rise.elevate.strategies.lowering.storeInMemory(isTransposedB,
          permuteB `;;`
            (vectorize(arrayPackingVec) `@` innermost(isFullyAppliedMap)) // `;;`
        ) `@` inLambda
    }

    val strategyHead: Strategy[Rise] = packB `;;`
      baseline `;`
      (tile(tileX, tileY) `@` outermost(mapNest(2))) `;;`
      (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
      (splitStrategy(blockedReduce) `@` innermost(isFullyAppliedReduce)) `;;`
      reorder(reordering) // `;;`
    (vectorize(vec) `@` innermost(isFullyAppliedMap))

    val strategyTail: Strategy[Rise] = reordering match {
      case List(1, 2, 3, 4, 5, 6) =>
        elevate.core.strategies.basic.id[Rise]
      case _ =>
        (vectorize(vec) `@` innermost(isFullyAppliedMap))
    }

    val strategy: Strategy[Rise] = strategyHead `;;` strategyTail

    val result = try {
      strategy.apply(e)
    } catch {
      case e: Throwable => {
        Failure(strategy)
      }
    }

    result match {
      case Success(expression) => Right(expression)
      case Failure(value) => Left(value.toString())
    }
  }


  val inject: (Expr, Map[String, Int], Map[String, List[Int]]) => Either[String, Expr] = (e, tuningParameterMap, permutationMap) => {

    val tileX = tuningParameterMap.get("tuned_tile51").get
    val tileY = tuningParameterMap.get("tuned_tile52").get
    val split = tuningParameterMap.get("tuned_split100").get
    val reordering = permutationMap.get("tuned_reorder").get.map(x => x + 1)
    val vec = tuningParameterMap.get("tuned_vec101").get

    //    val tileX = 32
    //    val tileY = 32
    //    val split = 4
    //    val vec = 32

    val loopPerm: Strategy[Rise] = baseline `;`
      (tile(tileX, tileY) `@` outermost(mapNest(2))) `;;`
      (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
      (splitStrategy(split) `@` innermost(isFullyAppliedReduce)) `;;`
      reorder(reordering) `;;`
      (vectorize(vec) `@` innermost(isFullyAppliedMap))

    val result = try {
      loopPerm.apply(e)
    } catch {
      case e: Throwable => {
        println("e: " + e)
        Failure(loopPerm)
      }
    }

    result match {
      case Success(expression) => Right(expression)
      case Failure(value) => Left(value.toString())
    }
  }

  val executor = CExecutor(
    lowering = lowerToC,
    goldExpression = gold,
    iterations = 21,
    inputSize = N,
    threshold = 10,
    timeout = 10000,
    output = "autotuning",
    saveToDisk = false
  )


  val execute: Expr => (
    Either[AutoTuningError, Double],
      Option[Double],
      Option[Double],
      Option[Double]
    ) = e => {

    val strategies = immutable.Seq.empty[Strategy[Rise]]

    val executionStart = System.currentTimeMillis()
    val result = executor.execute(Solution(e, strategies)).performance

    // todo move to other thing
    val runtime: Either[AutoTuningError, Double] = result match {
      case Some(value) => Right(value)
      case None => Left(AutoTuningError(EXECUTION_ERROR, None))
    }

    // todo measure these properly
    val codegenTime = (System.currentTimeMillis() - executionStart).toDouble
    val compilationTime = (System.currentTimeMillis() - executionStart).toDouble
    val executionTime = (System.currentTimeMillis() - executionStart).toDouble

    (runtime,
      Some(codegenTime),
      Some(compilationTime),
      Some(executionTime))
  }

  test("run experiment mmCPU") {
    val inputSize: Int = N

    val expertConfiugration: (Map[String, Int], Map[String, List[Int]]) = (
      Map(
        "tuned_tile51" -> 8,
        "tuned_tile52" -> 128,
        "tuned_split100" -> 128,
        "tuned_vec101" -> 8
      ),
      Map(
        "tuned_reorder" -> List(0, 1, 4, 2, 5, 3)
      )
    )

    val defaultConfiugration: (Map[String, Int], Map[String, List[Int]]) = (
      Map(
        "tuned_tile51" -> 32,
        "tuned_tile52" -> 32,
        "tuned_split100" -> 4,
        "tuned_vec101" -> 8
      ),
      Map(
        "tuned_reorder" -> List(0, 1, 4, 5, 2, 3)
      )
    )

    val configs = Seq(
      s"autotuning/config/mmCPU/${inputSize.toString}/rs_cot_${inputSize.toString}.json",
      s"autotuning/config/mmCPU/${inputSize.toString}/rs_emb_${inputSize.toString}.json",
      s"autotuning/config/mmCPU/${inputSize.toString}/bolog_cot_${inputSize.toString}.json",
      s"autotuning/config/mmCPU/${inputSize.toString}/atf_emb_${inputSize.toString}.json",
      s"autotuning/config/mmCPU/${inputSize.toString}/ytoptccs_${inputSize.toString}.json"
    )

    runExperiment(
      name = s"MM_CPU",
      configFiles = configs,
      iterations = getIterations(),
      output = s"artifact/results/rise/MM_CPU",
      mm,
      HostCode("", "", ""), // ignore this (we execute manually)
      Seq(inputSize, inputSize, inputSize),
      strategyMode = Some(inject),
      executor = Some(execute),
      plotOnly = false,
      expert = None,
      default = None,
      expert2 = Some(expertConfiugration),
      default2 = Some(defaultConfiugration)
    )
  }

}