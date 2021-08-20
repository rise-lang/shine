package rise.elevate

import elevate.core._
import elevate.core.strategies.basic._
//import elevate.core.strategies.debug.peek
//import rise.core.IsClosedForm
import elevate.core.strategies.debug.debug
import elevate.core.strategies.traversal._
import rise.core.DSL._
import rise.core.primitives._
import rise.core.types.DataType._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.algorithmic.reorder
import rise.elevate.strategies.lowering._
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.predicate._
import rise.elevate.strategies.tiling._
import rise.elevate.strategies.traversal
import rise.elevate.strategies.traversal._

import _root_.util.gen

object tvmGemm {
  val outermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.outermost(using default.RiseTraversable)
  val innermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.innermost(using default.RiseTraversable)

  //// MM INPUT EXPRESSION /////////////////////////////////////////////////////
  val N = 1024

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
  //)

  //// ICFP'20 TVM - STRATEGIES ////////////////////////////////////////////////
  // -- BASELINE ---------------------------------------------------------------

  val baseline: Strategy[Rise] = DFNF()(using default.RiseTraversable) `;`
    fuseReduceMap `@` topDown[Rise]

  // -- BLOCKING ---------------------------------------------------------------

  val isFullyAppliedReduce: Strategy[Rise] = isApplied(isApplied(isApplied(isReduce)))
  val blocking: Strategy[Rise] =
    baseline `;`
      (tile(32,32)        `@` outermost(mapNest(2))) `;;`
      (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
      (splitStrategy(4)   `@` innermost(isFullyAppliedReduce)) `;;`
      reorder(List(1,2,5,6,3,4))

  // -- VECTORIZATION ----------------------------------------------------------

  val isFullyAppliedMap: Strategy[Rise] = isApplied(isApplied(isMap))
  val vectorization: Strategy[Rise] =
    blocking `;;`
      (vectorize(32) `@` innermost(isApplied(isApplied(isMap))))

  // -- LOOP PERMUTATION -------------------------------------------------------

  val loopPerm: Strategy[Rise] = baseline `;`
    (tile(32,32)        `@` outermost(mapNest(2))) `;;`
    (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
    (splitStrategy(4)   `@` innermost(isFullyAppliedReduce)) `;;`
    reorder(List(1,2,5,3,6,4)) `;;`
    (vectorize(32) `@` innermost(isFullyAppliedMap))

  // -- ARRAY PACKING ----------------------------------------------------------

  val isTransposedB: Strategy[Rise] = isApplied(isTranspose)
  val permuteB: Strategy[Rise] =
      splitJoin2(32) `;` DFNF() `;` argument(idAfter) `;`
      topDown(liftId()) `;` topDown(createTransposePair) `;` RNF() `;`
      argument(argument(idAfter)) `;` normalize(liftId()) `;`
      topDown(idToCopy)

  val packB: Strategy[Rise] =
    storeInMemory(isTransposedB,
      permuteB `;;`
        (vectorize(32) `@` innermost(isFullyAppliedMap)) `;;`
        (parallel()    `@` outermost(isApplied(isMap)))
    ) `@` inLambda

  def inLambda(s: Strategy[Rise]): Strategy[Rise] =
    isLambda `;` ( (e: Rise) => body(inLambda(s))(e) ) <+ s

  val arrayPacking: Strategy[Rise] = packB `;;` loopPerm

  // -- CACHE BLOCKS -----------------------------------------------------------

  val cacheBlocks: Strategy[Rise] = (
    arrayPacking `;;`// debug[Rise]("after arrayPacking") `;`
      (unroll `@` innermost(isReduceSeq))
    )

  // -- PARALLEL ---------------------------------------------------------------

  val par = (
    arrayPacking `;;`
      ((parallel() `@` outermost(isApplied(isMap))) `@`
        outermost(isApplied(isLet))) `;;`
      (unroll `@` innermost(isReduceSeq))
    )
}

// scalastyle:off
class tvmGemm extends test_util.Tests {
  import tvmGemm._

  test("baseline") {
    run("baseline",  baseline `;` lowerToC, openMP = false)
  }

  test("blocking") {
    run("blocking", blocking `;` lowerToC, openMP = false)
  }

  test("vectorization") {
    run("vectorization", (vectorization `;` lowerToC), openMP = true)
  }

  test("loop permutation") {
    run("loop_permutation", (loopPerm `;` lowerToC), openMP = true)
  }

  test("array packing") {
    run("array_packing", arrayPacking `;` lowerToC, openMP = true)
  }

  test("cache blocks") {
    run("cache_blocks", cacheBlocks `;` lowerToC, openMP = true)
  }

  test("parallel") {
    run("parallel", par `;` lowerToC, openMP = true)
  }


  /// UTILS ////////////////////////////////////////////////////////////////////

  def run(version: String,
          strategy: Strategy[Rise],
          openMP: Boolean // generate C or OpenMP code?
         ): Unit = {

    val generateFiles = false
    val kernelsFolder: String = "/home/artifact/kernels"
    val plotsFolder: String = "/home/artifact/results/fig10/steps"

    def writeToFile(path: String, name: String, content: String, ending: String = ".c"): Unit = {
      import java.io._
      val w =new PrintWriter(new File(s"$path/$name$ending"))
      w.write(content)
      w.flush()
      w.close()
    }

    def currentTimeSec: Long = System.currentTimeMillis / 1000

    val versionUC = version.toUpperCase()
    // reset rewrite step counter
    elevate.core.SuccessRewriteCounter.rewriteCount = 0

    // rewrite the matmul input expresssion
    val time0 = currentTimeSec
    val rewritten = strategy(mm)
    val time1 = currentTimeSec
    logger.debug(s"[$versionUC] rewrite time: ${time1 - time0}s")
    if (generateFiles) {
      val steps = elevate.core.SuccessRewriteCounter.rewriteCount
      logger.debug(s"[$versionUC] required rewrite steps: $steps\n")
      writeToFile(plotsFolder, version, s"$version,$steps", ".csv")
    }

    // generate the C code
    val time2 = currentTimeSec
    val program = if(openMP) {
      gen.openmp.function(version).asStringFromExpr(rewritten.get)
    } else {
      gen.c.function(version).asStringFromExpr(rewritten.get)
    }
    val time3 = currentTimeSec
    logger.debug(s"[$versionUC] codegen time: ${time3 - time2}s")
    logger.debug(s"Program:\n${program}")

    // store the C code
    if (generateFiles) {
      logger.debug(s"[$versionUC] generated code stored as $version in $kernelsFolder")
      writeToFile(kernelsFolder, version, program)
    }
  }
}
