package rise.elevate

import elevate.core._
import elevate.core.strategies.basic._
import rise.core.HighLevelConstructs.{padClamp2D, padCst2D, slide2D}
//import elevate.core.strategies.debug.peek
//import rise.core.IsClosedForm
import _root_.util.gen
import elevate.core.strategies.debug.debug
import elevate.core.strategies.traversal._
import rise.core.TypedDSL._
import rise.core.primitives._
import rise.core.types._
import rise.elevate.rules.algorithmic._
//import rise.elevate.rules.lowering._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.algorithmic.reorder
import rise.elevate.strategies.lowering._
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.predicate._
import rise.elevate.strategies.tiling._
import rise.elevate.strategies.traversal
import rise.elevate.strategies.traversal._

// scalastyle:off
class gauss extends test_util.Tests {

  val outermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.outermost(default.RiseTraversable)
  val innermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.innermost(default.RiseTraversable)

  //// MM INPUT EXPRESSION /////////////////////////////////////////////////////

  val weights = List(
    List(2,4,5,4,2),
    List(4,9,12,9,4),
    List(5,12,15,12,5),
    List(4,9,12,9,4),
    List(2,4,5,4,2)
  )

  val N = 1024
  val M = 1024


  val gauss: Rise = //infer(
    fun(ArrayType(N, ArrayType(M, f32)))(in =>
      fun(ArrayType(5, ArrayType(5, f32)))(weights =>
        in |> padClamp2D(2) // in: NxM -> (N+4) x (M+4)
          |> slide2D(5,2) // -> M*Nx5x5, Matrix of 5x5 slides, each centered around original value
          |> mapSeq(fun(sector => // sector:
            zip(sector)(weights) //
            |> mapSeq(fun(iwrowPair =>
              zip(fst(iwrowPair))(snd(iwrowPair))
              |> mapSeq(fun(iwpair =>
                fst(iwpair) * snd(iwpair)
              )) |> reduceSeq(add)(l(0.0f))
            )) |> reduceSeq(add)(l(0.0f))
        ))
      )
    )

  //// ICFP'20 TVM - STRATEGIES ////////////////////////////////////////////////
  // -- BASELINE ---------------------------------------------------------------

  val baseline: Strategy[Rise] = DFNF()(default.RiseTraversable) `;`
    normalize.apply(fuseReduceMap `@` topDown[Rise])

  test("baseline") {
    //val tmp = DFNF()(default.RiseTraversable).apply(gauss)
    //println(tmp)
    //val tmp2 = normalize.apply(fuseReduceMap).apply(tmp.get)
    //println(tmp2)
    //run("baseline",  lowerToC, openMP = false)

    println(gen.CProgram(gauss))
  }
/*
  // -- BLOCKING ---------------------------------------------------------------

  val isFullyAppliedReduce: Strategy[Rise] = isApplied(isApplied(isApplied(isReduce)))
  val blocking: Strategy[Rise] =
    baseline `;`
      (tile(32,32)        `@` outermost(mapNest(2))) `;;`
      (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
      (splitStrategy(4)   `@` innermost(isFullyAppliedReduce)) `;;`
      reorder(List(1,2,5,6,3,4))

  test("blocking") {
    run("blocking", blocking `;` lowerToC, openMP = false)
  }

  // -- VECTORIZATION ----------------------------------------------------------

  val isFullyAppliedMap: Strategy[Rise] = isApplied(isApplied(isMap))
  val vectorization: Strategy[Rise] =
    blocking `;;`
      (vectorize(32) `@` innermost(isApplied(isApplied(isMap))))

  test("vectorization") {
    run("vectorization", (vectorization `;` lowerToC), openMP = true)
  }

  // -- LOOP PERMUTATION -------------------------------------------------------

  val loopPerm: Strategy[Rise] = baseline `;`
    (tile(32,32)        `@` outermost(mapNest(2))) `;;`
    (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
    (splitStrategy(4)   `@` innermost(isFullyAppliedReduce)) `;;`
    reorder(List(1,2,5,3,6,4)) `;;`
    (vectorize(32) `@` innermost(isFullyAppliedMap))

  test("loop permutation") {
    run("loop_permutation", (loopPerm `;` lowerToC), openMP = true)
  }

  // -- ARRAY PACKING ----------------------------------------------------------

  val isTransposedB: Strategy[Rise] = isApplied(isTranspose)
  val permuteB: Strategy[Rise] =
      splitJoin2(32) `;` DFNF() `;` argument(idAfter) `;`
      topDown(liftId()) `;` topDown(createTransposePair) `;` RNF() `;`
      argument(argument(idAfter)) `;` normalize.apply(liftId()) `;`
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
  test("array packing") {
    run("array_packing", arrayPacking `;` lowerToC, openMP = true)
  }

  // -- CACHE BLOCKS -----------------------------------------------------------

  val cacheBlocks: Strategy[Rise] = (
    arrayPacking `;;` debug[Rise]("after arrayPacking") `;`
      (unroll `@` innermost(isReduceSeq))
    )

  test("cache blocks") {
    run("cache_blocks", cacheBlocks `;` lowerToC, openMP = true)
  }

  // -- PARALLEL ---------------------------------------------------------------

  val par = (
    arrayPacking `;;`
      ((parallel() `@` outermost(isApplied(isMap))) `@`
        outermost(isApplied(isLet))) `;;`
      (unroll `@` innermost(isReduceSeq))
    )

  test("parallel") {
    run("parallel", par `;` lowerToC, openMP = true)
  }
*/

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
    Success.rewriteCount = 0

    // rewrite the matmul input expresssion
    val time0 = currentTimeSec
    val rewritten = strategy(gauss)
    val time1 = currentTimeSec
    println(s"[$versionUC] rewrite time: ${time1 - time0}s")
    if (generateFiles) {
      val steps = Success.rewriteCount
      println(s"[$versionUC] required rewrite steps: $steps\n")
      writeToFile(plotsFolder, version, s"$version,$steps", ".csv")
    }

    // generate the C code
    val time2 = currentTimeSec
    val program = if(openMP) {
      gen.OpenMPProgram(rewritten.get, version).code
    } else {
      gen.CProgram(rewritten.get, version).code
    }
    val time3 = currentTimeSec
    println(s"[$versionUC] codegen time: ${time3 - time2}s")
    println(s"Program:\n${program}")

    // store the C code
    if (generateFiles) {
      println(s"[$versionUC] generated code stored as $version in $kernelsFolder")
      writeToFile(kernelsFolder, version, program)
    }
  }
}
