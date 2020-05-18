package elevate.rise

import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.debug.peek
import elevate.core.strategies.traversal._
import elevate.rise.rules.traversal._
import elevate.rise.rules.algorithmic._
import elevate.rise.rules.lowering._
import elevate.rise.rules.movement._
import elevate.rise.strategies.tiling._
import elevate.rise.strategies.lowering._
import elevate.rise.strategies.normalForm._
import elevate.rise.strategies.predicate._
import elevate.rise.strategies.traversal._
import shine.test_util
import rise.core.TypedDSL._
import rise.core._
import rise.core.types._
import util.gen

// scalastyle:off
class tvmGemm extends test_util.Tests {

  //// MM INPUT EXPRESSION /////////////////////////////////////////////////////
  val N = 1024
  val mm = infer(
    fun(ArrayType(N, ArrayType(N, f32)))(a =>
      fun(ArrayType(N, ArrayType(N, f32)))(b =>
        map(fun(ak =>
          map(fun(bk =>
            (reduce(add)(l(0.0f)) o
              map(fun(x => fst(x) * snd(x)))) $
              zip(ak, bk))) $ transpose(b))) $ a))
  )

  //// ICFP'20 TVM - STRATEGIES ////////////////////////////////////////////////
  // -- BASELINE ---------------------------------------------------------------

  val baseline: Strategy[Rise] = ( DFNF `;`
    reduceMapFusion `@` topDown[Rise])

  test("baseline") {
    run("baseline", (baseline `;` lowerToC), openMP = false)
  }

  // -- BLOCKING ---------------------------------------------------------------

  def toDot(name: String): Strategy[Rise] = peek(x => exprToDot(name, x))
  def exprToDot(name: String, e: Expr): Unit = exprToDot("/home/bastian/development/rewriting/dot", name, e, dotPrinter(_))
  def exprToDot(path: String, name: String, e: Expr, dot: Expr => String): Unit = {
    import java.io._
    import sys.process._

    val w = new PrintWriter(new File(s"$path/$name.dot"))
    w.write(dot(e))
    w.flush()
    w.close()
    s"dot -Tpdf $path/$name.dot -o $path/$name.pdf".!
  }


    // differences compared to paper:
  // * todo: add 'baseline' reuse to paper
  // * need to fission reduce before `splitting` it
  // * isReduce must be isFullyAppliedReduce
  // * reorder is hardcoded

  val isFullyAppliedReduce: Strategy[Rise] = isApplied(isApplied(isApplied(isReduce)))
  val reorder125634: Strategy[Rise] =
    (mapFBeforeSlide `@` topDown[Rise]) `;;`
      (reduceMapFusion `@` topDown[Rise]) `;;`
      (reduceMapFusion `@` topDown[Rise]) `;;`
      RNF `;` (liftReduce `@` topDown[Rise]) `;;`
      RNF `;` (liftReduce `@` topDown[Rise]) `;;`
      RNF `;` (liftReduce `@` bottomUp[Rise]) `;;`
      RNF `;` (liftReduce `@` bottomUp[Rise])

  val normX = (mapFBeforeSlide `@` topDown[Rise]) `;;`
    (reduceMapFusion `@` topDown[Rise]) `;;`
    (reduceMapFusion `@` topDown[Rise])
  val blocking: Strategy[Rise] =
    (baseline `;` // <- not in paper
      (tile(32,32)      `@` outermost(mapNest(2))) `;;`
      (reduceMapFission `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
      (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;`
      //normX `;;`
      //reorder(List(1,2,3,4,5,6)))
      reorder125634)

  //case class reorder(l: List[Int]) extends Strategy[Rise] {
  //  def apply(e: Rise): RewriteResult[Rise] = l match {
  //    case x :: xs if x == 1 => stepDown(reorder(xs.map(_-1)))(e)
  //    case _ => Failure(reorder(l))
  //  }

  //  def stepDown(s: Strategy[Rise]): Strategy[Rise] =
  //    function(function(argumentOf(Reduce(), s))) <+
  //    function(function(argumentOf(ReduceSeq(), s))) <+
  //    function(argumentOf(primitives.Map(), s))
  //}

  test("blocking") {
    run("blocking", (blocking `;` lowerToC), openMP = false)
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

  // differences compared to paper:
  // * see blocking version (different loop perm used here but also hardcoded)

  val reorder125364: Strategy[Rise] =
    (mapFBeforeSlide `@` topDown[Rise]) `;;`
      (reduceMapFusion `@` topDown[Rise]) `;;`
      (reduceMapFusion `@` topDown[Rise]) `;;`
      RNF `;` (liftReduce `@` topDown[Rise]) `;;`
      RNF `;` (liftReduce `@` topDown[Rise]) `;;`
      RNF `;` (liftReduce `@` bottomUp[Rise])

  val loopPerm: Strategy[Rise] = baseline `;`
    (tile(32,32)      `@` outermost(mapNest(2))) `;;`
    (reduceMapFission `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
    (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;`
    reorder125364 `;;`
    (vectorize(32) `@` innermost(isFullyAppliedMap))

  test("loop permutation") {
    run("loop_permutation", (loopPerm `;` lowerToC), openMP = true)
  }

  // -- ARRAY PACKING ----------------------------------------------------------

  // difference compared to the paper:
  // * use of inLambda
  // pattern matching RewriteResult
  // * instead of fun(x => e) we build our own lambda with idx


  val isTransposedB: Strategy[Rise] = isApplied(isTranspose)
  val permuteB: Strategy[Rise] =
    splitJoin2(32) `;` DFNF `;` argument(idAfter) `;`
      topDown(liftId) `;` topDown(createTransposePair) `;` RNF `;`
      argument(argument(idAfter)) `;` normalize.apply(liftId) `;`
      topDown(idToCopy)

  val packB: Strategy[Rise] =
    storeInMemory(isTransposedB,
      permuteB `;;` // <- todo: move permuteB here in the paper as well
        (vectorize(32) `@` innermost(isVectorizeablePrimitive)) `;;`
        (parallel      `@` outermost(isApplied(isMap)))
    ) `@` inLambda

  def inLambda(s: Strategy[Rise]): Strategy[Rise] =
    isLambda `;` ((e: Rise) => body(inLambda(s))(e)) <+ s

  val arrayPacking: Strategy[Rise] = packB `;;` loopPerm
  test("array packing") {
    run("array_packing", (arrayPacking `;` lowerToC), openMP = true)
  }

  // -- CACHE BLOCKS -----------------------------------------------------------

  val cacheBlocks: Strategy[Rise] = (
    arrayPacking `;;`
      (unroll `@` innermost(isReduceSeq))
    )

  test("cache blocks") {
    run("cache_blocks", (cacheBlocks `;` lowerToC), openMP = true)
  }

  // -- PARALLEL ---------------------------------------------------------------

  val par = (
    arrayPacking `;;`
      ((parallel `@` outermost(isApplied(isMap))) `@` outermost(isApplied(isLet))) `;;`
      (unroll `@` innermost(isReduceSeq))
    )

  test("parallel") {
    run("parallel", (par `;` lowerToC), openMP = true)
  }


  /// UTILS ////////////////////////////////////////////////////////////////////

  val kernelsFolder: String = "/home/artifact/kernels"
  val plotsFolder: String = "/home/artifact/plots/steps"

  def run(version: String,
          strategy: Strategy[Rise],
          openMP: Boolean // generate C or OpenMP code?
         ): Unit = {

    def toFile(path: String, name: String, content: String, ending: String = ".c"): Unit = {
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
    val rewritten = strategy(mm)
    val time1 = currentTimeSec
    println(s"[$versionUC] rewrite time: ${time1 - time0}s")
    val steps = Success.rewriteCount
    println(s"[$versionUC] required rewrite steps: $steps\n")
    toFile(plotsFolder, version, s"$version,$steps", ".csv")

    // generate the C code
    val time2 = currentTimeSec
    val program = if(openMP) {
      gen.OpenMPProgram(rewritten, version).code
    } else {
      gen.CProgram(rewritten, version).code
    }
    val time3 = currentTimeSec
    println(s"[$versionUC] codegen time: ${time3 - time2}s")

    // store the C code
    println(s"[$versionUC] generated code stored as $version in $kernelsFolder")
    toFile(kernelsFolder, version, program)
  }
}
