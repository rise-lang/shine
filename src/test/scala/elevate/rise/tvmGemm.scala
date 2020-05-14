package elevate.rise

import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.rise.rules.traversal._
import elevate.rise.rules.algorithmic._
import elevate.rise.rules.lowering._
import elevate.rise.rules.movement._
import elevate.rise.strategies.tiling._
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

  val blocking: Strategy[Rise] =
    (baseline `;` // <- not in paper
      (tile(32,32)      `@` outermost(mapNest(2))) `;;`
      (reduceMapFission `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
      (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;`
      reorder125634)

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

  def storeInMemory(what: Strategy[Rise],
                    how: Strategy[Rise]): Strategy[Rise] = { p =>
    extract(what)(p) >>= (extracted => {
      how(extracted) >>= (storedSubExpr => {
        val idx = Identifier(freshName("x"))(extracted.t)

        replaceAll(what, idx)(p) match {
          case Success(replaced) => Success(toMem(storedSubExpr)(lambda(TDSL(idx), replaced)))
          case Failure(_) => Failure(storeInMemory(what, how))
        }
      })
    })
  }


  def replaceAll(exprPredicate: Strategy[Rise], withExpr: Rise): Strategy[Rise] =
    p => tryAll(exprPredicate `;` insert(withExpr)).apply(p)

  def toMem(e: Rise)(f: TDSL[Lambda]): TDSL[App] = let(f)(e)
  def insert(expr: Rise): Strategy[Rise] = _ => Success(expr)
  def extract(what: Strategy[Rise]): Strategy[Rise] = (expr: Rise) => {
    what(expr).flatMapFailure(_ => expr match {
      case App(f,e)        => extract(what)(f).flatMapFailure(_ => extract(what)(e))
      case Lambda(x, e)    => extract(what)(x).flatMapFailure(_ => extract(what)(e))
      case DepLambda(x, e) => extract(what)(e)
      case _: Identifier      => Failure(extract(what))
      case _: Literal         => Failure(extract(what))
      case _: ForeignFunction => Failure(extract(what))
      case _: Primitive       => Failure(extract(what))
      case _ => ??? // forgot something?
    })
  }

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

  def run(version: String,
          strategy: Strategy[Rise],
          openMP: Boolean // generate C or OpenMP code?
         ): Unit = {

    def programToFile(path: String, name: String, program: String): Unit = {
      import java.io._
      val w =new PrintWriter(new File(s"$path/$name.c"))
      w.write(program)
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
    println(s"[$versionUC] required rewrite steps: ${Success.rewriteCount}") // 658

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
    programToFile(kernelsFolder, version, program)
  }
}
