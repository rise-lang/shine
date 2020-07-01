package elevate.rise

import arithexpr.arithmetic.SizeVar
import elevate.core.Strategy
import elevate.core.strategies.debug.peek
import elevate.core.strategies.traversal._
import elevate.rise.rules.traversal._
import elevate.rise.rules.algorithmic._
import elevate.rise.rules.lowering.lowerToC
import elevate.rise.rules.movement._
import elevate.rise.strategies.tiling._
import elevate.rise.strategies.normalForm._
import shine.test_util
import rise.core.dotPrinter._
import rise.core.TypedDSL._
import rise.core.types.{ArrayType, f32, infer}
//import shine.DPIA.Types.TypeCheck
import util.gen

class tvmGemm extends test_util.Tests {

  val N = 1024
  val mm = infer(
    nFun(N=>
    fun(ArrayType(N, ArrayType(N, f32)))(a =>
      fun(ArrayType(N, ArrayType(N, f32)))(b =>
        map(fun(ak =>
          map(fun(bk =>
            (reduce(add)(l(0.0f)) o
              map(fun(x => fst(x) * snd(x)))) $
            zip(ak, bk))) $ transpose(b) )) $ a)))
  )

  // todo remove before PR
  def toDot(n: String): Strategy[Rise] = peek(x => exprToDot(n, x))

  // utils
  def currentTimeSec: Long = System.currentTimeMillis / 1000

  // *** BASELINE **************************************************************

  test("baseline") {
    val time0 = currentTimeSec
    val result = (LCNF `;` oncetd.apply(fuseReduceMap) `;` lowerToC)(mm)
    val time1 = currentTimeSec
    println(time1 - time0)

    val time2 = currentTimeSec
    println(gen.CProgram(result))
    val time3 = currentTimeSec
    println(time3 - time2)
  }

  // *** BLOCKING **************************************************************

  // prepare tiling, keep reduce(+) and map(*) close together, (necessary?)
  val fusedReduceMap: Strategy[Rise] = LCNF `;` oncetd(fuseReduceMap)

  // M.N.m.n.K (or in TVM terms: xo,yo,xi,yi,k)
  val tiledOuterTwo: Strategy[Rise] = fusedReduceMap `;`
    oncetd(tileNDList(List(32,32))) `;` LCNF

  // M.N.m.n.K.k (tile K-loop),
  // fission first to enable blocking the reduction loop
  val splitK: Strategy[Rise] = tiledOuterTwo `;`
    oncetd(fissionReduceMap) `;` oncetd(blockedReduce(SizeVar("N"))) `;` LCNF

  // move the split (blocking the reduction loop)
  // to prepare fusing map(*) and reduce(+) again
  val prepareFusion: Strategy[Rise] = splitK `;`
    oncetd(mapFBeforeSlide) `;` LCNF

  // move map(*) into both reduce loops again
  val fusedReduceMapAgain: Strategy[Rise] = prepareFusion `;`
    oncetd(fuseReduceMap) `;` LCNF `;` oncetd(fuseReduceMap) `;` LCNF

  // move outer K loop up M.N.m.K.n.k
  val moveOuterKLoopOnce: Strategy[Rise] = fusedReduceMapAgain `;`
    RNF `;` oncetd(liftReduce) `;` LCNF

  // move outer K loop further up M.N.K.m.n.k
  val moveOuterKLoopTwice: Strategy[Rise] = moveOuterKLoopOnce `;`
    RNF `;` oncetd(liftReduce) `;` LCNF

  // move inner K loop further up M.N.K.m.k.n,
  val moveInnerKLoopOnce: Strategy[Rise] = moveOuterKLoopTwice `;`
    RNF `;` oncebu(liftReduce) `;` LCNF

  // move inner K loop further up M.N.K.k.m.n,
  val blocking : Strategy[Rise] = moveInnerKLoopOnce `;`
    RNF `;` oncebu(liftReduce)

  test("blocking") {
    // try all steps
    gen.CProgram((fusedReduceMap `;` lowerToC)(mm))
    gen.CProgram((tiledOuterTwo `;` lowerToC)(mm))
    gen.CProgram((splitK `;` lowerToC)(mm))
    gen.CProgram((prepareFusion `;` lowerToC)(mm))
    gen.CProgram((fusedReduceMapAgain `;` lowerToC)(mm))
    gen.CProgram((moveOuterKLoopOnce `;` lowerToC)(mm))
    gen.CProgram((moveOuterKLoopTwice `;` lowerToC)(mm))
    gen.CProgram((moveInnerKLoopOnce `;` lowerToC)(mm))
    gen.CProgram((blocking `;` lowerToC)(mm))
  }

  test("keep parameter") {
    val lowering = elevate.rise.rules.lowering.lowerToC

    println("plainMM: " + mm)

    val result = (LCNF `;` oncetd(rules.algorithmic.splitJoin(SizeVar("N"))) `;` LCNF).apply(mm)
    println("splitJoinMM: " + result)

    val result2 = infer(result)
    println("result2: " + result2)

    val lowered = lowering.apply(result2)
    println("loweredMM: " + lowered)

    val lowered2 = infer(lowered)

    val code = gen.CProgram(lowered2)
    println("parameters: " + code.inputParams)
    println("function: " + code.function)
    println("decls: " + code.decls)
    println("code: " + code.code)
  }

  test("keep parameter error") {
    val lowering = elevate.rise.rules.lowering.lowerToC

    println("plainMM: " + mm)

    val tmp = (LCNF `;` oncetd(rules.algorithmic.splitJoin(SizeVar("N"))) `;` LCNF).apply(mm)
    val result = (LCNF `;` oncetd(rules.algorithmic.splitJoin(SizeVar("N"))) `;` LCNF).apply(tmp)

    println("splitJoinMM: " + result)

    val result2 = infer(result)
    println("result2: " + result2)

    val lowered = lowering.apply(result2)
    println("loweredMM: " + lowered)

    val lowered2 = infer(lowered)

    val code = gen.CProgram(lowered2)
    println("parameters: " + code.inputParams)
    println("function: " + code.function)
    println("decls: " + code.decls)
    println("code: " + code.code)
  }

}
