package elevate.rise

import elevate.core.Strategy
import elevate.core.strategies.debug.peek
import elevate.core.strategies.traversal._
import elevate.rise.rules.traversal._
import elevate.rise.rules.algorithmic._
import elevate.rise.rules.lowering.lowerToC
import elevate.rise.rules.movement._
import elevate.rise.strategies.tiling._
import elevate.rise.strategies.normalForm.{LCNF, RNF}
import rise.core.dotPrinter._
import rise.core.TypedDSL._
import rise.core.types.{ArrayType, float, infer}
import util.gen

class tvmGemm extends test_util.Tests {

  val N = 2048 // todo change later to 1024
  val mm = infer(
    fun(ArrayType(N, ArrayType(N, float)))(a =>
      fun(ArrayType(N, ArrayType(N, float)))(b =>
        map(fun(ak =>
          map(fun(bk =>
            (reduce(add)(l(0.0f)) o
              map(fun(x => fst(x) * snd(x)))) $
            zip(ak, bk))) $ transpose(b) )) $ a))
  )

  // todo remove before PR
  def toDot(n: String): Strategy[Rise] = peek(x => exprToDot(n, x))

  // ***************************************************************************

  test("baseline") {
    val time0 = System.currentTimeMillis / 1000
    val result = (LCNF `;` oncetd.apply(fuseReduceMap) `;` lowerToC)(mm)
    val time1 = System.currentTimeMillis / 1000
    println(time1 - time0)

    val time2 = System.currentTimeMillis / 1000
    println(gen.CProgram(result))
    val time3 = System.currentTimeMillis / 1000
    println(time3 - time2)
  }

  // ***************************************************************************

  // original loop-nest: M.N.K
  val blocking = // general strategy: tile all loops first then reorder
  // prepare tiling, keep reduce(+) and map(*) close together, (necessary?)
    LCNF `;` oncetd(fuseReduceMap) `;`
      // M.N.m.n.K (or in TVM terms: xo,yo,xi,yi,k)
      oncetd(tileNDList(List(32,32))) `;` LCNF `;`
      // M.N.m.n.K.k (tile K-loop),
      // fission first to enable blocking the reduction loop
      oncetd(fissionReduceMap) `;` oncetd(blockedReduce(4)) `;` LCNF `;`
      // move the split (blocking the reduction loop)
      // to prepare fusing map(*) and reduce(+) again
      oncetd(mapFBeforeSlide) `;` LCNF `;`
      // move map(*) into both reduce loops again
      oncetd(fuseReduceMap) `;` LCNF `;` oncetd(fuseReduceMap) `;` LCNF `;`
      // move outer K loop further up M.N.K.m.n.k
      RNF `;` oncetd(liftReduceOld) `;` LCNF `;`
      RNF `;` oncetd(liftReduceOld) `;` LCNF `;` toDot("left") `;`
      // move inner K loop further up M.N.K.k.m.n, inner reduction is now part of the outer-reduction operator
      oncetd(liftReduceInReduceOperator)// `;` LCNF `;`
      oncetd(liftReduceInReduceOperator)

  test("blocking") {
    val result = (blocking `;` lowerToC)(mm)
    println(result)
    gen.CProgram(result)
  }

}
