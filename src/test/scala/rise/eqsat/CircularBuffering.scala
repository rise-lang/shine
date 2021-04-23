package rise.eqsat

import rise.elevate.circularBuffering._
import Basic.proveEquiv

class CircularBuffering extends test_util.Tests {
  test("highLevel to circBuf") {
    import rise.core.primitives._
    import rise.core.DSL._
    import rise.core.DSL.HighLevelConstructs.dropLast

    val sum = reduce(add)(lf32(0.0f))
    val goal = // circBuf
      slide(3)(1) >> map(sum) >> fun(x =>
      makeArray(2)(
        x |> slide(2)(1) >> drop(1) >> dropLast(1) >> map(sum))(
        x |> slide(4)(1) >> map(sum)
      ))
    // FIXME: there is a bug here
    proveEquiv(wrapExpr(highLevel), wrapExpr(goal), Seq(
      // rules.eta, rules.beta, rules.betaNat,
      // rules.mapFusion,
      rules.takeBeforeMap, rules.dropBeforeMap,
      /* rules.takeInSlide, rules.dropInSlide,
      rules.dropBeforeTake,
      rules.reduceSeq, rules.mapSeqUnroll, // original: mapSeqUnrollWrite
      rules.mapOutsideMakeArray2, // original: mapOutsideMakeArray
      rules.circularBufferScalar, // original: circularBuffer(id)
      rules.iterateStream */
    ))
  }
}