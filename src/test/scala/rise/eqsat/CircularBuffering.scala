package rise.eqsat

import rise.elevate.circularBuffering._
import ProveEquiv.syntax._

class CircularBuffering extends test_util.Tests {
  test("highLevel to inlined") {
    proveEquiv(wrapExpr(highLevel), wrapExpr(inlined), Seq(
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      rules.reduceSeq, rules.mapSeqUnrollMapSeqWrite
    ))
  }

  test("highLevel to buffered") {
    proveEquiv(wrapExpr(highLevel), wrapExpr(inlined), Seq(
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      rules.mapSeq, rules.toMemAfterMapSeq, rules.reduceSeq, rules.mapSeqUnrollMapSeqWrite
    ))
  }

  test("highLevel to circBuf") {
    proveEquiv(wrapExpr(highLevel), wrapExpr(circBuf), Seq(
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      rules.mapFusion,
      rules.takeBeforeMap, rules.dropBeforeMap,
      rules.takeInSlide, rules.dropInSlide,
      rules.dropBeforeTake,
      rules.reduceSeq, rules.mapSeqUnrollWrite,
      rules.mapOutsideMakeArray2, // original: mapOutsideMakeArray
      rules.circularBuffer, // original: circularBuffer(id)
      rules.iterateStream
    ))
  }

  test("highLevelChain to inlinedChain") {
    proveEquiv(wrapExprChain(highLevelChain), wrapExprChain(inlinedChain), Seq(
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      rules.reduceSeq, rules.mapSeq
    ))
  }

  test("highLevelChain to circBufChain") {
    proveEquiv(wrapExprChain(highLevelChain), wrapExprChain(circBufChain), Seq(
      rules.eta, rules.betaExtract, rules.betaNatExtract, rules.reduceSeq,
      rules.circularBuffer, rules.circularBufferLoadFusion, rules.iterateStream
    ))
  }

  test("highLevelTogether to inlinedTogether") {
    proveEquiv(wrapExprTogether(highLevelTogether), wrapExprTogether(inlinedTogether), Seq(
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      rules.reduceSeq, rules.mapSeq
    ))
  }

  /* TODO: find out which rewrite rules are necessary, not done with Elevate either
  test("highLevelTogether to circBufTogether") {
    proveEquiv(wrapExprTogether(highLevelTogether), wrapExprTogether(circBufTogether), Seq(
      rules.eta, rules.beta, rules.betaNat, rules.reduceSeq, rules.mapSeq,
      rules.circularBuffer, rules.circularBufferLoadFusion, rules.iterateStream
    ))
  } */

  def proveEquiv(start: rise.core.Expr,
                 goal: rise.core.Expr,
                 rules: Seq[Rewrite]): Unit = {
    import rise.elevate.rules._
    import rise.elevate.rules.traversal.alternative._
    import elevate.core.strategies.basic.normalize

    val normGoal = normalize.apply(gentleBetaReduction() <+ etaReduction())(goal).get
    println(s"normalized goal: $normGoal")

    ProveEquiv.init().run(Expr.fromNamed(start), Expr.simplifyNats(Expr.fromNamed(normGoal)), rules)
  }
}
