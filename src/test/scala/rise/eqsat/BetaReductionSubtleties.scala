package rise.eqsat

import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.primitives._
import rise.core.types.DataType._
import ProveEquiv.syntax._

class BetaReductionSubtleties extends test_util.Tests {
  test("incomplete beta-reduction") {
    /*
      * NOTE (Thomas)
      *
      * An example where implementing beta reduction using extraction works or not
      * depending on rewrite rule scheduling:
      * ```
      * (\x. transpose (transpose x)) in --beta-->
      * transpose(transpose(in))
      * ```
      * An example of problematic sequence being:
      * ```
      * (\x. transpose (transpose x)) in --removeTransposePair-->
      * (\x. x) in --beta-->
      * in
      * ```
      * Where if we think about the e-graph as a set we would expect this rewrite sequence
      * to represent `transpose(transpose(in))` as well, but it is not the case with an extraction
      * implementation. If we represent what happens with sets:
      * - extraction:
      * ```
      * (\x. transpose (transpose x)) in --removeTransposePair-->
      * (\x. {transpose (transpose x)), x}) in --beta-->
      * {in,  (\x. {transpose (transpose x)), x}) in}
      * ```
      * - expected set behaviour:
      * ```
      * (\x. transpose (transpose x)) in --removeTransposePair-->
      * (\x. {transpose (transpose x)), x}) in --beta-->
      * {transpose(transpose(in)), in,  (\x. {transpose (transpose x)), x}) in}
      * ```
      *
      * So far I have not found any practical case where this is a problem for us because
      * 1. with the current naive scheduling we apply all rules at all times,
      *    so the problematic state is not reached
      * 2. in most cases BENF is what we want
      *
      * I believe that having a non-extraction based beta reduction would behave best
      * in terms of correctness. However I would not be surprised if performance suffers a bit.
      */
    val in = generate(fun(_ => generate(fun(_ => lf32(0.0f))))) :: (1`.`2`.`f32)
    val start = Expr.fromNamed(
      fun(x => transpose(transpose(x)))(in))
    val goal = Expr.fromNamed(
      transpose(transpose(in)))

    // extraction works with parallel application
    // ProveEquiv.init().run(start, goal, Seq(rules.beta, rules.removeTransposePair))
    ProveEquiv.init().run(start, goal, Seq(rules.betaExtract, rules.removeTransposePair), Seq())

    // extraction does not work with sequential application
    def seqCheck(betaRule: Rewrite): Boolean = {
      val egraph = EGraph.empty()
      egraph.requireAnalyses(betaRule.requiredAnalyses())

      val startId = egraph.addExpr(start)
      egraph.rebuild(Seq(startId))
      for (_ <- 0 until 4) {
        val shc = SubstsHC.empty
        rules.removeTransposePair.apply(egraph, shc)(
          rules.removeTransposePair.search(egraph, shc))
        egraph.rebuild(Seq(startId))
        betaRule.apply(egraph, shc)(betaRule.search(egraph, shc))
        egraph.rebuild(Seq(startId))
      }
      val shc = SubstsHC.empty
      Pattern.fromExpr(goal).compile()
        .searchEClass(egraph, shc, startId).isDefined
    }
    // assert(seqCheck(rules.beta))
    assert(!seqCheck(rules.betaExtract))
  }
}
