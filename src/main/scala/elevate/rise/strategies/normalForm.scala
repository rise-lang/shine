package elevate.rise.strategies

import elevate.core.{RewriteResult, Strategy}
import elevate.core.strategies.basic._
import elevate.core.strategies.predicate._
import elevate.core.strategies.traversal.one
import elevate.rise.rules.traversal._
import elevate.rise.Rise
import elevate.rise.strategies.predicate._
import elevate.rise.rules._
import elevate.rise.rules.algorithmic._
import elevate.rise.rules.traversal.{argumentOf, body, function}
import lift.core.DSL._


object normalForm {

  // Beta-Eta-Normal-Form
  case object BENF extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = normalize.apply(etaReduction <+ betaReduction)(e)
    override def toString = "BENF"
  }

  case object LCNF extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = lambdaCalculusNormalForm(e)
    override def toString = "LCNF"
  }
  def lambdaCalculusNormalForm: Strategy[Rise] =
    BENF `;`
      // there is no argument of a map which is not eta-abstracted, i.e., every argument of a map is a lambda
      normalize.apply(argumentOf(map, (not(isLambda) `;` etaAbstraction))) `;`
      // a reduce always contains two lambdas declaring y and acc
      normalize.apply(argumentOf(reduce, (not(isLambda) `;` etaAbstraction))) `;`
      normalize.apply(argumentOf(reduce, body((not(isLambda) `;` etaAbstraction)))) `;`
      // there is no map(f) without an argument == there is no way to get to a map without visiting two applies
      // same for reduce and three applies
      normalize.apply(
        one(function(isMap) <+ one(function(isReduce))) `;`                  // there is a map in two hops, i.e, Something(Apply(map, f))
          not(isApply) `;`                                                      // and the current node is not an Apply i.e. Something != Apply
          one((function(isMap) <+ one(function(isReduce))) `;` etaAbstraction)   // eta-abstract the inner Apply
      )

  case object RNF extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = rewriteNormalForm(e)
    override def toString = "RNF"
  }
  def rewriteNormalForm: Strategy[Rise] = normalize.apply(LCNF `;` mapLastFission) `;` LCNF

  case object CNF extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = codegenNormalForm(e)
    override def toString = "CNF"
  }
  def codegenNormalForm: Strategy[Rise] = normalize.apply(mapFusion)

}
