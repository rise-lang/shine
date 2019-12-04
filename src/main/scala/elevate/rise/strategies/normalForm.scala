package elevate.rise.strategies

import elevate.core.{RewriteResult, Strategy}
import elevate.core.strategies.basic._
import elevate.rise.strategies.traversal._
import elevate.rise.Rise
import elevate.rise.strategies.predicate._
import elevate.rise.rules._
import elevate.rise.rules.algorithmic._
import lift.core.DSL._


object normalForm {

  case object BENF extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = betaEtaNormalForm(e)
    override def toString = "BENF"
  }
  def betaEtaNormalForm: Strategy[Rise] =
    normalize.apply(etaReduction <+ betaReduction)

  case object LCNF extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = lambdaCalculusNormalForm(e)
    override def toString = "LCNF"
  }
  def lambdaCalculusNormalForm: Strategy[Rise] =
    BENF `;` normalize.apply(argumentOf(map, (isNotLambda `;` etaAbstraction)))

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
