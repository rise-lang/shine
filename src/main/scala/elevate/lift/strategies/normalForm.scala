package elevate.lift.strategies

import elevate.core.{RewriteResult, Strategy}
import elevate.core.strategies.basic._
import elevate.lift.strategies.traversal._
import elevate.lift.Lift
import elevate.lift.strategies.predicate._
import elevate.lift.rules._
import elevate.lift.rules.algorithmic._
import lift.core.DSL._


object normalForm {

  case object BENF extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = betaEtaNormalForm(e)
    override def toString = "BENF"
  }
  def betaEtaNormalForm: Strategy[Lift] =
    normalize.apply(etaReduction <+ betaReduction)

  case object LCNF extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = lambdaCalculusNormalForm(e)
    override def toString = "LCNF"
  }
  def lambdaCalculusNormalForm: Strategy[Lift] =
    BENF `;` normalize.apply(argumentOf(map, (isNotLambda `;` etaAbstraction)))

  case object RNF extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = rewriteNormalForm(e)
    override def toString = "RNF"
  }
  def rewriteNormalForm: Strategy[Lift] = normalize.apply(LCNF `;` mapLastFission) `;` LCNF

  case object CNF extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = codegenNormalForm(e)
    override def toString = "CNF"
  }
  def codegenNormalForm: Strategy[Lift] = normalize.apply(mapFusion)

}
