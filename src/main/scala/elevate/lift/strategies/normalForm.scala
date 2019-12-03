package elevate.lift.strategies

import elevate.core.{Failure, Lift, RewriteResult, Strategy, Success}
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.lift.strategies.traversal._
import elevate.core.strategies.predicate._
import elevate.lift.strategies.predicate._
import elevate.lift.strategies.algorithmic._
import elevate.lift.rules._
import elevate.lift.rules.algorithmic._
import lift.core.{Expr, Lambda}
import lift.core.TypedDSL._


object normalForm {

  case object BENF extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = betaEtaNormalForm(e)
    override def toString = "BENF"
  }
  def betaEtaNormalForm: Strategy[Lift] =
    normalize(etaReduction <+ betaReduction)

  case object LCNF extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = lambdaCalculusNormalForm(e)
    override def toString = "LCNF"
  }
  def lambdaCalculusNormalForm: Strategy[Lift] =
    BENF `;` normalize(argumentOf(lift.core.primitives.Map()(), (isNotLambda `;` etaAbstraction)))

  case object RNF extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = rewriteNormalForm(e)
    override def toString = "RNF"
  }
  def rewriteNormalForm: Strategy[Lift] = normalize(LCNF `;` mapLastFission) `;` LCNF

  case object CNF extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = codegenNormalForm(e)
    override def toString = "CNF"
  }
  def codegenNormalForm: Strategy[Lift] = normalize(mapFusion)

}
