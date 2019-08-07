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
import lift.core.primitives.map


object normalForm {

  def normalize: Strategy[Lift] => Strategy[Lift] =
    s => repeat(oncetd(s))

  //def BENF: Strategy[Lift] = betaEtaNormalForm
  def BENF = BENFCaseClass()
  case class BENFCaseClass() extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = betaEtaNormalForm(e)
    override def toString = "BENF"
  }
  def betaEtaNormalForm: Strategy[Lift] =
    normalize(etaReduction <+ betaReduction)

  //def LCNF: Strategy[Lift] = lambdaCalculusNormalForm
  def LCNF = LCNFCaseClass()
  case class LCNFCaseClass() extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = lambdaCalculusNormalForm(e)
    override def toString = "LCNF"
  }
  def lambdaCalculusNormalForm: Strategy[Lift] =
    BENF `;` normalize(argumentOf(map)(isNotLambda `;` etaAbstraction))

  //def RNF: Strategy[Lift] = rewriteNormalForm
  def RNF = RNFCaseClass()
  case class RNFCaseClass() extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = rewriteNormalForm(e)
    override def toString = "RNF"
  }
  def rewriteNormalForm: Strategy[Lift] = normalize(LCNF `;` mapLastFission) `;` LCNF

  //def CNF: Strategy[Lift] = codegenNormalForm
  def CNF = CNFCaseClass()
  case class CNFCaseClass() extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = codegenNormalForm(e)
    override def toString = "CNF"
  }
  def codegenNormalForm: Strategy[Lift] = normalize(mapFusion)

}
