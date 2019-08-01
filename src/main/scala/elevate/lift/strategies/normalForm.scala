package elevate.lift.strategies

import elevate.core.{Failure, Strategy, Success}
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.lift.strategies.traversal._
import elevate.lift.strategies.predicate._
import elevate.lift.strategies.algorithmic._
import elevate.lift.rules._
import elevate.lift.rules.algorithmic._
import lift.core.{Expr, Lambda}
import lift.core.primitives.map


object normalForm {

  def normalize: Strategy => Strategy =
    s => repeat(oncetd(s))

  def BENF: Strategy = betaEtaNormalForm
  def betaEtaNormalForm: Strategy = normalize(betaReduction <+ etaReduction)

  def LCNF: Strategy = lambdaCalculusNormalForm
  def lambdaCalculusNormalForm: Strategy =
    BENF `;` tryAll(argumentOf(map)(isLambda <+ etaAbstraction))

  def RNF: Strategy = rewriteNormalForm
  def rewriteNormalForm: Strategy = normalize(mapLastFission) `;` LCNF

  def CNF: Strategy = codegenNormalForm
  def codegenNormalForm: Strategy = normalize(mapFusion)
}
