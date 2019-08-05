package elevate.lift.strategies

import elevate.core.{Failure, Lift, Strategy, Success}
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

  def BENF: Strategy[Lift] = betaEtaNormalForm
  def betaEtaNormalForm: Strategy[Lift] = normalize(betaReduction <+ etaReduction)

  def LCNF: Strategy[Lift] = lambdaCalculusNormalForm
  def lambdaCalculusNormalForm: Strategy[Lift] =
    BENF `;` tryAll(argumentOf(map)(isLambda <+ etaAbstraction))

  def RNF: Strategy[Lift] = rewriteNormalForm
  def rewriteNormalForm: Strategy[Lift] = normalize(mapLastFission) `;` LCNF

  def CNF: Strategy[Lift] = codegenNormalForm
  def codegenNormalForm: Strategy[Lift] = normalize(mapFusion)

}
