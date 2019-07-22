package elevate.lift.strategies

import elevate.core.Strategy
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.lift.strategies.traversal._
import elevate.lift.strategies.algorithmic._
import elevate.lift.rules._
import elevate.lift.rules.algorithmic._
import lift.core.primitives.map


object normalForm {

  def normalize: Strategy => Strategy =
    s => repeat(oncetd(s))

  def LCNF: Strategy = lambdaCalculusNormalForm
  def lambdaCalculusNormalForm: Strategy =
    normalize(betaReduction <+ etaReduction) //`;` tryAll(argumentOf(map)(etaAbstraction))

  def RNF: Strategy = rewriteNormalForm
  def rewriteNormalForm: Strategy = normalize(mapFullFission)

  def CNF: Strategy = codegenNormalForm
  def codegenNormalForm: Strategy = normalize(mapFusion)
}
