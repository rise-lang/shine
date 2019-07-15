package elevate.core.strategies

import elevate.core.Strategy
import elevate.core.rules._
import elevate.core.rules.algorithmic._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.core.strategies.algorithmic._


object normalForm {

  def normalize: Strategy => Strategy =
    s => repeat(oncetd(s))

  def reductionNormalForm: Strategy = normalize(betaReduction <+ etaReduction)

  def rewriteNormalForm: Strategy = normalize(mapFullFission)

  def codegenNormalForm: Strategy = normalize(mapFusion)
}
