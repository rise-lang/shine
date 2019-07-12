package elevate.core.strategies

import elevate.core.Strategy
import elevate.core.rules._
import elevate.core.rules.algorithmic._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.core.strategies.algorithmic._

object normalforms {

  def normalize: Strategy => Strategy =
    s => repeat(oncetd(s))

  def reductionNormalform: Strategy = normalize(betaReduction <+ etaReduction)

  def rewriteNormalform: Strategy = normalize(mapFullFission)

  def codegenNormalform: Strategy = normalize(mapFusion)
}
