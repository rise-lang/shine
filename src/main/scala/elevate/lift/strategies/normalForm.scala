package elevate.lift.strategies

import elevate.core.Strategy
import elevate.core.strategies.basic.repeat
import elevate.core.strategies.traversal.oncetd
import elevate.lift.strategies.algorithmic._
import elevate.lift.rules._
import elevate.lift.rules.algorithmic._


object normalForm {

  def normalize: Strategy => Strategy =
    s => repeat(oncetd(s))

  def reductionNormalForm: Strategy = normalize(betaReduction <+ etaReduction)

  def rewriteNormalForm: Strategy = normalize(mapFullFission)

  def codegenNormalForm: Strategy = normalize(mapFusion)
}
