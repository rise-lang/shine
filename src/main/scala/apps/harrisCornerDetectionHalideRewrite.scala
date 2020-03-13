package apps

import elevate.core._
import elevate.rise._

object harrisCornerDetectionHalideRewrite {
  private def rewriteSteps(steps: Seq[Strategy[Rise]]): Strategy[Rise] = a => {
    var nRewrite = 0
    steps.foldLeft[RewriteResult[Rise]](Success(a))({ case (r, s) =>
      r.flatMapSuccess { e =>
        nRewrite += 1
        val result = util.printTime(s"rewrite $nRewrite", s(e))
        util.dotPrintTmp(s"rewrite$nRewrite", result)
        result
      }
    })
  }

  def circularBuffers: Strategy[Rise] = {
    rewriteSteps(Seq(
      cameraPipeRewrite.gentlyReducedForm
    ))
  }
}
