package apps

import rise.core.TypedDSL._
import rise.OpenMP.TypedDSL._
import rise.core.primitives.SlideSeq.{Indices => RotateIndices}
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.rise._
import elevate.rise.rules._
import elevate.rise.rules.traversal._
import elevate.rise.rules.algorithmic._
import elevate.rise.rules.movement._

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

  val unrollDots = normalize.apply(lowering.reduceSeqUnroll)

  def splitPar: Strategy[Rise] = {
    rewriteSteps(Seq(
      cameraPipeRewrite.gentlyReducedForm,
      cameraPipeRewrite.afterTopLevel(
        { expr: Rise =>
          Success[Rise](
            (split(32) >> mapPar(mapSeq(mapSeq(fun(x => x)))) >> join)(expr))
        } `;`
        unrollDots `;`
        cameraPipeRewrite.gentlyReducedForm
      )
    ))
  }

  def afterDefs(s: Strategy[Rise]): Strategy[Rise] = p => {
    (function(body(afterDefs(s))) <+ s)(p)
  }

  def argumentsTd(s: Strategy[Rise]): Strategy[Rise] = p =>
    (s <+
      argument(argumentsTd(s)) <+
      (cameraPipeRewrite.isAppliedZip `;` function(argument(argumentsTd(s))))
      )(p)

  def normalizeInput: Strategy[Rise] =
    repeat(argumentsTd(
      gentleBetaReduction <+ etaReduction <+ mapFusion <+
      mapFBeforeSlide <+ slideInsideZip <+ transposeBeforeMapMapF
    )) `;`
    cameraPipeRewrite.normalizeInput

  val lineBuffer = lowering.slideSeq(RotateIndices, mapSeq(fun(x => x)))

  def circularBuffers: Strategy[Rise] = {
    rewriteSteps(Seq(
      cameraPipeRewrite.gentlyReducedForm,

      cameraPipeRewrite.afterTopLevel(afterDefs(
        normalizeInput `;` cameraPipeRewrite.stronglyReducedForm
      )) `;` cameraPipeRewrite.gentlyReducedForm,

      cameraPipeRewrite.afterTopLevel(afterDefs(
        argument(
          cameraPipeRewrite.unifyMapInputs(Seq(
            s => function(argument(argument(s))),
            s => argument(argument(s))
          )) `;`
          function(argument(argument(normalizeInput))) `;`
          argument(argument(normalizeInput))
        ) `;`
        subexpressionElimination({
          import rise.core.DSL._
          slide(3)(1)(identifier("x1"))
        })
      )) `;` cameraPipeRewrite.gentlyReducedForm,

      cameraPipeRewrite.afterTopLevel(
        argument(oncetd(lineBuffer)) `;`
        function(body(
          oncetd(lowering.mapStream) `;`
          argument(argument(oncetd(lineBuffer))) `;`
          argument(function(argument(oncetd(lineBuffer)))) `;`
          cameraPipeRewrite.gentlyReducedForm `;`
          function(argument(body(
            oncetd(lowering.iterateStream)
          )))
        )) `;` unrollDots
      ) `;` cameraPipeRewrite.gentlyReducedForm
    ))
  }
}
