package apps

import rise.core.types._
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.core.strategies.predicate._
import elevate.rise._
import elevate.rise.rules._
import elevate.rise.rules.traversal._
import elevate.rise.rules.algorithmic._
import elevate.rise.rules.movement._
import cameraPipeRewrite.{afterTopLevel, depFunction, gentlyReducedForm, isAppliedMap, isAppliedZip, stronglyReducedForm}

object harrisCornerDetectionHalideRewrite {
  private def rewriteSteps(steps: Seq[Strategy[Rise]]): Strategy[Rise] = a => {
    var nRewrite = 0
    steps.foldLeft[RewriteResult[Rise]](Success(a))({ case (r, s) =>
      r.flatMapSuccess { e =>
        nRewrite += 1
        val result = util.printTime(s"rewrite $nRewrite", s(e))
        // util.dotPrintTmp(s"rewrite$nRewrite", result)
        result
      }
    })
  }

  val unrollDots = normalize.apply(lowering.reduceSeqUnroll)

  def afterDefs(s: Strategy[Rise]): Strategy[Rise] = p => {
    (function(body(afterDefs(s))) <+ s)(p)
  }

  def argumentsTd(s: Strategy[Rise]): Strategy[Rise] = p =>
    (s <+
      argument(argumentsTd(s)) <+
      (isAppliedZip `;` function(argument(argumentsTd(s))))
      )(p)

  def normalizeInput: Strategy[Rise] =
    repeat(argumentsTd(
      gentleBetaReduction <+ etaReduction <+ mapFusion <+
      slideBeforeMap <+ slideInsideZip <+ mapMapFBeforeTranspose
    )) `;`
    cameraPipeRewrite.normalizeInput

  object storeToPrivate {
    import rise.core.TypedDSL._
    import rise.OpenCL.TypedDSL.toPrivate

    def apply(find: Strategy[Rise]): Strategy[Rise] =
      subexpressionElimination(find) `;` {
        // TODO: use rewrite rules
        case rise.core.App(f, v) =>
          Success(writeUnrolled(v.t)(v) |> toPrivate |> let(f))
        case _ => ???
      } `;` gentlyReducedForm

    def writeUnrolled(t: Type): TDSL[Rise] = t match {
      case _: BasicType => fun(p => p)
      case PairType(_: BasicType, _: BasicType) => fun(p => p)
      case ArrayType(_, elem) => fun(p => mapSeqUnroll(writeUnrolled(elem), p))
      case _ => ???
    }
  }

  def isAppliedPair: Strategy[Rise] =
    function(function(isEqualTo(rise.core.DSL.pair)))
  def isAppliedReduce: Strategy[Rise] =
    function(function(function(isEqualTo(rise.core.DSL.reduce))))
  def isAppliedUnzip: Strategy[Rise] =
    function(isEqualTo(rise.core.DSL.unzip))
  def isPadEmpty: Strategy[Rise] =
    depFunction(isEqualTo(rise.core.DSL.padEmpty))

  object ocl {
    val unrollDots = normalize.apply(
      lowering.ocl.reduceSeqUnroll(AddressSpace.Private))

    val lineBuffer = lowering.ocl.circularBuffer(AddressSpace.Global)

    def harrisBufferedShape: Seq[Strategy[Rise]] = Seq(
      gentlyReducedForm,

      afterTopLevel(afterDefs(
        normalizeInput `;` stronglyReducedForm
      )) `;` gentlyReducedForm,

      afterTopLevel(afterDefs(argument(
        slideOutsideZip `;`
        argument(argument(normalizeInput `;` stronglyReducedForm))
      ))) `;` gentlyReducedForm
    )

    def harrisIxWithIy: Strategy[Rise] =
      afterTopLevel(afterDefs(oncetd(
        isAppliedPair `;` mapOutsidePair `;` oncetd(zipSame)
      )))

    def harrisBufferedLowering: Strategy[Rise] = {
      oncetd(lowering.iterateStream) `;`
      repeatNTimes(2, argumentsTd(function(lineBuffer))) `;`
      normalize.apply(lowering.ocl.circularBufferLoadFusion) `;`
      gentlyReducedForm `;`
      argument(argument(oncetd(lowering.mapSeq))) `;`
      argument(function(argument(
        stronglyReducedForm `;` oncetd(lowering.mapSeq)
      ))) `;`
      function(argument(oncetd(
        function(lowering.mapSeq) `;`
        argument(lambdaBodyWithName(x => {
          import rise.core.DSL._
          storeToPrivate(
            isEqualTo(fst(x)) <+ isAppliedReduce
          ) `;`
          storeToPrivate(
            isEqualTo(fst(snd(x))) <+ isAppliedReduce
          ) `;`
          storeToPrivate(
            isEqualTo(snd(snd(x))) <+ isAppliedReduce
          )
        }))
      ))) `;`
      unrollDots
    }

    def harrisBuffered: Strategy[Rise] = {
      rewriteSteps(harrisBufferedShape ++ Seq(
        harrisIxWithIy,
        afterTopLevel(harrisBufferedLowering) `;` gentlyReducedForm
      ))
    }

    def harrisSplitParShape(strip: Int): Strategy[Rise] = {
      afterTopLevel(
        oncetd(splitJoin(strip)) `;`
          gentlyReducedForm `;`
          argumentsTd(slideBeforeSplit) `;`
          argumentsTd(slideBeforeMap) `;`
          argumentsTd(slideBeforeSlide) `;`
          argumentsTd(slideBeforeMap)
      ) `;` gentlyReducedForm
    }

    def harrisBufferedSplitPar(strip: Int): Strategy[Rise] = {
      rewriteSteps(Seq(
        harrisBufferedShape.reduce(_`;`_),
        harrisIxWithIy,
        harrisSplitParShape(strip),

        afterTopLevel(
          oncetd(lowering.mapGlobal()) `;`
          oncetd(harrisBufferedLowering)
        )
      ))
    }

    def harrisBufferedVecUnalignedSplitPar(vwidth: Int, strip: Int)
    : Strategy[Rise] = {
      rewriteSteps(Seq(
        harrisBufferedShape.reduce(_ `;` _),

        harrisSplitParShape(strip),

        afterTopLevel( // zip unzip simplification
          oncetd(argument(isAppliedUnzip) `;` betaReduction) `;`
          normalize.apply(
            gentleBetaReduction <+ etaReduction <+ mapFusion <+
            fstReduction <+ sndReduction <+
            zipUnzipAccessSimplification <+ mapProjZipUnification
          )
        ),

        afterTopLevel( // vectorize reductions
          normalize.apply(
            isAppliedMap `;`
            oncetd(reduceMapFusion) `;`
            normalize.apply(
              gentleBetaReduction <+ etaReduction <+ mapLastFission
            ) `;` (
              vectorize.alignedAfter(vwidth) <+ (
              vectorize.roundUpAfter(vwidth) `;`
              normalize.apply(padEmptyBeforeMap)
            )) `;`
            oncetd(vectorize.beforeMapDot)
          ) `;`
          normalize.apply(
            isAppliedMap `;`
            function(argument(body(isAppliedReduce))) `;`
            normalize.apply(
              gentleBetaReduction <+ etaReduction <+ mapLastFission
            ) `;`
            vectorize.roundUpAfter(vwidth) `;`
            normalize.apply(padEmptyBeforeMap) `;`
            argument(argument(vectorize.beforeMapReduce))
          ) `;`
          normalize.apply(
            takeOutisdeZip <+ takeAfterMap <+
            removeTakeBeforePadEmpty
          ) `;`
          oncetd(
            isAppliedMap `;`
            argument(isAppliedZip) `;`
            argument(argument(isAppliedZip)) `;`
            vectorize.after(vwidth) `;`
            argument(vectorize.beforeMap) `;`
            normalize.apply(
              gentleBetaReduction <+ etaReduction <+
              removeTransposePair <+ mapFusion <+
              idxReduction <+ fstReduction <+ sndReduction <+
              unzipZipIsPair <+ vectorize.asScalarAsVectorId
            )
          )
        ),

        afterTopLevel( // move padEmpty
          normalize.apply(
            gentleBetaReduction <+ etaReduction <+
            takeOutsidePair <+ vectorize.asScalarOutsidePair
          ) `;`
          harrisIxWithIy `;`
          normalize.apply(
            gentleBetaReduction <+ etaReduction <+
            removeTransposePair <+ mapFusion <+
            idxReduction <+ fstReduction <+ sndReduction <+
            padEmptyBeforeTranspose <+ padEmptyBeforeMap <+
            padEmptyBeforeSlide <+ padEmptyBeforeZip
          ) `;`
          normalize.apply(
            gentleBetaReduction <+ etaReduction <+ mapLastFission
          ) `;`
          oncetd(
            function(argument(argument(argument(isPadEmpty)))) `;`
            mapFusion `;` function(oncetd(slideBeforeMapMapF))
          ) `;`
          oncetd(
            isAppliedZip `;` argument(isAppliedZip) `;`
            subexpressionElimination {
              isAppliedMap `;` function(argument(
                function(isEqualTo(rise.core.DSL.mapSnd)) `;`
                argument(isPadEmpty)
              ))
            }
          ) `;`
          // gentlyReducedForm `;`
          normalize.apply(
            gentleBetaReduction <+ etaReduction <+
            mapLastFission
          ) `;`
          oncetd(
            isAppliedMap `;`
            function(argument(argument(argument(
              function(isEqualTo(rise.core.DSL.mapSnd)) `;`
              argument(isPadEmpty)
            )))) `;`
            gentlyReducedForm `;`
            oncetd(slideBeforeMapMapF)
          ) `;`
          gentlyReducedForm `;`
          normalize.apply(
            gentleBetaReduction <+ etaReduction <+
            mapFstBeforeMapSnd <+ mapFstFusion <+ mapSndFusion <+
            removeTakeBeforePadEmpty
          )
        ),

        afterTopLevel(
          oncetd(
            isAppliedZip `;` argument(isAppliedZip) `;`
            normalize.apply(
              gentleBetaReduction <+ etaReduction <+ mapFusion <+
                removeTransposePair <+ transposeBeforeMapJoin <+
                slideBeforeMap <+ mapMapFBeforeTranspose
            ) `;`
            normalize.apply(
              gentleBetaReduction <+ etaReduction <+ mapLastFission <+
                mapMapFBeforeJoin
            ) `;`
            normalize.apply(
              gentleBetaReduction <+ etaReduction <+ mapFusion <+
                vectorize.beforeMap
            ) `;`
            normalize.apply(
              gentleBetaReduction <+ etaReduction <+ mapLastFission <+
                mapMapFBeforeTranspose
            ) `;`
            gentlyReducedForm `;`
            repeatNTimes(2, oncetd(mapOutsideZip)) `;`
            oncetd(zipSame) `;`
            oncetd(isAppliedZip `;` cameraPipeRewrite.anyMapOutsideZip) `;`
            oncetd(zipSame) `;`
            gentlyReducedForm `;`
            function(argument(stronglyReducedForm))
          )
        ) `;` gentlyReducedForm,

        afterTopLevel(
          oncetd(lowering.mapGlobal()) `;`
          oncetd(harrisBufferedLowering) `;`
          oncetd(
            lambdaBodyWithName(jnbh =>
              isAppliedPair `;`
              storeToPrivate(isEqualTo(jnbh))
            )
          ) `;`
          oncetd(
            function(function(isEqualTo(rise.core.DSL.mapSeq))) `;`
            oncetd(lambdaBodyWithName(x =>
              storeToPrivate(isEqualTo(x))
            ))
          )
        )
      ))
    }
  }
}
