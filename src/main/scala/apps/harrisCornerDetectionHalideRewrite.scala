package apps

import harrisCornerDetectionHalide.{
  sobelXWeights2d, sobelXWeightsV, sobelXWeightsH,
  sobelYWeights2d, sobelYWeightsV, sobelYWeightsH
}
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
import cameraPipeRewrite.{afterTopLevel, anyMapOutsideZip, depFunction, isAppliedMap, isAppliedZip, stronglyReducedForm}

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

  def someGentleReduction: Strategy[Rise] =
    gentleBetaReduction <+ etaReduction <+
    idxReduction <+ fstReduction <+ sndReduction <+
    removeTransposePair
  def reducedFusedForm: Strategy[Rise] = normalize.apply(
    someGentleReduction <+ mapFusion
  )
  def reducedFissionedForm: Strategy[Rise] = normalize.apply(
    someGentleReduction <+ mapLastFission
  )

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
      someGentleReduction <+ mapFusion <+
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
      } `;` reducedFusedForm

    def writeUnrolled(t: Type): TDSL[Rise] = t match {
      case _: BasicType => fun(p => p)
      case PairType(_: BasicType, _: BasicType) => fun(p => p)
      case ArrayType(_, elem) => fun(p => mapSeqUnroll(writeUnrolled(elem), p))
      case _ => throw new Exception(s"did not expect $t")
    }
  }

  def isAppliedPair: Strategy[Rise] =
    function(function(isEqualTo(rise.core.DSL.pair)))
  def isReduceFI: Strategy[Rise] =
    function(function(isEqualTo(rise.core.DSL.reduce)))
  def isAppliedReduce: Strategy[Rise] = function(isReduceFI)
  def isAppliedUnzip: Strategy[Rise] =
    function(isEqualTo(rise.core.DSL.unzip))
  def isPadEmpty: Strategy[Rise] =
    depFunction(isEqualTo(rise.core.DSL.padEmpty))

  object ocl {
    val unrollDots = normalize.apply(
      lowering.ocl.reduceSeqUnroll(AddressSpace.Private))

    val lineBuffer = lowering.ocl.circularBuffer(AddressSpace.Global)

    def harrisBufferedShape: Seq[Strategy[Rise]] = Seq(
      reducedFusedForm,

      afterTopLevel(afterDefs(
        normalizeInput `;` stronglyReducedForm
      )) `;` reducedFusedForm,

      afterTopLevel(afterDefs(argument(
        slideOutsideZip `;`
        argument(argument(normalizeInput `;` stronglyReducedForm))
      ))) `;` reducedFusedForm,

      afterTopLevel( // zip unzip simplification
        oncetd(argument(isAppliedUnzip) `;` betaReduction) `;`
        normalize.apply(
          someGentleReduction <+ mapFusion <+
          zipUnzipAccessSimplification <+ mapProjZipUnification
        )
      ),
    )

    def harrisIxWithIy: Strategy[Rise] =
      afterTopLevel(afterDefs(
        normalize.apply(
          someGentleReduction <+
          takeOutsidePair <+ vectorize.asScalarOutsidePair
        ) `;`
        oncetd(
          isAppliedPair `;` mapOutsidePair `;`
          `try` { oncetd(slideOutsideZip) `;` oncetd(mapOutsideZip) } `;`
          oncetd(zipSame)
        ) `;` reducedFusedForm
      ))

    def harrisBufferedLowering(
      lowerReductionLoop: Strategy[Rise] = lowering.mapSeq
    ): Strategy[Rise] = {
      oncetd(lowering.iterateStream) `;`
      repeatNTimes(2, argumentsTd(function(lineBuffer))) `;`
      normalize.apply(lowering.ocl.circularBufferLoadFusion) `;`
      reducedFusedForm `;`
      argument(argument(oncetd(lowering.mapSeq))) `;`
      argument(function(argument(
        stronglyReducedForm `;` oncetd(lowerReductionLoop)
      ))) `;`
      function(argument(oncetd(
        function(lowerReductionLoop) `;`
        argument(stronglyReducedForm `;` lambdaBodyWithName(x => {
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
        afterTopLevel(harrisBufferedLowering()) `;` reducedFusedForm
      ))
    }

    def harrisSplitParShape(strip: Int): Strategy[Rise] = {
      afterTopLevel(
        oncetd(splitJoin(strip)) `;`
        reducedFusedForm `;`
        argumentsTd(slideBeforeSplit) `;`
        argumentsTd(slideBeforeMap) `;`
        argumentsTd(slideBeforeSlide) `;`
        argumentsTd(slideBeforeMap)
      ) `;` reducedFusedForm
    }

    def harrisBufferedSplitPar(strip: Int): Strategy[Rise] = {
      rewriteSteps(Seq(
        harrisBufferedShape.reduce(_`;`_),
        harrisIxWithIy,
        harrisSplitParShape(strip),

        afterTopLevel(
          oncetd(lowering.mapGlobal()) `;`
          oncetd(harrisBufferedLowering())
        )
      ))
    }

    def vectorizeRoundUpAndNormalize(vwidth: Int): Strategy[Rise] = {
      vectorize.roundUpAfter(vwidth) `;`
      normalize.apply(padEmptyBeforeMap <+ padEmptyBeforeTranspose)
    }

    def normalizeVectorized: Strategy[Rise] = normalize.apply(
      someGentleReduction <+ mapFusion <+
      transposeBeforeMapJoin <+ mapMapFBeforeTranspose <+
      vectorize.beforeMap
    )

    def vectorizeReductions(vwidth: Int): Strategy[Rise] = {
      afterTopLevel(
        normalize.apply(
          isAppliedMap `;`
          oncetd(reduceMapFusion) `;`
          reducedFissionedForm `;` (
            vectorize.alignedAfter(vwidth) <+
            vectorizeRoundUpAndNormalize(vwidth)
          ) `;`
          oncetd(vectorize.beforeMapDot) `;`
          normalizeVectorized
        ) `;`
        normalize.apply(
          isAppliedMap `;`
          function(argument(isReduceFI <+ body(isAppliedReduce))) `;`
          reducedFissionedForm `;` (
            vectorize.alignedAfter(vwidth) <+
            vectorizeRoundUpAndNormalize(vwidth)
          ) `;`
          argument(argument(vectorize.beforeMapReduce)) `;`
          normalizeVectorized
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
            someGentleReduction <+ mapFusion <+
            unzipZipIsPair <+ vectorize.asScalarAsVectorId
          )
        )
      )
    }

    def movePadEmpty: Strategy[Rise] =
      afterTopLevel(
        normalize.apply(
          someGentleReduction <+ mapFusion <+
          padEmptyBeforeTranspose <+ padEmptyBeforeMap <+
          padEmptyBeforeSlide <+ padEmptyBeforeZip <+
          unzipZipIdentity <+ vectorize.padEmptyBeforeZipAsVector <+
          vectorize.padEmptyBeforeAsVector <+
          mapFstBeforeMapSnd <+ mapFstFusion <+ mapSndFusion <+
          removeTakeBeforePadEmpty <+ padEmptyFusion
        ) `;`
        reducedFissionedForm `;`
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
        reducedFissionedForm `;`
        oncetd(
          isAppliedMap `;`
          function(argument(argument(argument(
            function(isEqualTo(rise.core.DSL.mapSnd)) `;`
            argument(isPadEmpty)
          )))) `;`
          reducedFusedForm `;`
          oncetd(slideBeforeMapMapF)
        ) `;`
        reducedFusedForm `;`
        normalize.apply(
          someGentleReduction <+ mapFstBeforeMapSnd <+
          mapFstFusion <+ mapSndFusion <+ removeTakeBeforePadEmpty <+
          padEmptyFusion
        )
      )

    def unifyZipZipInput: Strategy[Rise] =
      afterTopLevel(
        oncetd(
          isAppliedZip `;` argument(isAppliedZip) `;`
          normalize.apply(
            someGentleReduction <+ mapFusion <+
            transposeBeforeMapJoin <+ slideBeforeMap <+ mapMapFBeforeTranspose
          ) `;`
          normalize.apply(
            someGentleReduction <+ mapLastFission <+
            mapMapFBeforeJoin
          ) `;`
          normalize.apply(
            someGentleReduction <+ mapFusion <+
            vectorize.beforeMap <+ slideBeforeMap
          ) `;`
          normalize.apply(
            someGentleReduction <+ mapLastFission <+
            mapMapFBeforeTranspose
          ) `;`
          reducedFusedForm `;`
          repeatNTimes(2, oncetd(mapOutsideZip)) `;`
          oncetd(zipSame) `;`
          oncetd(isAppliedZip `;` anyMapOutsideZip) `;`
          oncetd(zipSame) `;`
          reducedFusedForm `;`
          function(argument(stronglyReducedForm))
        )
      ) `;` reducedFusedForm

    def storeSlidingWindowsToPrivate: Strategy[Rise] =
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

    def harrisBufferedVecUnalignedSplitPar(vwidth: Int, strip: Int)
    : Strategy[Rise] = {
      rewriteSteps(Seq(
        harrisBufferedShape.reduce(_ `;` _),
        harrisSplitParShape(strip),
        vectorizeReductions(vwidth),
        harrisIxWithIy,
        movePadEmpty,
        unifyZipZipInput,

        afterTopLevel(
          oncetd(lowering.mapGlobal()) `;`
          oncetd(harrisBufferedLowering()) `;`
          storeSlidingWindowsToPrivate
        )
      ))
    }

    def alignLoads: Strategy[Rise] =
      afterTopLevel(
        normalize.apply(
          isAppliedMap `;`
          argument(function(isEqualTo(rise.core.DSL.transpose))) `;`
          reducedFissionedForm `;` oncetd(vectorize.alignSlide) `;`
          reducedFusedForm `;`
          normalize.apply(
            someGentleReduction <+ mapFusion <+
            vectorize.padEmptyBeforeAsScalar <+ vectorize.asScalarAsVectorId <+
            padEmptyBeforeMap <+ padEmptyBeforeTranspose <+
            removeTakeBeforePadEmpty
          ) `;`
          normalize.apply(
            someGentleReduction <+ mapLastFission <+
            mapMapFBeforeTranspose
          ) `;` reducedFusedForm
        )
      )

    def harrisBufferedVecAlignedSplitPar(vwidth: Int, strip: Int)
    : Strategy[Rise] = {
      rewriteSteps(Seq(
        harrisBufferedShape.reduce(_ `;` _),
        harrisSplitParShape(strip),
        vectorizeReductions(vwidth),
        harrisIxWithIy,
        alignLoads,
        movePadEmpty,
        unifyZipZipInput,

        afterTopLevel(
          oncetd(lowering.mapGlobal()) `;`
          oncetd(harrisBufferedLowering()) `;`
          normalize.apply(vectorize.mapAfterShuffle) `;`
          storeSlidingWindowsToPrivate
        )
      ))
    }

    def separateReductionThroughMap(separate: Strategy[Rise]): Strategy[Rise] =
      isAppliedMap `;`
      oncetd(separate) `;`
      reducedFissionedForm `;`
      oncetd(mapSlideBeforeTranspose) `;`
      reducedFusedForm `;` reducedFissionedForm `;`
      normalize.apply(slideBeforeMapMapF) `;`
      reducedFusedForm

    def separateReductions: Strategy[Rise] =
      oncebu(separateReductionThroughMap(
        separateDotVH(sobelXWeights2d, sobelXWeightsV, sobelXWeightsH)
      )) `;`
      oncebu(separateReductionThroughMap(
        separateDotVH(sobelYWeights2d, sobelYWeightsV, sobelYWeightsH)
      )) `;`
      repeatNTimes(3, oncebu(separateReductionThroughMap(
        separateSumVH
      )))

    def harrisBufferedRegRotVecAlignedSplitPar(vwidth: Int, strip: Int)
    : Strategy[Rise] = {
      rewriteSteps(Seq(
        harrisBufferedShape.reduce(_ `;` _),
        harrisSplitParShape(strip),
        separateReductions,
        vectorizeReductions(vwidth),
        alignLoads,
        harrisIxWithIy,
        movePadEmpty,

        afterTopLevel(
          oncetd(
            isAppliedZip `;` argument(isAppliedZip) `;`
            repeatNTimes(2, oncetd(mapOutsideZip)) `;`
            oncetd(slideOutsideZip) `;`
            oncetd(isAppliedZip `;` anyMapOutsideZip) `;`
            oncetd(slideOutsideZip)
          ) `;` reducedFusedForm `;`
          repeatNTimes(2, oncetd({
            import rise.core.DSL._
            var t: Type = null
            function(isEqualTo(slide(2)(1))) `;`
            argument { e: Rise => t = e.t; Success(e) } `;`
            { p: Rise => oncetd(lowering.ocl.rotateValues(AddressSpace.Private,
              t match {
                case ArrayType(_, ArrayType(_, _)) =>
                  mapSeqUnroll(fun(x => x))
                case _ => fun(x => x)
              })).apply(p) }
          })) `;`
          oncetd(lowering.mapGlobal()) `;`
          oncetd(harrisBufferedLowering(lowering.iterateStream))
        )
      ))
    }
  }
}
