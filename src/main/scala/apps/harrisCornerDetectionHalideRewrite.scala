package apps

import harrisCornerDetectionHalide.{
  sobelXWeights2d, sobelXWeightsV, sobelXWeightsH,
  sobelYWeights2d, sobelYWeightsV, sobelYWeightsH
}
import rise.core.types._
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import rise.elevate.rules.traversal.alternative._
import elevate.core.strategies.predicate._
import rise.elevate._
import rise.elevate.rules._
import rise.elevate.rules.traversal._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.movement._
import cameraPipelineRewrite._
import rise.elevate.strategies.predicate.isEqualToUntyped

object harrisCornerDetectionHalideRewrite {
  private def rewriteSteps(steps: scala.collection.Seq[Strategy[Rise]]): Strategy[Rise] = a => {
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

  val unrollDots: Strategy[Rise] = normalize.apply(lowering.reduceSeqUnroll)

  def someGentleReduction: Strategy[Rise] =
    gentleBetaReduction() <+ etaReduction() <+
    idxReduction <+ fstReduction <+ sndReduction <+
    removeTransposePair
  def reducedFusedForm: Strategy[Rise] = normalize.apply(
    someGentleReduction <+ mapFusion
  )
  def reducedFissionedForm: Strategy[Rise] = normalize.apply(
    someGentleReduction <+ mapLastFission()
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
      slideBeforeMap <+ slideInsideZip <+ mapMapFBeforeTranspose()
    )) `;`
    cameraPipelineRewrite.normalizeInput

  object storeToPrivate {
    import rise.core.DSL._
    import rise.openCL.DSL.toPrivate
    import rise.elevate.rules.lowering.typeHasTrivialCopy

    def apply(find: Strategy[Rise]): Strategy[Rise] =
      subexpressionElimination(find) `;` {
        // TODO: use rewrite rules
        case rise.core.App(f, v) =>
          Success(writeUnrolled(v.t)(v) |> toPrivate |> letf(f))
        case _ => ???
      } `;` reducedFusedForm

    def writeUnrolled(t: Type): ToBeTyped[Rise] = t match {
      case _ if typeHasTrivialCopy(t) => fun(p => p)
      case PairType(a, b) if typeHasTrivialCopy(a) && typeHasTrivialCopy(b) =>
        fun(p => p)
      case ArrayType(_, elem) => fun(p => rise.core.primitives.mapSeqUnroll(writeUnrolled(elem))(p))
      case _ => throw new Exception(s"did not expect $t")
    }
  }

  def isAppliedPair: Strategy[Rise] =
    function(function(isEqualTo(rise.core.primitives.makePair.primitive)))
  def isReduceFI: Strategy[Rise] =
    function(function(isEqualTo(rise.core.primitives.reduce.primitive)))
  def isAppliedReduce: Strategy[Rise] = function(isReduceFI)
  def isAppliedUnzip: Strategy[Rise] =
    function(isEqualTo(rise.core.primitives.unzip.primitive))
  def isPadEmpty: Strategy[Rise] =
    depFunction(isEqualTo(rise.core.primitives.padEmpty.primitive))

  object ocl {
    val unrollDots: Strategy[Rise] = normalize.apply(
      lowering.ocl.reduceSeqUnroll(AddressSpace.Private))

    val lineBuffer: Strategy[Rise] = lowering.ocl.circularBuffer(AddressSpace.Global)

    def harrisBufferedShape: scala.collection.Seq[Strategy[Rise]] = scala.collection.Seq(
      reducedFusedForm,

      afterTopLevel(afterDefs(
        normalizeInput `;` stronglyReducedForm
      )) `;` reducedFusedForm,

      afterTopLevel(afterDefs(argument(
        slideOutsideZip `;`
        argument(argument(normalizeInput `;` stronglyReducedForm))
      ))) `;` reducedFusedForm,

      afterTopLevel( // zip unzip simplification
        topDown(argument(isAppliedUnzip) `;` betaReduction) `;`
        normalize.apply(
          someGentleReduction <+ mapFusion <+
          zipUnzipAccessSimplification <+ mapProjZipUnification()
        )
      ),
    )

    def harrisIxWithIy: Strategy[Rise] =
      afterTopLevel(afterDefs(
        normalize.apply(
          someGentleReduction <+
          takeOutsidePair <+ vectorize.asScalarOutsidePair
        ) `;`
        topDown(
          isAppliedPair `;` mapOutsidePair `;`
          `try` { topDown(slideOutsideZip) `;` topDown(mapOutsideZip) } `;`
          topDown(zipSame)
        ) `;` reducedFusedForm
      ))

    def harrisBufferedLowering(
      lowerReductionLoop: Strategy[Rise] = lowering.mapSeq
    ): Strategy[Rise] = {
      topDown(lowering.iterateStream) `;`
      repeatNTimes(2)(argumentsTd(function(lineBuffer))) `;`
      normalize.apply(lowering.ocl.circularBufferLoadFusion) `;`
      reducedFusedForm `;`
      argument(argument(topDown(lowering.mapSeq))) `;`
      argument(function(argument(
        stronglyReducedForm `;` topDown(lowerReductionLoop)
      ))) `;`
      function(argument(topDown(
        function(lowerReductionLoop) `;`
        argument(stronglyReducedForm `;` lambdaBodyWithName(x => {
          import rise.core.primitives._
          storeToPrivate(
            isEqualToUntyped(fst(x)) <+ isAppliedReduce
          ) `;`
          storeToPrivate(
            isEqualToUntyped(fst(snd(x))) <+ isAppliedReduce
          ) `;`
          storeToPrivate(
            isEqualToUntyped(snd(snd(x))) <+ isAppliedReduce
          )
        }))
      ))) `;`
      unrollDots
    }

    def harrisBuffered: Strategy[Rise] = {
      rewriteSteps(harrisBufferedShape ++ scala.collection.Seq(
        harrisIxWithIy,
        afterTopLevel(harrisBufferedLowering()) `;` reducedFusedForm
      ))
    }

    def harrisSplitParShape(strip: Int): Strategy[Rise] = {
      afterTopLevel(
        topDown(splitJoin(strip)) `;`
        reducedFusedForm `;`
        argumentsTd(slideBeforeSplit) `;`
        argumentsTd(slideBeforeMap) `;`
        argumentsTd(slideBeforeSlide) `;`
        argumentsTd(slideBeforeMap)
      ) `;` reducedFusedForm
    }

    def harrisBufferedSplitPar(strip: Int): Strategy[Rise] = {
      rewriteSteps(scala.collection.Seq(
        harrisBufferedShape.reduce(_`;`_),
        harrisIxWithIy,
        harrisSplitParShape(strip),

        afterTopLevel(
          topDown(lowering.mapGlobal()) `;`
          topDown(harrisBufferedLowering())
        )
      ))
    }

    def vectorizeRoundUpAndNormalize(vwidth: Int): Strategy[Rise] = {
      vectorize.roundUpAfter(vwidth) `;`
      normalize.apply(padEmptyBeforeMap <+ padEmptyBeforeTranspose)
    }

    def normalizeVectorized: Strategy[Rise] = normalize.apply(
      someGentleReduction <+ mapFusion <+
      transposeBeforeMapJoin <+ mapMapFBeforeTranspose() <+
      vectorize.beforeMap
    )

    def vectorizeReductions(vwidth: Int): Strategy[Rise] = {
      afterTopLevel(
        normalize.apply(
          isAppliedMap `;`
          topDown(reduceMapFusion) `;`
          reducedFissionedForm `;` (
            vectorize.alignedAfter(vwidth) <+
            vectorizeRoundUpAndNormalize(vwidth)
          ) `;`
          topDown(vectorize.beforeMapDot) `;`
          normalizeVectorized
        ) `;`
        normalize.apply(
          isAppliedMap `;`
          function(argument(isReduceFI <+ body(isAppliedReduce))) `;`
          reducedFissionedForm `;`
          vectorizeRoundUpAndNormalize(vwidth) `;`
          argument(argument(vectorize.beforeMapReduce)) `;`
          normalizeVectorized
        ) `;`
        normalize.apply(
          takeOutisdeZip <+ takeAfterMap <+
          removeTakeBeforePadEmpty
        ) `;`
        topDown(
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
        topDown(
          function(argument(argument(argument(isPadEmpty)))) `;`
          mapFusion `;` function(topDown(slideBeforeMapMapF))
        ) `;`
        topDown(
          isAppliedZip `;` argument(isAppliedZip) `;`
          subexpressionElimination {
            isAppliedMap `;` function(argument(
              function(isEqualToUntyped(rise.core.primitives.mapSnd.primitive)) `;`
              argument(isPadEmpty)
            ))
          }
        ) `;`
        reducedFissionedForm `;`
        topDown(
          isAppliedMap `;`
          function(argument(argument(argument(
            function(isEqualToUntyped(rise.core.primitives.mapSnd.primitive)) `;`
            argument(isPadEmpty)
          )))) `;`
          reducedFusedForm `;`
          topDown(slideBeforeMapMapF)
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
        topDown(
          isAppliedZip `;` argument(isAppliedZip) `;`
          normalize.apply(
            someGentleReduction <+ mapFusion <+
            transposeBeforeMapJoin <+ slideBeforeMap <+ mapMapFBeforeTranspose()
          ) `;`
          normalize.apply(
            someGentleReduction <+ mapLastFission() <+
            mapMapFBeforeJoin
          ) `;`
          normalize.apply(
            someGentleReduction <+ mapFusion <+
            vectorize.beforeMap <+ slideBeforeMap
          ) `;`
          normalize.apply(
            someGentleReduction <+ mapLastFission() <+
            mapMapFBeforeTranspose()
          ) `;`
          reducedFusedForm `;`
          repeatNTimes(2)(topDown(mapOutsideZip)) `;`
          topDown(zipSame) `;`
          topDown(isAppliedZip `;` anyMapOutsideZip) `;`
          topDown(zipSame) `;`
          reducedFusedForm `;`
          function(argument(stronglyReducedForm))
        )
      ) `;` reducedFusedForm

    def storeSlidingWindowsToPrivate: Strategy[Rise] =
      topDown(
        lambdaBodyWithName(jnbh =>
          isAppliedPair `;`
          storeToPrivate(isEqualToUntyped(jnbh))
        )
      ) `;`
      topDown(
        function(function(isEqualToUntyped(rise.core.primitives.mapSeq.primitive))) `;`
        topDown(lambdaBodyWithName(x =>
          storeToPrivate(isEqualToUntyped(x))
        ))
      )

    def harrisBufferedVecUnalignedSplitPar(vwidth: Int, strip: Int)
    : Strategy[Rise] = {
      rewriteSteps(scala.collection.Seq(
        harrisBufferedShape.reduce(_ `;` _),
        harrisSplitParShape(strip),
        vectorizeReductions(vwidth),
        harrisIxWithIy,
        movePadEmpty,
        unifyZipZipInput,

        afterTopLevel(
          topDown(lowering.mapGlobal()) `;`
          topDown(harrisBufferedLowering()) `;`
          storeSlidingWindowsToPrivate
        )
      ))
    }

    def alignLoads: Strategy[Rise] =
      afterTopLevel(
        normalize.apply(
          isAppliedMap `;`
          argument(function(isEqualToUntyped(rise.core.primitives.transpose.primitive))) `;`
          reducedFissionedForm `;` topDown(vectorize.alignSlide) `;`
          reducedFusedForm `;`
          normalize.apply(
            someGentleReduction <+ mapFusion <+
            vectorize.padEmptyBeforeAsScalar <+ vectorize.asScalarAsVectorId <+
            padEmptyBeforeMap <+ padEmptyBeforeTranspose <+
            removeTakeBeforePadEmpty
          ) `;`
          normalize.apply(
            someGentleReduction <+ mapLastFission() <+
            mapMapFBeforeTranspose()
          ) `;` reducedFusedForm
        )
      )

    def harrisBufferedVecAlignedSplitPar(vwidth: Int, strip: Int)
    : Strategy[Rise] = {
      rewriteSteps(scala.collection.Seq(
        harrisBufferedShape.reduce(_ `;` _),
        harrisSplitParShape(strip),
        vectorizeReductions(vwidth),
        harrisIxWithIy,
        alignLoads,
        movePadEmpty,
        unifyZipZipInput,

        afterTopLevel(
          topDown(lowering.mapGlobal()) `;`
          topDown(harrisBufferedLowering()) `;`
          normalize.apply(vectorize.mapAfterShuffle) `;`
          storeSlidingWindowsToPrivate
        )
      ))
    }

    def separateReductionThroughMap(separate: Strategy[Rise]): Strategy[Rise] =
      isAppliedMap `;`
      topDown(separate) `;`
      reducedFissionedForm `;`
      topDown(mapSlideBeforeTranspose) `;`
      reducedFusedForm `;` reducedFissionedForm `;`
      normalize.apply(slideBeforeMapMapF) `;`
      reducedFusedForm

    def separateReductions: Strategy[Rise] =
      bottomUp(separateReductionThroughMap(
        separateDotVH(sobelXWeights2d, sobelXWeightsV, sobelXWeightsH)
      )) `;`
      bottomUp(separateReductionThroughMap(
        separateDotVH(sobelYWeights2d, sobelYWeightsV, sobelYWeightsH)
      )) `;`
      repeatNTimes(3)(bottomUp(separateReductionThroughMap(
        separateSumVH
      )))

    def harrisBufferedRegRotVecAlignedSplitPar(vwidth: Int, strip: Int)
    : Strategy[Rise] = {
      rewriteSteps(scala.collection.Seq(
        harrisBufferedShape.reduce(_ `;` _),
        harrisSplitParShape(strip),
        separateReductions,
        vectorizeReductions(vwidth),
        alignLoads,
        harrisIxWithIy,
        movePadEmpty,

        afterTopLevel(
          topDown(
            isAppliedZip `;` argument(isAppliedZip) `;`
            repeatNTimes(2)(topDown(mapOutsideZip)) `;`
            topDown(slideOutsideZip) `;`
            topDown(isAppliedZip `;` anyMapOutsideZip) `;`
            topDown(slideOutsideZip)
          ) `;` reducedFusedForm `;`
          repeatNTimes(2)(topDown({
            import rise.core.primitives._
            import rise.core.DSL._
            var t: Type = null
            function(isEqualToUntyped(slide(2)(1))) `;`
            argument { e: Rise => t = e.t; Success(e) } `;`
            { p: Rise => topDown(lowering.ocl.rotateValues(AddressSpace.Private,
              t match {
                case ArrayType(_, ArrayType(_, _)) =>
                  mapSeqUnroll(fun(x => x))
                case _ => fun(x => x)
              })).apply(p) }
          })) `;`
          topDown(lowering.mapGlobal()) `;`
          topDown(harrisBufferedLowering(lowering.iterateStream))
        )
      ))
    }
  }
}
