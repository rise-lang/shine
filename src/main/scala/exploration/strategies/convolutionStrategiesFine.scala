package exploration.strategies

import apps.separableConvolution2D.{binomialWeights2d, binomialWeightsH, binomialWeightsV}
import elevate.core.{LeftChoice, _}
import elevate.core.strategies.basic.repeatNTimes
import elevate.core.strategies.traversal._
import rise.core.DSL._
import rise.core._
import rise.elevate.Rise
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering
import rise.elevate.rules.movement._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.alternative._
import rise.elevate.strategies.algorithmic._

object convolutionStrategiesFine {

  private val idE: Expr = fun(x => x)
  private val idS: Strategy[Rise] = elevate.core.strategies.basic.id

  private val BENF = rise.elevate.strategies.normalForm.BENF()(alternative.RiseTraversable)

  private val weights2d = binomialWeights2d
  private val weightsV = binomialWeightsV
  private val weightsH = binomialWeightsH

  private val separateDot: Strategy[Rise] =
    separateDotHV(weights2d, weightsH, weightsV)

  private val separateDotT: Strategy[Rise] =
    separateDotVH(weights2d, weightsV, weightsH)




  // todo add generic lowering
  // maybe full lowering?
  //
  //  // lowerings
  //  val baseSeq = (topDown(lowering.reduceSeqUnroll) `;` repeatNTimes(2)(topDown(lowering.mapSeq)))
  //  val factorisedSeq = (repeatNTimes(2)(topDown(lowering.reduceSeqUnroll)) `;` repeatNTimes(2)(topDown(lowering.mapSeq)))
  //  val separatedSeq = (repeatNTimes(2)(topDown(lowering.reduceSeqUnroll)) `;` repeatNTimes(2)(topDown(lowering.mapSeq)) `;` repeatNTimes(2)(skip(1)(lowering.mapSeq)) `;` body(argument(lowering.toMemAfterMapSeq)))
  //  val scanlineSeq = (repeatNTimes(2)(topDown(lowering.reduceSeqUnroll)) `;` repeatNTimes(2)(topDown(lowering.mapSeq)) `;` skip(1)(lowering.mapSeq))
  //  val regRotSeq = (repeatNTimes(2)(topDown(lowering.reduceSeqUnroll)) `;` topDown(lowering.mapSeq) `;` topDown(lowering.rotateValues(idE)) `;` topDown(lowering.iterateStream))
  //
  //    val loweringStrategy:Strategy[Rise] = baseSeq `<+` factorisedSeq `<+` separatedSeq `<+` scanlineSeq `<+` regRotSeq
  //
  //  // strategies
  //  val baseToFactorise = topDown(separateDot)
  //

  // apply BENF after each rewrite
  // normal form for lowering?

  // base to scanline strategies
  val strategies: Seq[Strategy[Rise]] = Seq(
    idS,
    topDown(separateDotT),
    topDown(`*f >> S -> S >> **f`),
    topDown(mapFusion),
    topDown(`*S >> T -> T >> S >> *T`),
    topDown(removeTransposePair),
    topDown(skip(1)(mapFirstFission)),
    topDown(`S >> **f -> *f >> S`)
  )

}
