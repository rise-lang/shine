package rise.eqsat

import apps.harrisCornerDetectionHalide.harris
import apps.{harrisCornerDetectionHalideRewrite => elevate}
import ProveEquiv.syntax._

class HarrisHalide extends test_util.Tests {
  val nf = apps.harrisCornerDetectionHalideRewrite.reducedFusedForm

  // TODO
  ignore("buffered shape") {
    val start = nf(harris(1, 1).toExpr).get
    val goal = elevate.ocl.harrisBufferedShape.reduce(_`;`_)(start).get
    ProveEquiv.init().runBENF(start, goal, Seq(
      rules.betaExtract, rules.betaNatExtract, rules.eta,
      /*rules.idxReduction,*/ rules.fstReduction, rules.sndReduction,
      rules.slideOutsideZip, rules.slideInsideZip,
      rules.mapMapFBeforeTranspose,
      rules.mapFusion,
      rules.zipRotateRight,
      rules.mapOutsideZip,
      rules.mapIdentityAfter
    ))
  }

  test("harris ix with iy") {
    val start = elevate.ocl.harrisBufferedShape.reduce(_`;`_)(harris(1, 1)).get
    val goal = elevate.ocl.harrisIxWithIy(start).get
    ProveEquiv.init().runBENF(start, goal, Seq(
      rules.betaExtract, rules.betaNatExtract, rules.eta,
      rules.fstReduction, rules.sndReduction,
      rules.takeOutsidePair,
      rules.vectorize.asScalarOutsidePair,
      rules.mapOutsidePair,
      rules.slideOutsideZip,
      rules.mapOutsideZip,
      rules.zipSame,
      rules.mapFusion
    ))
  }

  // TODO: subexpression elimination and address space support
  ignore("harris buffered lowering") {
    val h = harris(1, 1).toExpr
    val start = (elevate.ocl.harrisBufferedShape.reduce(_`;`_) `;`
      elevate.ocl.harrisIxWithIy)(h).get
    val goal = elevate.ocl.harrisBuffered(h).get
    ProveEquiv.init().runBENF(start, goal, Seq(
      rules.betaExtract, rules.betaNatExtract, rules.eta,
      rules.fstReduction, rules.sndReduction,
      rules.iterateStream,
      // rules.ocl.circularBuffer,
      // rules.ocl.circularBufferLoadFusion,
      rules.mapFusion,
      rules.mapSeq,
      rules.reduceSeq,
      // rules.ocl.reduceSeqUnroll,
    ))
  }
}
