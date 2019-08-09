package elevate.lift.rules

import elevate.core._
import lift.core._
import lift.core.primitives._

object specialize {
  def mapSeq: Strategy = {
    case primitives.map => Success(primitives.mapSeq)
    case _ => Failure(mapSeq)
  }

  // only transforms maps which contain ForeignFunctions or mapSeqs
  def mapSeqCompute: Strategy = {
    // (mapSeq λη1. (my_abs η1))
    case Apply(`map`, l@Lambda(_, Apply(ForeignFunction(_,_), _))) => Success(Apply(primitives.mapSeq, l))
    // (map λη1. ((mapSeq λη2. (my_abs η2)) η1))
    case Apply(`map`, l@Lambda(_, Apply(Apply(primitives.mapSeq, _), _))) => Success(Apply(primitives.mapSeq, l))
    case _ => Failure(mapSeqCompute)
  }

  def reduceSeq: Strategy = {
    case primitives.reduce => Success(primitives.reduceSeq)
    case _ => Failure(reduceSeq)
  }

  def slideSeq(rot: primitives.slideSeq.Rotate): Strategy = {
    case primitives.slide => Success(primitives.slideSeq(rot))
    case _ => Failure(slideSeq(rot))
  }
}