package elevate.lift.rules

import elevate.core.{Failure, Lift, Strategy, Success}
import lift.core.{Apply, Lambda, primitives}
import lift.core.primitives._

// todo: define rules as case objects/classes
object specialize {

  def mapSeq: Strategy[Lift] = {
    case primitives.map => Success(primitives.mapSeq)
    case _ => Failure(mapSeq)
  }

  // only transforms maps which contain ForeignFunctions or mapSeqs
  def mapSeqCompute: Strategy[Lift] = {
    // (mapSeq λη1. (my_abs η1))
    case Apply(`map`, l@Lambda(_, Apply(ForeignFunction(_,_), _))) => Success(Apply(primitives.mapSeq, l))
    // (map λη1. ((mapSeq λη2. (my_abs η2)) η1))
    case Apply(`map`, l@Lambda(_, Apply(Apply(primitives.mapSeq, _), _))) => Success(Apply(primitives.mapSeq, l))
    case _ => Failure(mapSeqCompute)
  }

  def reduceSeq: Strategy[Lift] = {
    case primitives.reduce => Success(primitives.reduceSeq)
    case _ => Failure(reduceSeq)
  }

  def slideSeq(rot: primitives.slideSeq.Rotate): Strategy[Lift] = {
    case primitives.slide => Success(primitives.slideSeq(rot))
    case _ => Failure(slideSeq(rot))
  }
}