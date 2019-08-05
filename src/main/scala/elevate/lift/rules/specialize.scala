package elevate.lift.rules

import elevate.core.{Failure, Lift, RewriteResult, Strategy, Success}
import lift.core.{Apply, Lambda, primitives}
import lift.core.primitives._

object specialize {

  case object mapSeq extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case primitives.map => Success(primitives.mapSeq)
      case _ => Failure(mapSeq)
    }
  }

  // only transforms maps which contain ForeignFunctions or mapSeqs
  case object mapSeqCompute extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      // (mapSeq λη1. (my_abs η1))
      case Apply(`map`, l@Lambda(_, Apply(ForeignFunction(_, _), _))) => Success(Apply(primitives.mapSeq, l))
      // (map λη1. ((mapSeq λη2. (my_abs η2)) η1))
      case Apply(`map`, l@Lambda(_, Apply(Apply(primitives.mapSeq, _), _))) => Success(Apply(primitives.mapSeq, l))
      case _ => Failure(mapSeqCompute)
    }
  }

  case object reduceSeq extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case primitives.reduce => Success(primitives.reduceSeq)
      case _ => Failure(reduceSeq)
    }
  }

  case class slideSeq(rot: primitives.slideSeq.Rotate) extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case primitives.slide => Success(primitives.slideSeq(rot))
      case _ => Failure(slideSeq(rot))
    }
  }
}