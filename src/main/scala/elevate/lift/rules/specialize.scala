package elevate.lift.rules

import elevate.core.{Failure, NotApplicable, Strategy, Success}
import lift.core.primitives

object specialize {
  def mapSeq: Strategy = {
    case primitives.map => Success(primitives.mapSeq)
    case _ => Failure(mapSeq)
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