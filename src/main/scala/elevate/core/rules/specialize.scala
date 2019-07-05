package elevate.core.rules

import elevate.core.{NotFound, Strategy}
import lift.core.primitives

object specialize {
  def mapSeq: Strategy = {
    case primitives.map => primitives.mapSeq
    case _ => throw NotFound(mapSeq)
  }

  def reduceSeq: Strategy = {
    case primitives.reduce => primitives.reduceSeq
    case _ => throw NotFound(reduceSeq)
  }

  def slideSeq(rot: primitives.slideSeq.Rotate): Strategy = {
    case primitives.slide => primitives.slideSeq(rot)
    case _ => throw NotFound(elevate.core.rules.specialize.slideSeq(rot))
  }
}