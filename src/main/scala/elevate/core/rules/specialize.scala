package elevate.core.rules

import elevate.core.{NotApplicable, Strategy}
import lift.core.primitives

object specialize {
  def mapSeq: Strategy = Rule({
    case primitives.map => primitives.mapSeq
    case _ => throw NotApplicable(mapSeq)
  })

  def reduceSeq: Strategy = Rule({
    case primitives.reduce => primitives.reduceSeq
    case _ => throw NotApplicable(mapSeq)
  })

  def slideSeq(rot: primitives.slideSeq.Rotate): Strategy = Rule({
    case primitives.slide => primitives.slideSeq(rot)
    case _ => throw NotApplicable(mapSeq)
  })
}