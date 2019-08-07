package FSmooth

import DSL._

object PairFunctionConstants {
  case object pair extends Constants {
    override def t: Type = Double -> Double -> (Double x Double)
  }

  case object fst extends Constants {
    override def t: Type = (Double x Double) -> Double
  }

  case object snd extends Constants {
    override def t: Type = (Double x Double) -> Double
  }
}
