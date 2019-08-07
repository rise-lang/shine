package FSmooth

import DSL._

object VectorFunctionConstants {
  case object build extends Constants {
     override def t: Type = Card -> (Index -> Double) -> Array(Double)
  }

  case object ifold extends Constants {
    override def t: Type = (Double -> Index -> Double) -> Double -> Card -> Double
  }

  case object get extends Constants {
    override def t: Type = Array(Double) -> Index -> Double
  }

  case object length extends Constants {
    override def t: Type = Array(Double) -> Card
  }
}
