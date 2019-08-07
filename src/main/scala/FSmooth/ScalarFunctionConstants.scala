package FSmooth

import DSL._

object ScalarFunctionConstants {
  case object `+` extends Constants {
    override def t: Type = (Double x Double) -> Double
  }

  case object `-` extends Constants {
    override def t: Type = (Double x Double) -> Double
  }

  case object `*` extends Constants {
    override def t: Type = (Double x Double) -> Double
  }

  case object `/` extends Constants {
    override def t: Type = (Double x Double) -> Double
  }

  case object `**` extends Constants {
    override def t: Type = (Double x Double) -> Double
  }

  case object sign extends Constants {
    override def t: Type = Double -> Double
  }

  case object cos extends Constants {
    override def t: Type = Double -> Double
  }

  case object tan extends Constants {
    override def t: Type = Double -> Double
  }

  case object log extends Constants {
    override def t: Type = Double -> Double
  }

  case object exp extends Constants {
    override def t: Type = Double -> Double
  }

  case object `>` extends Constants {
    override def t: Type = (Double -> Double) -> Bool
  }

  case object `<` extends Constants {
    override def t: Type = (Double -> Double) -> Bool
  }

  case object `=:=` extends Constants {
    override def t: Type = (Double -> Double) -> Bool
  }

  case object `<>` extends Constants {
    override def t: Type = (Double -> Double) -> Bool
  }

  case object `&&` extends Constants {
    override def t: Type = (Double -> Double) -> Bool
  }

  case object `||` extends Constants {
    override def t: Type = (Double -> Double) -> Bool
  }

  case object `!` extends Constants {
    override def t: Type = Bool -> Bool
  }
}
