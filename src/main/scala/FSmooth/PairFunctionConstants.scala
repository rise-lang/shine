package FSmooth

import DSL._

//noinspection DuplicatedCode
object PairFunctionConstants {
  case class pair(override val t: Type) extends Constants(t) {
    override def typeScheme: Type = implM(M1 => implM(M2 =>
      M1 -> M2 -> (M1 x M2)
    ))
    override def copyWithType(t: Type): Constants = pair(t)
  }

  case class fst(override val t: Type) extends Constants(t) {
    override def typeScheme: Type = implM(M1 => implM(M2 =>
      (M1 x M2) -> M1
    ))
    override def copyWithType(t: Type): Constants = fst(t)
  }

  case class snd(override val t: Type) extends Constants(t) {
    override def typeScheme: Type = implM(M1 => implM(M2 =>
      (M1 x M2) -> M2
    ))
    override def copyWithType(t: Type): Constants = snd(t)
  }
}
