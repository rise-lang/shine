package FSmooth

import DSL._

//noinspection DuplicatedCode
object PairFunctionConstants {
  case class pair(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = implM(M1 => implM(M2 =>
      M1 -> M2 -> (M1 x M2)
    ))
    override def copy(t: Type): Constants = pair(Some(t))
  }

  case class fst(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = implM(M1 => implM(M2 =>
      (M1 x M2) -> M1
    ))
    override def copy(t: Type): Constants = fst(Some(t))
  }

  case class snd(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = implM(M1 => implM(M2 =>
      (M1 x M2) -> M2
    ))
    override def copy(t: Type): Constants = snd(Some(t))
  }
}
