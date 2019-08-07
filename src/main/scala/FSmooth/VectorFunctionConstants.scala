package FSmooth

import DSL._

object VectorFunctionConstants {
  case class build(override val t: Type) extends Constants(t) {
     override def typeScheme: Type = implM(M => Card -> (Index -> M) -> Array(M))
    override def copy(t: Type): Constants = build(t)
  }

  case class ifold(override val t: Type) extends Constants(t) {
    override def typeScheme: Type = implM(M => (M -> Index -> M) -> M -> Card -> M)
    override def copy(t: Type): Constants = ifold(t)
  }

  case class get(override val t: Type) extends Constants(t) {
    override def typeScheme: Type = implM(M => Array(M) -> Index -> M)
    override def copy(t: Type): Constants = get(t)
  }

  case class length(override val t: Type) extends Constants(t) {
    override def typeScheme: Type = implM(M => Array(M) -> Card)
    override def copy(t: Type): Constants = length(t)
  }
}
