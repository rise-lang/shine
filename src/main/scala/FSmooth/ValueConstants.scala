package FSmooth

//noinspection DuplicatedCode
object ValueConstants {
  sealed abstract class ValueConstants(override val t: Type) extends Constants(t) {
    override def typeScheme: Type = t
    override def setType(t: Type): Constants = this
  }

  case object `true` extends ValueConstants(Bool)
  case object `false` extends ValueConstants(Bool)
}
