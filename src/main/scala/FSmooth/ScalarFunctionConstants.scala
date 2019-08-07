package FSmooth

import DSL._

//noinspection DuplicatedCode
object ScalarFunctionConstants {
  case class `+`(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = implNum(Num => Num -> Num -> Num )
    override def copy(t: Type): Constants = new `+`(Some(t))
  }

  case class `-`(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = implNum(Num => Num -> Num -> Num )
    override def copy(t: Type): Constants = new `-`(Some(t))
  }

  case class `*`(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = implNum(Num => Num -> Num -> Num )
    override def copy(t: Type): Constants = `*`(Some(t))
  }

  case class `/`(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = implNum(Num => Num -> Num -> Num )
    override def copy(t: Type): Constants = `/`(Some(t))
  }

  case class `**`(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = implNum(Num => Num -> Num -> Num )
    override def copy(t: Type): Constants = `**`(Some(t))
  }

  case class sign(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = implNum(Num => Num -> Num )
    override def copy(t: Type): Constants = sign(Some(t))
  }

  case class cos(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = implNum(Num => Num -> Num )
    override def copy(t: Type): Constants = cos(Some(t))
  }

  case class tan(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = implNum(Num => Num -> Num )
    override def copy(t: Type): Constants = tan(Some(t))
  }

  case class log(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = implNum(Num => Num -> Num )
    override def copy(t: Type): Constants = log(Some(t))
  }

  case class exp(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = implNum(Num => Num -> Num )
    override def copy(t: Type): Constants = exp(Some(t))
  }

  case class `>`(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = implNum(Num => (Num -> Num) -> Bool )
    override def copy(t: Type): Constants = `>`(Some(t))
  }

  case class `<`(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = implNum(Num => (Num -> Num) -> Bool )
    override def copy(t: Type): Constants = `<`(Some(t))
  }

  case class `=:=`(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = implNum(Num => (Num -> Num) -> Bool )
    override def copy(t: Type): Constants = `=:=`(Some(t))
  }

  case class `<>`(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = implNum(Num => (Num -> Num) -> Bool )
    override def copy(t: Type): Constants = `<>`(Some(t))
  }

  case class `&&`(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = implNum(Num => (Num -> Num) -> Bool )
    override def copy(t: Type): Constants = `&&`(Some(t))
  }

  case class `||`(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = implNum(Num => (Num -> Num) -> Bool )
    override def copy(t: Type): Constants = `||`(Some(t))
  }

  case class `!`(override val t: Option[Type]) extends Constants(t) {
    override def typeScheme: Type = Bool -> Bool
    override def copy(t: Type): Constants = new `!`(Some(t))
  }
}
