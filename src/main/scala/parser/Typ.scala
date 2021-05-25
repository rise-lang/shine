package parser

abstract sealed class ConcreteKind()
  final case class Data() extends ConcreteKind{ //Data is the same like Type
    override def toString = "<data>"
  }
  final case class AddrSpace() extends ConcreteKind{
    override def toString = "<addrSpace>"
  }
  final case class Nat() extends ConcreteKind{
    override def toString = "<nat>"
  }

abstract sealed class ConcreteType()
  final case class NatTyp() extends ConcreteType {
    override def toString = s"<NatTyp>"
  }

  final case class BoolType() extends ConcreteType {
    override def toString = s"<Bool>"
  }

  //<i>::= i32|i64
  abstract sealed class I() extends ConcreteType

  final case class ShortTyp() extends I {
    override def toString = s"<I16>"
  }

  final case class IntTyp() extends I {
    override def toString = s"<Int>"
  }

  //<f>::= f32|f64
  abstract sealed class F() extends ConcreteType

  final case class FloatTyp() extends F {
    override def toString = s"<Float>"
  }

  final case class DoubleType() extends F {
    override def toString = s"<Double>"
  }

