package parser

/*
nat is a natural number
it is needed for arrayTypes (how long is the array)
 */
case class Nat(number: Int){
  require(number >= 0, "number is negative")
  override def toString = s"<$number:nat>"
}


abstract sealed class ConcreteKind()
  final case class DataK() extends ConcreteKind{
    override def toString = "<dataKind>"
  }
  final case class TypeK() extends ConcreteKind{
    override def toString = "<typeKind>"
  }
  final case class AddrSpaceK() extends ConcreteKind{
    override def toString = "<addrSpaceKind>"
  }
  final case class NatK() extends ConcreteKind{
    override def toString = "<natKind>"
  }

/*
  the different possible Types in a hierachy
  this resembles and represents the hierachie of RISE
  With this Types we perform Pattern-Matching to reconstruct the logic of the given Code and
    to create with this Types an interface with the existing Logic of RISE, which is yet
    an embedded language. This own RISE-Parser should make it possible to have for example better Errormessages

  class-hierachie:
 abstract sealed class TypType()
  1.abstract sealed class functionTypes() extends TypType
    //<LambdaTyp>::=<Typ> -> <Typ>
    1.1.final case class LambdaTyp(typ1:TypType, typ2:TypType) extends  functionTypes
    //<GenericsTyp>::=(<Identifier> : <Kind>) -> <Typ>
    1.2.final case class GenericsTyp(identifier:Identifier, kind:Kind, typ:TypType) extends functionTypes
  2.abstract sealed class dataTypes() extends TypType
    2.1abstract sealed class simpleTypes() extends dataTypes
      2.1.1final case class natTyp(number:nat) extends simpleTypes
      2.1.2abstract sealed class basic() extends simpleTypes
        2.1.2.1final case class boolTyp(bool: Boolean) extends basic
        2.1.2.2abstract sealed class z() extends basic
          //<i>::= i32|i64
          2.1.2.2.1abstract sealed class i() extends z
            2.1.2.2.1.1final case class shortTyp() extends i
            2.1.2.2.1.2final case class intTyp() extends i
          //<f>::= f32|f64
          2.1.2.2.2abstract sealed class f() extends z
            2.1.2.2.2.1final case class floatTyp() extends f
            2.1.2.2.2.2final case class doubleTyp() extends f
        //<indexType>::=idx"["<nat>"]"
        2.1.2.3final case class indexType(number:nat) extends basic
      2.2abstract sealed class complexType() extends dataTypes
        //<tupleTypes>::="("<Typ>,<Typ>")"
        2.2.1final case class tupleType(typ1:TypType, typ2:TypType) extends complexType
        //<arrayTypes>::=<nat>.<Typ>
        2.2.2final case class arrayType(number:nat, typ:TypType) extends complexType

 For LambdaTyp we have two Types as arguments, because <LambdaTyp>::=<Typ> -> <Typ>
 For GenericsType etc. are the types because of the complex structure.
 We only save types here. Tokens and the value of an Float are saved in Token.
 Here we save, if in the code is  "\x:Double ->" we save <Backslash><x:doubleN><Dots><doubleTyp><Arrow>
 */
//TODO add whitespace
abstract sealed class ConcreteType()

  abstract sealed class FunctionTypes() extends ConcreteType

  //<LambdaTyp>::=<Typ> -> <Typ>
  final case class FunctionTyp(typ1: ConcreteType, typ2: ConcreteType) extends FunctionTypes {
    val t1: ConcreteType = typ1
    val t2: ConcreteType = typ2

    override def toString = t1.toString + " -> " + t2.toString
  }

  //<GenericsTyp>::=(<Identifier> : <Kind>) -> <Typ>
  final case class GenericsTyp(identifier: Identifier, kind: ConcreteKind, typ: ConcreteType) extends FunctionTypes {
    val i: Identifier = identifier
    val k: ConcreteKind = kind
    val t: ConcreteType = typ

    override def toString = "( " + i.toString + " : " + k.toString + " ) -> " + t.toString
  }

  abstract sealed class DataTypes() extends ConcreteType

  abstract sealed class SimpleTypes() extends DataTypes

  final case class NatTyp() extends SimpleTypes {
    override def toString = s"<NatTyp>"
  }

  abstract sealed class Basic() extends SimpleTypes

  final case class BoolType() extends Basic {
    override def toString = s"<Bool>"
  }

  abstract sealed class Z() extends Basic

  //<i>::= i32|i64
  abstract sealed class I() extends Z

  final case class ShortTyp() extends I {
    override def toString = s"<I16>"
  }

  final case class IntTyp() extends I {
    override def toString = s"<Int>"
  }

  //<f>::= f32|f64
  abstract sealed class F() extends Z

  final case class FloatTyp() extends F {
    override def toString = s"<Float>"
  }

  final case class DoubleType() extends F {
    override def toString = s"<Double>"
  }

  //<indexType>::=idx"["<nat>"]"
  final case class IndexType(number: Nat) extends Basic {
    val n: Nat = number

    override def toString = s"idx[$n]"
  }

  abstract sealed class ComplexType() extends DataTypes

  //<tupleTypes>::="("<Typ>,<Typ>")"
  final case class TupleType(typ1: ConcreteType, typ2: ConcreteType) extends ComplexType {
    val t1: ConcreteType = typ1
    val t2: ConcreteType = typ2

    override def toString = "( " + t1.toString + " , " + t2.toString + " )"
  }

  //<arrayTypes>::=<nat>.<Typ>
  final case class ArrayType(number: Nat, typ: ConcreteType) extends ComplexType {
    val n: Nat = number
    val t: ConcreteType = typ

    override def toString = s"$n" + "." + typ.toString
  }

