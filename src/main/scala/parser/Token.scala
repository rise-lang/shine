package parser

import OpType.UnaryOpType.UnaryOp

sealed abstract class Token (span: Span){
  val s:Span = span
  val begin = span.range.begin
  val end = span.range.end
}

  final case class ForeignKeyword(span:Span) extends Token(span){
    override def toString: String = "'foreign'"
  }
//ForeignFct("g", "x"::"y"::Nil, "return x*y;")
  final case class ForeignFctBodyColumn(body:String, span: Span) extends Token(span){
    override def toString: String = "<{"+body+"}>"
  }

  final case class LParentheses(span: Span) extends Token(span){
    require(begin.column == end.column, "begin.column is unequal to end.column")
    require((begin.row +1) == end.row, "LParentheses: (begin.row +1) should be end.row: "+ span)
    override def toString = "'('"
  }

  // ")"
  final case class RParentheses(span: Span) extends Token(span){
    require(begin.column == end.column, "begin.column is unequal to end.column")
    require((begin.row +1) == end.row, "RParentheses: (begin.row +1) should be end.row: "+ span)
    override def toString = "')'"
  }

  final case class LBracket(span: Span) extends Token(span){
    require(begin.column == end.column, "begin.column is unequal to end.column")
    require((begin.row +1) == end.row, "LBracket: (begin.row +1) should be end.row: "+ span)
    override def toString = "'['"
  }

  final case class RBracket(span: Span) extends Token(span){
    require(begin.column == end.column, "begin.column is unequal to end.column")
    require((begin.row +1) == end.row, "RBracket: (begin.row +1) should be end.row: "+ span)
    override def toString = "']'"
  }
  final case class LBraces(span: Span) extends Token(span){
    require(begin.column == end.column, "begin.column is unequal to end.column")
    require((begin.row +1) == end.row, "LBracket: (begin.row +1) should be end.row: "+ span)
    override def toString = "'{'"
  }
  final case class RBraces(span: Span) extends Token(span){
    require(begin.column == end.column, "begin.column is unequal to end.column")
    require((begin.row +1) == end.row, "RBracket: (begin.row +1) should be end.row: "+ span)
    override def toString = "'}'"
  }

  object Identifier{
    val regex = "[a-z][a-zA-Z0-9_]*"
  }
  // example: "split"
  final case class Identifier (name: String, span: Span) extends Token(span){
    require(!name.isEmpty, "String is empty")
    //<Identifier>::=[<leer>] <Buchstaben>{<Buchstaben>|<Ziffer>| _ }
    require(name.matches(Identifier.regex), "'"+name+ "' has not the preffered structure")
    require(begin.column == end.column, "not in one column")

    override def toString = s"<$name :Identifier>"
  }

  object TypeIdentifier{
    val regex = "[A-Z][a-zA-Z0-9_]*"
  }
  final case class TypeIdentifier (name: String, span: Span) extends Token(span){
    require(!name.isEmpty, "String is empty")
    //<Identifier>::=[<leer>] <Buchstaben>{<Buchstaben>|<Ziffer>| _ }
    require(name.matches(TypeIdentifier.regex), "'"+name+"'has not the preffered structure in " + span)
    require(begin.column == end.column, "not in one column")

    override def toString = s"<$name :TypeIdentifier>"
  }


  // example "Double" which is saved as <doubleTyp>
  final case class ScalarType(concreteType: ConcreteType, span: Span) extends Token(span){
    require(begin.column == end.column, "not in one column")
    val t:ConcreteType = concreteType
    override def toString = t.toString
  }

  final case class VectorType(len: Int, concreteType: ConcreteType, span: Span) extends Token(span){
    require(begin.column == end.column, "not in one column")
    len match {
      case 2 =>
      case 4 =>
      case 8 =>
      case 16 =>
      case illegalLength => throw new IllegalArgumentException("The length " + illegalLength + " is not an accepted length")
    }
    val t:ConcreteType = concreteType
    override def toString = "Vec("+len+","+t.toString+")"
  }

  final case class Kind(concreteKind: ConcreteKind, span: Span) extends Token(span){
    require(begin.column == end.column, "not in one column")
    val k:ConcreteKind = concreteKind
    override def toString = k.toString
  }
  // "\"
  final case class Backslash (span: Span) extends Token(span){
    require(begin.column == end.column, "begin.column is unequal to end.column")
    require((begin.row +1) == end.row, "Backslash: (begin.row +1) should be end.row: "+ span)
    override def toString = "'\\'"
  }

  // "->"
  final case class Arrow (span: Span) extends Token(span){
    require(begin.column == end.column && (begin.row+1) ==end.row, "arrow has not lenght two")
    override def toString = "'->'"
  }

  // "=>"
  final case class DepArrow (span: Span) extends Token(span){
    require(begin.column == end.column && (begin.row+1) ==end.row, "depArrow has not lenght two")
    override def toString = "'=>'"
  }

  final case class Dot(span: Span) extends Token(span){
    require(begin.column == end.column, "begin.column is unequal to end.column")
    require((begin.row +1) == end.row, "Dot: (begin.row +1) should be end.row: "+ span)
    override def toString = "'.'"
  }

  final case class Comma(span: Span) extends Token(span){
    require(begin.column == end.column, "begin.column is unequal to end.column")
    require((begin.row +1) == end.row, "Comma: (begin.row +1) should be end.row: "+ span)
    override def toString = "','"
  }

  // ":"
  //  (\x:a.x) : a -> a
  final case class Colon(span: Span) extends Token(span){
    require(begin.column == end.column, "begin.column is unequal to end.column")
    require((begin.row +1) == end.row, "Colon: (begin.row +1) should be end.row: "+ span)
    override def toString = "':'"
  }

  final case class DoubleColons(span: Span) extends Token(span){
    override def toString = "'::'"
  }

  final case class EqualsSign(span: Span) extends Token(span){
    require(begin.column == end.column, "begin.column is unequal to end.column")
    require((begin.row +1) == end.row, "EqualsSign: (begin.row +1) should be end.row: "+ span)
    override def toString = "'='"
  }

object OpType {
  object UnaryOpType extends Enumeration {
    type UnaryOp = Value
    val NEG: UnaryOp = Value("'~'") //it's not clear which one is which alone from the representation "-"
    val NOT: UnaryOp = Value("'!'")
  }

  object BinOpType extends Enumeration {
    type BinaryOp = Value
      val ADD: BinaryOp = Value("'+'")
      val SUB: BinaryOp = Value("'-'")
      val MUL: BinaryOp = Value("'*'")
      val DIV: BinaryOp = Value("'/'")
      val MOD: BinaryOp = Value("'%'")
      val GT: BinaryOp = Value("'>'")
      val LT: BinaryOp = Value("'<'")
      val EQ: BinaryOp = Value("'=='")
  }
}
import OpType.BinOpType._

  // example: "+"
  final case class BinOp(opType: BinaryOp, span: Span) extends Token(span){
    require(begin.column == end.column, "begin.column is unequal to end.column")
    require((begin.row +1) == end.row, "BinOp: (begin.row +1) should be end.row: "+ span)
    override def toString = opType.toString
  }

  // example: "!"
  final case class UnOp(opType: UnaryOp, span: Span) extends Token(span){
    require(begin.column == end.column, "begin.column is unequal to end.column")
    require((begin.row +1) == end.row, "UnOP: (begin.row +1) should be end.row: "+ span)
    override def toString = opType.toString
  }

  object Number{
    val regex = "[0-9]+[.]?[0-9]*"
  }

  // example: "32" which is saved as <32, intN>
  abstract sealed class Number (span: Span) extends Token(span){
    require(begin.column == end.column, "not in one column")
  }
    final case class I8(number: Short, span: Span) extends Number(span) {
      override def toString = s"<$number:I8>"
    }

    final case class I32(number: Int, span: Span) extends Number(span){
      override def toString = s"<$number:I32>"
    }

    final case class F32(number: Float, span: Span) extends Number(span){
      override def toString = s"<$number:F32>"
    }

    final case class F64(number: Double, span: Span) extends Number(span){
      override def toString = s"<$number:F64>"
    }

    case class NatNumber(number: Int, span: Span) extends Number(span){
      override def toString = s"<$number:nat>"
    }

// example: "Local" which is saved as <Local, Adressspace>
final case class AddrSpaceType(addrSpace: String, span: Span) extends Token(span){
  require(addrSpace.equals("Global")||addrSpace.equals("Constant")||
    addrSpace.equals("Local")||addrSpace.equals("Private"), "not an accepted AddrSpaceType")
  val a:String = addrSpace
  require(begin.column == end.column, "not in one column") //Todo: Check if the lenght of the span fits the lenght of "Local", "Private", "Global" or "Constant"
  override def toString = s"<$addrSpace: AddrSpace>"
}



final case class BeginTypAnnotatedIdent(span: Span) extends Token(span){
  override  def toString = "<BeginTypAnn>"
}
final case class EndTypAnnotatedIdent(span: Span) extends Token(span){
  override  def toString = "<EndTypAnn>"
}
final case class BeginNamedExpr(span: Span) extends Token(span){
  override  def toString = "<BeginNamedExpr>"
}
final case class EndNamedExpr(span: Span) extends Token(span){
  override  def toString = "<EndNamedExpr>"
}
final case class BeginForeignFct(span: Span) extends Token(span){
  override def toString: String = "<BeginForeignFct>"
}
final case class EndForeignFct(span: Span) extends Token(span){
  override def toString: String = "<EndForeignFct>"
}