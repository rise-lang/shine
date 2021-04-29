package parser

import OpType.UnaryOpType.UnaryOp

/*
  is the span of the specified Token
  the exact span is needed to point to the faulty position if the parser isn't able to continue
 */
// TODO BufferedSource instead of String
case class Location (column: Int, row: Int){
  //row and line positive numbers
  require(row >= 0, "row is negative")
  require(column >= 0, "column is negative")

  override def toString: String = s"($column,$row)"
  def ==(end:Location) = this.column == end.column && this.row == end.row
}

case class Span(file: FileReader, begin: Location, end: Location) {
  // TODO make more solid (begin is safe to be smaller than end etc)
  if(end.column == begin.column){
    require(end.row >= begin.row, "they have the same column/line and end.row is before begin.row")
  }
  require(end.column >= begin.column, "end.column is before begin.column")

  def this(file: FileReader, loc: Location) = this(file, loc, loc) //beginLocation is equal to endLocation for '/'
  override def toString = if(begin.column==end.column){
    if(begin.row==end.row){
      end.toString + " in " + file.fileName
    }else{
      "("+begin.column+"," + begin.row+"-"+end.row+")" + " in " + file.fileName
    }
  }else{
    begin.toString+ "-"+end.toString + " in " + file.fileName
  }

  def ==(span: Span): Boolean = span.begin == this.begin && span.end == this.end &&span.file.fileName.equals(this.file.fileName)

  /*
  both have to be in the same file!
   */
  def isAfter(other:Span):Boolean={
    if(other.end.column<this.begin.column) return true
    if(other.end.column>this.begin.column) return false

    if(other.end.row<=this.begin.row) return true
    if(other.end.row>this.begin.row) return false

    throw new IllegalStateException("The Function isAfter in Span should never reach this point")
  }

  def +(other:Span):Span={
    if(this.file.fileName!=other.file.fileName){
      throw new IllegalArgumentException("You try to add two spans of different files")
    }
    if(other.isAfter(this)){
      println("after:"+  "this.begin: "+ this.begin +"; this.end : " + this.end  + ";other.begin: " + other.begin+ ";other.end: " + other.end)
        Span(this.file, this.begin, other.end)
    }else{
      println("before:"+  "this.begin: "+ this.begin +"; this.end : " + this.end  + ";other.begin: " + other.begin+ ";other.end: " + other.end)
        Span(this.file, other.begin, this.end)
    }
  }

  def returnMessage():String={
    val begin = this.begin
    val end = this.end
    val f = this.file
    if(begin.column==end.column){
      return f.sourceLines(begin.column).substring(begin.row, end.row)
    }

    var ret = f.sourceLines(begin.column).substring(begin.row)
    for(i <- (begin.column+1) until (end.column)){
       ret+= this.file.sourceLines(i)
    }
    ret += f.sourceLines(end.column).substring(0, end.column)
    ret
  }
}
object Span {
  import parser.parse._
  def getSpanSynElem(synElem:SyntaxElement):Option[Span]={
    synElem match {
      case SExprClutched(_, spanClutch) => Some(spanClutch)
      case SLet(span) => Some(span)
      case SAddrSpace(_, span)=> Some(span)
      case SType(_, span)=> Some(span)
      case SExpr(expr) => expr.span
      case SNat(_, span) => Some(span)
      case SData(_, span) => Some(span)
      case SIntToExpr(_, span)=> Some(span)
    }
  }
  def combineOptionSpan(sp1:Option[Span],sp2:Option[Span]): Option[Span] ={
    sp1 match{
      case None => None
      case Some(span1)=>sp2 match {
        case None =>None
        case Some(span2)=>Some(span1+span2)
      }
    }
  }
  def getSpanOfFirstElemInSeq(seq: Seq[rise.core.types.Constraint]): Option[parser.Span]= seq match {
    case Seq(first, tail @ _*) =>first.span
    case Seq() => None
  }

  def getSpanListOfSeq(seq: Seq[rise.core.types.Constraint]): List[Option[parser.Span]]= seq match {
    case Seq(first, tail @ _*) =>first.span :: getSpanListOfSeq(tail)
    case Seq() => Nil
  }
}

/*
a symbol/token, which represents a Lexem: a logical part of the Code

Kinds of Token: //(Definition of <number> etc. is in EBNF.txt)
  Lexem | Tokenname| Attributwert
( | lBrace| _
) | rBrace| _
<Identifier> | Identifier| (Name:String, span:Location) <- Zeile:number als String speichern?
<Typ> | Typen| (Name:Typ, span:Location) <-Typ als sumType (enum) https://medium.com/@shannonbarnes_85491/algebraic-data-types-in-scala-701f3227fe91
\ | Backslash | _
-> | Arrow| _
: | Dots | _
<Operator> | operator | (Name:Operator, span:Location)
<number> | Number | (Name:Number, span:Location)
<Adressspace> |Adressspace | (Name:Adressspace, span: Span)

class hierachie:
sealed abstract class Token (span: Span)
  1.final case class lBrace (span: Span) extends Token(span)
  2.final case class rBrace (span: Span) extends Token(span)
  3.final case class Identifier (identifierType: IdentifierType , span: Span) extends Token(span)
  4.final case class Typ (typType: TypType, span: Span) extends Token(span)
  5.final case class Backslash (span: Span) extends Token(span)
  6.final case class Arrow (span: Span) extends Token(span)
  7.final case class Dots (span: Span) extends Token(span)
  8.final case class Operator (opType:OpType, span: Span) extends Token(span)
  9.abstract sealed class Number (span: Span) extends Token(span)
    9.1.final case class shortN (number: Short, span: Span) extends Number(span)
    9.2.final case class floatN (number: Float, span: Span) extends Number(span)
    9.3.final case class doubleN (number: Double, span: Span) extends Number(span)
    9.4.final case class intN (number: Int, span: Span) extends Number(span)
  10.final case class Adressspace (addrSpaceType: AddrSpaceType, span: Span ) extends Token(span)

We can see it is a low-hierachie with Token on the top.
Token has Location as an argument, which is the Location of the Token.
So every subclass of Token has an Location where exact in the Code it is.
The keyword final signalizes it does not have subclasses.
lBraces and rBraces are obiously both Braces,
why are they not a subclass to Braces?
It isn't needed for the Logic of the Parser.
With this, it is faster and easier to recognize an lBrace and an rBrace in Pattern Matching:
  b match = {
    case lBrace => ...
    case rBrace => ...
    case _ => throw new IllegalArgument("b is not a Brace")
  }
It exist only an few arguments:

 #Identifier: identifierType is the Identifier as an String stored
 #Typ: typType represents the Type in RISE, this is an own complex-class-hierachie
 #Operator: opType in which the Operation is stored as an Char
 #shortN, floatN, doubleN, intN: they store the  short/float/double/int in number
 #Adressspace: addrSpaceType is the Addressprace which is stored
Why is many of this data extra stored in an own datatype for this specified purpose,
if we could directly save the Operator as an Char in Operator?
If we would save it directly in Operator, than the exact form of it is fixed and we have to make
all requirements of the char Operator directly in this class here.
This would result that this class here is larger and the Code in this area is harder to change and to understand, because
of the boilerplate, which we now have in an other File. (opType is defined in the File Operator; typType in Typ etc.)

For example "\x:Double ->" we save <Backslash><x:doubleN><Dots><doubleTyp><Arrow>
So we need the Classes Backlash, doubleN, Dots, doubleTyp and arrow to save it correctly
 */
sealed abstract class Token (span: Span){
  val s:Span = span
}

  final case class ForeignKeyword(span:Span) extends Token(span){
    override def toString: String = "'foreign'"
  }
//ForeignFct("g", "x"::"y"::Nil, "return x*y;")
  final case class ForeignFctBodyColumn(body:String, span: Span) extends Token(span){
    override def toString: String = "<ForeignFctbdyLine {\n"+body+"\n}>"
  }

  final case class LParentheses(span: Span) extends Token(span){
    require(span.begin.column == span.end.column, "span.begin.column is unequal to span.end.column")
    require((span.begin.row +1) == span.end.row, "LParentheses: (span.begin.row +1) should be span.end.row: "+ span)
    override def toString = "'('"
  }

  // ")"
  final case class RParentheses(span: Span) extends Token(span){
    require(span.begin.column == span.end.column, "span.begin.column is unequal to span.end.column")
    require((span.begin.row +1) == span.end.row, "RParentheses: (span.begin.row +1) should be span.end.row: "+ span)
    override def toString = "')'"
  }

  final case class LBracket(span: Span) extends Token(span){
    require(span.begin.column == span.end.column, "span.begin.column is unequal to span.end.column")
    require((span.begin.row +1) == span.end.row, "LBracket: (span.begin.row +1) should be span.end.row: "+ span)
    override def toString = "'['"
  }

  final case class RBracket(span: Span) extends Token(span){
    require(span.begin.column == span.end.column, "span.begin.column is unequal to span.end.column")
    require((span.begin.row +1) == span.end.row, "RBracket: (span.begin.row +1) should be span.end.row: "+ span)
    override def toString = "']'"
  }
  final case class LBraces(span: Span) extends Token(span){
    require(span.begin.column == span.end.column, "span.begin.column is unequal to span.end.column")
    require((span.begin.row +1) == span.end.row, "LBracket: (span.begin.row +1) should be span.end.row: "+ span)
    override def toString = "'{'"
  }
  final case class RBraces(span: Span) extends Token(span){
    require(span.begin.column == span.end.column, "span.begin.column is unequal to span.end.column")
    require((span.begin.row +1) == span.end.row, "RBracket: (span.begin.row +1) should be span.end.row: "+ span)
    override def toString = "'}'"
  }

  // example: "split"
  final case class Identifier (name: String, span: Span) extends Token(span){
    require(!name.isEmpty, "String is empty")
    //<Identifier>::=[<leer>] <Buchstaben>{<Buchstaben>|<Ziffer>| _ }
    require(name.matches("[a-z][a-zA-Z0-9_]*"), "'"+name+ "' has not the preffered structure")
    require(span.begin.column == span.end.column, "not in one column")

    override def toString = s"<$name :Identifier>"
  }

  final case class TypeIdentifier (name: String, span: Span) extends Token(span){
    require(!name.isEmpty, "String is empty")
    //<Identifier>::=[<leer>] <Buchstaben>{<Buchstaben>|<Ziffer>| _ }
    require(name.matches("[A-Z][a-zA-Z0-9_]*"), "'"+name+"'has not the preffered structure in " + span)
    require(span.begin.column == span.end.column, "not in one column")

    override def toString = s"<$name :TypeIdentifier>"
  }


  // example "Double" which is saved as <doubleTyp>
  final case class ScalarType(concreteType: ConcreteType, span: Span) extends Token(span){
    require(span.begin.column == span.end.column, "not in one column")
    val t:ConcreteType = concreteType
    override def toString = t.toString
  }

  final case class VectorType(len: Int, concreteType: ConcreteType, span: Span) extends Token(span){
    require(span.begin.column == span.end.column, "not in one column")
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
    require(span.begin.column == span.end.column, "not in one column")
    val k:ConcreteKind = concreteKind
    override def toString = k.toString
  }
  // "\"
  final case class Backslash (span: Span) extends Token(span){
    require(span.begin.column == span.end.column, "span.begin.column is unequal to span.end.column")
    require((span.begin.row +1) == span.end.row, "Backslash: (span.begin.row +1) should be span.end.row: "+ span)
    override def toString = "'\\'"
  }

  // "->"
  final case class Arrow (span: Span) extends Token(span){
    require(span.begin.column == span.end.column && (span.begin.row+1) ==span.end.row, "arrow has not lenght two")
    override def toString = "'->'"
  }

  // "=>"
  final case class DepArrow (span: Span) extends Token(span){
    require(span.begin.column == span.end.column && (span.begin.row+1) ==span.end.row, "depArrow has not lenght two")
    override def toString = "'=>'"
  }

  final case class Dot(span: Span) extends Token(span){
    require(span.begin.column == span.end.column, "span.begin.column is unequal to span.end.column")
    require((span.begin.row +1) == span.end.row, "Dot: (span.begin.row +1) should be span.end.row: "+ span)
    override def toString = "'.'"
  }

  final case class Comma(span: Span) extends Token(span){
    require(span.begin.column == span.end.column, "span.begin.column is unequal to span.end.column")
    require((span.begin.row +1) == span.end.row, "Comma: (span.begin.row +1) should be span.end.row: "+ span)
    override def toString = "','"
  }

  // ":"
  //  (\x:a.x) : a -> a
  final case class Colon(span: Span) extends Token(span){
    require(span.begin.column == span.end.column, "span.begin.column is unequal to span.end.column")
    require((span.begin.row +1) == span.end.row, "Colon: (span.begin.row +1) should be span.end.row: "+ span)
    override def toString = "':'"
  }

  final case class DoubleColons(span: Span) extends Token(span){
    override def toString = "'::'"
  }

  final case class EqualsSign(span: Span) extends Token(span){
    require(span.begin.column == span.end.column, "span.begin.column is unequal to span.end.column")
    require((span.begin.row +1) == span.end.row, "EqualsSign: (span.begin.row +1) should be span.end.row: "+ span)
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
    require(span.begin.column == span.end.column, "span.begin.column is unequal to span.end.column")
    require((span.begin.row +1) == span.end.row, "BinOp: (span.begin.row +1) should be span.end.row: "+ span)
    override def toString = opType.toString
  }

  // example: "!"
  final case class UnOp(opType: UnaryOp, span: Span) extends Token(span){
    require(span.begin.column == span.end.column, "span.begin.column is unequal to span.end.column")
    require((span.begin.row +1) == span.end.row, "UnOP: (span.begin.row +1) should be span.end.row: "+ span)
    override def toString = opType.toString
  }

  // example: "32" which is saved as <32, intN>
  abstract sealed class Number (span: Span) extends Token(span){
    require(span.begin.column == span.end.column, "not in one column")
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
  require(span.begin.column == span.end.column, "not in one column") //Todo: Check if the lenght of the span fits the lenght of "Local", "Private", "Global" or "Constant"
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