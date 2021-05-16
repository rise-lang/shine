package parser

import java.nio.file.Paths
import rise.{core => r, openCL => o}
import r.{DSL => rd, primitives => rp, semantics => rS, types => rt}
import o.{primitives => op}
import parser.parse.SpanPlaceholder
import rise.core.types.{Kind, Type}

/*
https://stackoverflow.com/questions/38243530/custom-exception-in-scala is reference
 */
trait ParserError { self: Throwable =>
  def span: Span
}



abstract sealed class ErrorMessage(span:Option[Span]) {
  val sp = span
  val s = span.getOrElse(SpanPlaceholder)
  def getErrorStackTraceElem():StackTraceElement=Thread.currentThread.getStackTrace().tail.tail.tail.tail.head
  val errorStackTraceElem = getErrorStackTraceElem()
  val fileReader = s.file
  val begin = s.begin
  val end = s.end
  private def underline(string:String):String = {
    var str:String = ""
    for( a <- string){ str = str + a + "\u0332" }
    str
  }

  private def importantPart(str: String): String = {
    val string: String = underline(str)
    string
  }

  def description():String= {
    throw new Exception("descriptionError method must be overridden")
  }
  def returnMessage():String ={
    val e = errorStackTraceElem
    val filePath = Paths.get("src/main/scala/"+e.getClassName.substring(0,e.getClassName.indexOf('.'))+"/"+e.getFileName+":"+e.getLineNumber).toUri

    val (m,before,after, loc) = span match {
      case Some(sp) => (
        sp.returnMessage(),
        sp.file.sourceLines(begin.column).substring(0, begin.row),
        sp.file.sourceLines(end.column).substring(end.row),
        sp.toString    //exact location of error, related code of error, short description of error
      )
      case None => ("","","","")
    }
    val important = importantPart(m)

    val relatedCode = before + important + after
    val desc = description()

    val message = desc + " : " + loc + " : '" + relatedCode +"'" + " <<<"+
      e.getLineNumber + " in '" + e.getMethodName + "' in '" +e.getFileName + "' " + filePath +">>>"
      //e.getClassName + " "+

    message
  }

  def throwException():Unit ={
    val message = returnMessage()

    throw new Exception(message) with ParserError {
      override def span: Span = s
    }
  }

  override def toString: String = returnMessage()


  def ==(error:ErrorMessage): Boolean = error.s==this.s&&error.description().equals(this.description())
}

//______________________________________________________________________________________________________
//Lexer

abstract sealed class PreAndErrorToken(span: Span) extends ErrorMessage(Some(span))

final case class EndOfLine(span:Span) extends PreAndErrorToken(span){
  require(span.begin == span.end, "span.begin is unequal to span.end")
  override def description() = "End of Line"
}
final case class EndOfFile(span:Span) extends PreAndErrorToken(span){
  require(span.begin == span.end, "span.begin is unequal to span.end")
  override def description() = "End of File"
}
final case class UnknownSymbol(c:Char, span:Span) extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def description() = "The character '" + c + "' is unknown"
}
final case class OnlyOneEqualSign(span:Span) extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def description() = "Only one EqualSign"
}
final case class NegSign(span:Span) extends PreAndErrorToken(span){
  require(span.begin == span.end, "span.begin is unequal to span.end")
  override def description() = "A Negative Sign"
}
final case class IdentifierWithNotAllowedSymbol(unknownSymbol:Char, str:String, span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def description() = "The Identifier '" + str + "' has an unknown Symbol '" + unknownSymbol + "'"
}
final case class IdentifierBeginsWithUnderscore(span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def description() = "Identifier with an Underscore"
}
final case class IdentifierBeginsWithDigits(str:String, span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def description() = "The Identifier '"+ str+ "' begins with an Digits/a Number"
}
final case class NumberWithUnknownSymbol(unknownSymbol: Char, str:String, span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def description() = "The Number '" + str + "' has an unknown Symbol '" + unknownSymbol + "'"
}
final case class F32DeclaredAsI8(number:Float, span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def description() = "The number '" + number + "' is an F32 Number but it is declared as I8"
}
final case class F32DeclaredAsI32(number:Float, span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def description() = "The number '" + number + "' is an F32 Number but it is declared as I32"
}
final case class IdentifierBeginsWithAF32Number(str:String, span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def description() = "The Identifier '"+ str+ "' begins with a F32 Number"
}
final case class NumberWithUnderscore(str:String, span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def description() = "The Number '"+ str+ "' has an underscore in it"
}
final case class NotExpectedToken(expectedToken:String, givenToken:String, span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def description() = "It is an '"+ expectedToken +"' expected. The Lexeme '" + givenToken +
    "' is not an '"+ expectedToken+ "'!"
}

final case class NotExpectedTwoBackslash(expectedToken:String, span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def description() = "It is an '"+ expectedToken +"' expected. But we have here two '\\'!"
}

final case class ToShortToBeThisToken(expectedLength:Int, token:String, span:Span)
  extends PreAndErrorToken(span){
  require(expectedLength >0, "expectedLength is less or equal to zero")
  require(span.begin.column == span.end.column, "not in one column")
  override def description() = "the given length is less than "+ expectedLength +" for "+ token +"!"
}
final case class NOTanBinOperator(symbol: String, span:Span)
  extends PreAndErrorToken(span) {
  require(span.begin == span.end, "span.begin is unequal to span.end")
  override def description() = "The Symbol '" + symbol + "' is not an Operator"
}
final case class NOTanUnOperator(symbol: String, span:Span)
  extends PreAndErrorToken(span) {
  require(span.begin == span.end, "span.begin is unequal to span.end")
  override def description() = "The Symbol '" + symbol + "' is not an Operator"
}

final case class UnknownType(str: String, span:Span) extends PreAndErrorToken(span) {
  require(span.begin.column == span.end.column, "not in one column")
  override def description() = "The Type '" + str + "' is not an accepted Type in RISE"
}

final case class UnknownKind(str: String, span:Span) extends PreAndErrorToken(span) {
  require(span.begin.column == span.end.column, "not in one column")
  override def description() = "The Kind '" + str + "' is not an accepted Kind in RISE"
}

abstract sealed class ThisTokenShouldntBeHere(token: Token, span:Span)
  extends PreAndErrorToken(span) {
  require(token.s.begin.column == token.s.end.column, "not in one column")
  override def description() = "The Token '" + token.toString + "' was not here expected"
}

final case class ThisTokenShouldntBeHereExpectedArrowOrDots(token: Token, span:Span)
  extends ThisTokenShouldntBeHere(token, span)


final case class LeftBraceMissing(span:Span) extends PreAndErrorToken(span){
  override def description() = "Left Brace is missing!"
}
final case class RightBraceMissing(span:Span) extends PreAndErrorToken(span){
  override def description() = "Right Brace is missing!"
}
final case class TypeIdentifierExpectedNotIdentifier(name: String, span:Span)
  extends PreAndErrorToken(span){
  override def description() = "It is an TypeIdentifier expected. '" + name + "' is an Identifier"
}
final case class IdentifierExpectedNotTypeIdentifier(name: String, span:Span)
  extends PreAndErrorToken(span){
  override def description() = "It is an Identifier expected. '" + name + "' is an TypeIdentifier"
}


//__________________________________________________________________________________________________________
//Parser


abstract sealed class PreAndErrorSynElems(span: Span, whatToParse: String) extends ErrorMessage(Some(span)){
  def subDescription():String= {
    throw new Exception("SubDescriptionError method must be overridden")
  }

  override def description(): String = subDescription() + " ~ while parsing'" + whatToParse + "' ~"
}

final case class SynListIsEmpty(span: Span, whatToParse: String)
  extends PreAndErrorSynElems(span, whatToParse){
  override def subDescription(): String = "SyntayElementslist is empty"
}

final case class TokListIsEmpty(span: Span, whatToParse: String)
  extends PreAndErrorSynElems(span, whatToParse){
  override def subDescription(): String = "Tokenlist is empty"
}

/*
has to have at least one elem else ListIsEmpty Exeption should be used
 */
final case class TokListTooSmall(list: List[Token], minimumLen: Int, whatToParse: String)
  extends PreAndErrorSynElems(list.head.s, whatToParse){
  override def subDescription(): String = "Tokenlist size is "+ list.size+ " but min. Lenght is "+ minimumLen
}

final case class NotCorrectToken(token: Token, expectedToken:String, whatToParse: String)
  extends PreAndErrorSynElems(token.s, whatToParse){
  override def subDescription(): String = "'"+token+"' is not '"+ expectedToken+"'"
}

final case class NoKindWithThisName(kind: TypeIdentifier, whatToParse: String)
  extends PreAndErrorSynElems(kind.span, whatToParse){
  override def subDescription(): String = "TypeIdent '"+kind.name+"' is not declared"
}

final case class NotAcceptedScalarType(span:Span, notAcceptedType: ConcreteType, whatToParse: String)
  extends  PreAndErrorSynElems(span, whatToParse){
  override def subDescription(): String = "'"+notAcceptedType+"' is not an ScalarType"
}

final case class NotCorrectKind(span:Span, kind: ConcreteKind, whatToParse: String)
  extends PreAndErrorSynElems(span, whatToParse){
  override def subDescription(): String = "'"+kind+"' is not the expected Kind"
}

final case class NotCorrectSynElem(wrongSynElem: parser.parse.SyntaxElement,expectedSynElem: String, whatToParse: String)
  extends PreAndErrorSynElems(Span.getSpanSynElem(wrongSynElem).get, whatToParse){
  override def subDescription(): String = "'"+wrongSynElem+"' is not the expected SynElem '" + expectedSynElem +"'"
}

//__________________________________________________________________________________________________________
abstract sealed class UseOrFailState()
  final case class isParsing() extends UseOrFailState
  final case class isFailed() extends UseOrFailState
  final case class isMatched() extends UseOrFailState

final case class UsedOrFailedRule(state: UseOrFailState, whatToParse: String){
  override def toString():String =  {
    state match {
      case isParsing() => "parsing Rule "+ whatToParse
      case isFailed() =>  "failed Rule  "+ whatToParse
      case isMatched() => "matched Rule "+ whatToParse
    }
  }
}

//__________________________________________________________________________________________________________
//ErrorList
/*Gib aus welche Regeln angewendet werden und ob sie fehlschlagen.
  parsing NoAppExpr
  parsing Identifier
  failed to apply Rule: Identifier
  parsing Lambda
  ....
  matched Lambda
  matched NoAppExpr
 */

final case class ErrorList(){
  private var errorList: List[Either[UsedOrFailedRule, PreAndErrorSynElems]] = Nil
  private var deepestError: Int = -1

  def add(e:PreAndErrorSynElems):ErrorList={
    if(errorList.isEmpty || deepestError.equals(-1)){
       deepestError = 0
    }else{
      errorList(deepestError) match {
        case Right(newE)=> if(e.s.isAfter(newE.s)){
          deepestError = 0
        } else {
          deepestError += 1
        }
        case Left(usedOrFailedRule) => throw new IllegalStateException("Rules dont save spans and because of that can not be the deepest elem: "+ usedOrFailedRule)
      }
    }
    errorList = Right(e)::errorList
    //println("ErrorList: " +this.toString)
    this
  }
  def add(r:UsedOrFailedRule):ErrorList={
    if(!deepestError.equals(-1)){
      deepestError += 1
    }
    errorList = Left(r)::errorList
    this
  }

  /*
  so that is looks better with spaces, but if it takes too much time, it is probably better to solve it in an different way
   */
  def retSpace(space:Int): String ={
    val str = ""
    String.format("%-"+space+"s", str)
  }
  def returnDeepestElem():String = {
    //in returnList I reverse the List, so I have to reverse the number too!
    val dErrorInReverseList = errorList.length-this.deepestError-1
    "deepest Error at "+ dErrorInReverseList + ":\n"+
      this.errorList(this.deepestError).getOrElse(throw new IllegalStateException("Rules should not be the deepest elem")).returnMessage()
  }
  def returnList():String = {
    var s = "\nfull ErrorList:\n"
    val l = this.errorList.reverse
    var space = 1

    for(i <- 0 until l.size){
      if(space<0){
        println(s)
        throw new IllegalStateException("Oh no space is negative")
      }
      l(i) match {
        case Left(r)=> {
          s = s+retSpace(space)+space+";"+i+".th: "+r.toString()+ "\n"
          r.state match{
            case isParsing()=> space += 1
            case isMatched()=> space -= 1
            case isFailed() => space -= 1
          }
        }
        case Right(e)=> {
          s = s +retSpace(space)+space+";"+i+".th: "+ e.returnMessage() + "\n"
          space -= 1
        }
      }
    }
    s
  }
  override def toString: String =  returnDeepestElem()+returnList()
  def getList():List[Either[UsedOrFailedRule, PreAndErrorSynElems]]=this.errorList
  def getDeepestElemPos():Int =this.deepestError
  def getDeepestElem():PreAndErrorSynElems =this.errorList(this.deepestError).
    getOrElse(throw new IllegalStateException("Rules should not be the deepest elem"))
}

//__________________________________________________________________________________________________________
//ConstraintTypeError
abstract sealed class ConstraintInputTypes()
  case class ExpectedAndFoundT(expectedT:rt.Type,foundT:rt.Type) extends ConstraintInputTypes
  case class ExpectedAndFoundTLeftAndRight(expectedT:rt.Type,foundTLeft:rt.Type,
                                           foundTRight:rt.Type) extends ConstraintInputTypes
  case class ExpectedAndFoundTLeftAndRightDep[K <: Kind](expectedT:rt.Type,foundTLeft:rt.Type,
                                         foundTRight:K#T) extends ConstraintInputTypes
  case class ExpectedAndFoundN(expected:rt.Nat,found:rt.Nat) extends ConstraintInputTypes
  case class ExpectedAndFoundB(expected:arithexpr.arithmetic.BoolExpr,found:arithexpr.arithmetic.BoolExpr) extends ConstraintInputTypes
  case class ExpectedAndFoundA(expected:rt.AddressSpace,found:rt.AddressSpace) extends ConstraintInputTypes
  case class ExpectedAndFoundNatToData(expected:rt.NatToData,found:rt.NatToData) extends ConstraintInputTypes
  case class ExpectedAndFoundNatCollection(expected:rt.NatCollection, found:rt.NatCollection) extends ConstraintInputTypes

//Todo: find better solution than SpanPlaceholder
abstract sealed class ConstraintError(span: Option[Span]) extends ErrorMessage(span){
  var constraintTypes: Option[ConstraintInputTypes] = None
  def getTypes():List[rt.Type] = constraintTypes match {
    case None => throw new IllegalStateException("ConstraintTypes are not initialised yet")
    case Some(cT)=> cT match {
      case ExpectedAndFoundT(eT, fT) => eT::fT::Nil
      case _ => throw new IllegalStateException("wrong ConstraintType")
    }
  }
  def defineTypes(expectedT:rt.Type, foundT:rt.Type): Unit ={
    constraintTypes match {
      case Some(_) => throw new IllegalStateException("You are not supposed to call this function two times")
      case None => constraintTypes = Some(ExpectedAndFoundT(expectedT, foundT))
    }
  }
}

abstract sealed class AppLikeConstraintError(span: Option[Span]) extends ConstraintError(span){
  override def getErrorStackTraceElem():StackTraceElement=Thread.currentThread.getStackTrace().tail.tail.tail.tail.tail.tail.head
  override def getTypes():List[rt.Type] = constraintTypes match {
    case None => throw new IllegalStateException("ConstraintTypes are not initialised yet")
    case Some(cT)=> cT match {
      case ExpectedAndFoundTLeftAndRight(expectedT, foundTLeft, foundTRight) => expectedT :: foundTLeft :: foundTRight::Nil
      case a => throw new IllegalStateException("wrong ConstraintType: "+ a)
    }
  }

  override def defineTypes(expectedT: Type, foundT: Type): Unit = throw new IllegalStateException("not call this function")
  def defineTypes(expectedT:rt.Type, foundTLeft:rt.Type, foundTRight:rt.Type): Unit ={
    constraintTypes match {
      case Some(_) => throw new IllegalStateException("You are not supposed to call this function two times")
      case None => constraintTypes = Some(ExpectedAndFoundTLeftAndRight(expectedT, foundTLeft, foundTRight))
    }
  }
}

case class TypeConstraintError(span: Option[Span]) extends ConstraintError(span){
  override def description(): String = {
    val expectedT::foundT::Nil = getTypes()
    "expected '" + expectedT + "' but found '"+foundT+ "'"
  }
}

case class IdentConstraintError(span: Option[Span]) extends ConstraintError(span){
  override def description(): String = {
    val expectedT::foundT::Nil = getTypes()
    "expected '" + expectedT + "' but found '"+foundT+ "'"
  }
}

case class LambdaConstraintError(span: Option[Span]) extends ConstraintError(span){
  override def description(): String = {
    val expectedT::foundT::Nil = getTypes()
    "expected '" + expectedT + "' but found '"+foundT+ "'"
  }
}

case class AppConstraintError(span: Option[Span]) extends AppLikeConstraintError(span){
  override def defineTypes(expectedT:rt.Type, foundT:rt.Type): Unit ={
    constraintTypes match {
      case Some(_) => throw new IllegalStateException("You are not supposed to call this function two times")
      case None => constraintTypes = {
        val (inTofE, exT) = foundT match {
          case rt.FunType(inT, outT) =>  (inT, outT)
          case _ => throw new IllegalStateException("FunType is expected")
        }
        val inTofF = expectedT
        Some(ExpectedAndFoundTLeftAndRight(exT, inTofF, inTofE))
      }
    }
  }
  override def description(): String = {
    val expectedT::foundTLeft::foundTRight::Nil = getTypes()
    "expected '" + expectedT + "' but found App('"+foundTLeft+ "','"+
      foundTRight+"')"
  }
}

case class DepLambdaConstraintError(span: Option[Span]) extends ConstraintError(span){
  override def description(): String = {
    val expectedT::foundT::Nil = getTypes()
    "expected '" + expectedT + "' but found '"+foundT+ "'"
  }
}

case class DepAppConstraintError(span: Option[Span]) extends ConstraintError(span){
  override def getTypes(): List[Type] = throw new IllegalStateException("this function should not be used in NatConstraintError")
  override def defineTypes(expectedT: Type, foundT: Type): Unit =throw new IllegalStateException("this function should not be used in NatConstraintError")
  def defineTypesDep[K <: Kind](expectedT:rt.Type, foundTLeft:rt.Type, foundRight:K#T): Unit ={
    constraintTypes match {
      case Some(_) => throw new IllegalStateException("You are not supposed to call this function two times")
      case None => constraintTypes =
        Some(ExpectedAndFoundTLeftAndRightDep(expectedT, foundTLeft, foundRight))
    }
  }
  def getTypesDep[K <: Kind]():(rt.Type, rt.Type, K#T) = constraintTypes match {
    case None => throw new IllegalStateException("ConstraintTypes are not initialised yet")
    case Some(cT) => cT match {
      case ExpectedAndFoundTLeftAndRightDep(expectedT,foundTLeft, foundTRight) => foundTRight match {
        //case fR:K#T => (expectedT, foundTLeft, fR) //abstract type pattern K#T is unchecked since it is eliminated by erasure
        case fR => (expectedT, foundTLeft, fR.asInstanceOf[K#T])
      }
      case a => throw new IllegalStateException("wrong ConstraintType: "+ a)
    }
  }
  override def description(): String = {
    val (expectedT,foundLeft,foundRight) = getTypesDep()
    val foundRightStr: String = foundRight.toString
    "expected '" + expectedT + "' but found DepApp('"+foundLeft+ "','"+
      foundRightStr+"')"
  }
}

case class TypeAnnotationConstraintError(span: Option[Span]) extends ConstraintError(span){
  override def description(): String = {
    val annotatedT::foundT::Nil = getTypes()
    "annotated '" + annotatedT + "' but found '"+foundT+"'"
  }
}

case class TypeAssertionConstraintError(span: Option[Span]) extends ConstraintError(span){
  override def description(): String = {
    val annotatedT::freezeAnnT::Nil = getTypes()
    "annotated '" + annotatedT +"' but found freeze Type '"+freezeAnnT+"'"
  }
}

case class NatConstraintError(span: Option[Span]) extends ConstraintError(span){
  def getNats():List[rt.Nat] = constraintTypes match {
    case None => throw new IllegalStateException("ConstraintTypes are not initialised yet")
    case Some(cT)=> cT match {
      case ExpectedAndFoundN(eT, fT) => eT::fT::Nil
      case _ => throw new IllegalStateException("wrong ConstraintType")
    }
  }

  override def getTypes(): List[Type] = throw new IllegalStateException("this function should not be used in NatConstraintError")
  override def defineTypes(expectedT: Type, foundT: Type): Unit =throw new IllegalStateException("this function should not be used in NatConstraintError")
  def defineNats(expectedT:rt.Nat, foundT:rt.Nat): Unit ={
    constraintTypes match {
      case Some(_) => throw new IllegalStateException("You are not supposed to call this function two times")
      case None => constraintTypes = Some(ExpectedAndFoundN(expectedT, foundT))
    }
  }
  override def description(): String = {
    val expectedT::foundT::Nil = getNats()
    "expected '" + expectedT + "' but found '"+foundT+ "'"
  }
}

  case class BoolConstraintError(span: Option[Span]) extends ConstraintError(span){
    override def getTypes(): List[Type] = throw new IllegalStateException("this function should not be used in NatConstraintError")
    override def defineTypes(expectedT: Type, foundT: Type): Unit =throw new IllegalStateException("this function should not be used in NatConstraintError")
    def getBools():List[arithexpr.arithmetic.BoolExpr] = constraintTypes match {
      case None => throw new IllegalStateException("ConstraintTypes are not initialised yet")
      case Some(cT)=> cT match {
        case ExpectedAndFoundB(eT, fT) => eT::fT::Nil
        case _ => throw new IllegalStateException("wrong ConstraintType")
      }
    }
    def defineBools(expectedT:arithexpr.arithmetic.BoolExpr, foundT:arithexpr.arithmetic.BoolExpr): Unit ={
      constraintTypes match {
        case Some(_) => throw new IllegalStateException("You are not supposed to call this function two times")
        case None => constraintTypes = Some(ExpectedAndFoundB(expectedT, foundT))
      }
    }
    override def description(): String = {
      val expectedT::foundT::Nil = getBools()
      "expected '" + expectedT + "' but found '"+foundT+ "'"
    }
  }

case class AddrConstraintError(span: Option[Span]) extends ConstraintError(span){
  override def getTypes(): List[Type] = throw new IllegalStateException("this function should not be used in NatConstraintError")
  override def defineTypes(expected: Type, found: Type): Unit =throw new IllegalStateException("this function should not be used in NatConstraintError")
  def getAddrs():List[rt.AddressSpace] = constraintTypes match {
    case None => throw new IllegalStateException("ConstraintTypes are not initialised yet")
    case Some(cT)=> cT match {
      case ExpectedAndFoundA(eT, fT) => eT::fT::Nil
      case _ => throw new IllegalStateException("wrong ConstraintType")
    }
  }
  def defineAddrs(expected:rt.AddressSpace, found:rt.AddressSpace): Unit ={
    constraintTypes match {
      case Some(_) => throw new IllegalStateException("You are not supposed to call this function two times")
      case None => constraintTypes = Some(ExpectedAndFoundA(expected, found))
    }
  }
  override def description(): String = {
    val expected::found::Nil = getAddrs()
    "expected '" + expected + "' but found '"+found+ "'"
  }
}


case class NatToDataConstraintError(span: Option[Span]) extends ConstraintError(span){
  override def getTypes(): List[Type] = throw new IllegalStateException("this function should not be used in NatConstraintError")
  override def defineTypes(expected: Type, found: Type): Unit =throw new IllegalStateException("this function should not be used in NatConstraintError")
  def getNatToDatas():List[rt.NatToData] = constraintTypes match {
    case None => throw new IllegalStateException("ConstraintTypes are not initialised yet")
    case Some(cT)=> cT match {
      case ExpectedAndFoundNatToData(eT, fT) => eT::fT::Nil
      case _ => throw new IllegalStateException("wrong ConstraintType")
    }
  }
  def defineNatToDatas(expected:rt.NatToData, found:rt.NatToData): Unit ={
    constraintTypes match {
      case Some(_) => throw new IllegalStateException("You are not supposed to call this function two times")
      case None => constraintTypes = Some(ExpectedAndFoundNatToData(expected, found))
    }
  }
  override def description(): String = {
    val expected::found::Nil = getNatToDatas()
    "expected '" + expected + "' but found '"+found+ "'"
  }
}

case class NatCollectionConstraintError(span: Option[Span]) extends ConstraintError(span){
  override def getTypes(): List[Type] = throw new IllegalStateException("this function should not be used in NatConstraintError")
  override def defineTypes(expected: Type, found: Type): Unit =throw new IllegalStateException("this function should not be used in NatConstraintError")
  def getNatCollection():List[rt.NatCollection] = constraintTypes match {
    case None => throw new IllegalStateException("ConstraintTypes are not initialised yet")
    case Some(cT)=> cT match {
      case ExpectedAndFoundNatCollection(eT, fT) => eT::fT::Nil
      case _ => throw new IllegalStateException("wrong ConstraintType")
    }
  }
  def defineNatCollection(expected:rt.NatCollection, found:rt.NatCollection): Unit ={
    constraintTypes match {
      case Some(_) => throw new IllegalStateException("You are not supposed to call this function two times")
      case None => constraintTypes = Some(ExpectedAndFoundNatCollection(expected, found))
    }
  }
  override def description(): String = {
    val expected::found::Nil = getNatCollection()
    "expected '" + expected + "' but found '"+found+ "'"
  }
}

//case class OpaqueConstraintError(span: Span, foundT:rt.Type, annotatedT:rt.Type, freezeAnnT:rt.Type) extends ConstraintTypeError(span){
//  override def description(): String = "annotated '" + annotatedT + "' but found '"+
//    foundT+"' or found freeze Type '"+freezeAnnT+"'"
//}
