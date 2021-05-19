package parser.ErrorMessage

import parser._
import parser.parse.SpanPlaceholder
import rise.core.types.{Kind, Type}
import rise.core.{types => rt}

import java.nio.file.Paths
//__________________________________________AnsiColors
abstract sealed class AnsiColor_enum()
final case class BLACK() extends AnsiColor_enum{
  override def toString: String = "\u001B[30m"
}
final case class RED() extends AnsiColor_enum{
  override def toString: String = "\u001B[31m"
}
final case class GREEN() extends AnsiColor_enum{
  override def toString: String = "\u001B[32m"
}
final case class YELLOW() extends AnsiColor_enum{
  override def toString: String = "\u001B[33m"
}
final case class BLUE() extends AnsiColor_enum{
  override def toString: String = "\u001B[34m"
}
final case class MAGENTA() extends AnsiColor_enum{
  override def toString: String = "\u001B[35m"
}
final case class CYAN() extends AnsiColor_enum{
  override def toString: String = "\u001B[36m"
}
final case class WHITE() extends AnsiColor_enum{
  override def toString: String = "\u001B[37m"
}
final case class BLACK_B() extends AnsiColor_enum{
  override def toString: String = "\u001B[40m"
}
final case class RED_B() extends AnsiColor_enum{
  override def toString: String = "\u001B[41m"
}
final case class GREEN_B() extends AnsiColor_enum{
  override def toString: String = "\u001B[42m"
}
final case class YELLOW_B() extends AnsiColor_enum{
  override def toString: String = "\u001B[43m"
}
final case class BLUE_B() extends AnsiColor_enum{
  override def toString: String = "\u001B[44m"
}
final case class MAGENTA_B() extends AnsiColor_enum{
  override def toString: String = "\u001B[45m"
}
final case class CYAN_B() extends AnsiColor_enum{
  override def toString: String = "\u001B[46m"
}
final case class WHITE_B() extends AnsiColor_enum{
  override def toString: String = "\u001B[47m"
}
final case class RESET() extends AnsiColor_enum{
  override def toString: String = "\u001B[0m"
}
final case class BOLD() extends AnsiColor_enum{
  override def toString: String = "\u001B[1m"
}
final case class UNDERLINED() extends AnsiColor_enum{
  override def toString: String = "\u001B[4m"
}
final case class BLINK() extends AnsiColor_enum{
  override def toString: String = "\u001B[5m"
}
final case class REVERSED() extends AnsiColor_enum{
  override def toString: String = "\u001B[7m"
}
final case class INVISIBLE() extends AnsiColor_enum{
  override def toString: String = "\u001B[8m"
}
//__________________________________________Object ErrorMessage with useful methods
object ErrorMessage {
  def give_error(fileName:String, description_error:String, what_exp:String,help:Option[String],
                 name_of_error:String,start_column:Int, end_column:Int,
                 codeLines:Array[String], underl: Underline, important_column:Int,
                 important_row_Begin:Int, important_row_End:Int,
                 indentationBefore: Int=2, indentationAfter: Int=1,
                 indentColour: AnsiColor_enum=CYAN(), indentColour_ImportantColumn: AnsiColor_enum=BLUE(),
                ): String ={
    val c = important_column+1
    val r = important_row_Begin+1
    val filePath = Paths.get(fileName+":"+c+":"+r).toUri
    val res = indentColour_ImportantColumn.toString + name_of_error + Console.RESET+": "+
      description_error+" `"+codeLines(important_column).substring(important_row_Begin,important_row_End)+"`"+"\n"+
      give_char_n_times(important_column.toString.length, ' ', None)+
      indentColour_ImportantColumn.toString+"-->"+Console.RESET+" "+filePath+"\n"

    val help_message = help match {
      case Some(h) => giveIndent(None, BLUE(), indentationBefore,indentationAfter) + "help: " + h +"\n"
      case None => ""
    }
    res + give_code(what_exp,name_of_error,start_column,end_column,
      codeLines,underl,important_column,important_row_Begin,important_row_End,
      indentationBefore,indentationAfter,indentColour,indentColour_ImportantColumn
    ) + help_message
  }

  //Todo:if important column: BLUE, else CYAN as a colour for indentation
  private def give_code(what_exp:String, name_error:String,start_column:Int, end_column:Int,
                        codeLines:Array[String], underl: Underline, important_column:Int,
                        important_row_Begin:Int, important_row_End:Int,
                        indentationBefore: Int, indentationAfter: Int,
                        indentColour: AnsiColor_enum, indentColour_ImportantColumn: AnsiColor_enum): String ={
    val box_indent = name_error.length+": ".length-(indentationBefore+1)
    val outline = giveIndent(None,indentColour, indentationBefore, 0) +
      give_char_n_times(box_indent, '-', Some(indentColour))+"\n"

    var res =""
    for(i <- start_column until end_column){
      val (uL,iC) =if(i==important_column){
        (Some(underl),indentColour_ImportantColumn)
      }else{
        (None, indentColour)
      }
      res+=give_line_of_code(what_exp, codeLines, i, uL, important_row_Begin, important_row_End,
        indentationBefore, indentationAfter,iC)
    }
    outline+res+outline
  }

  //alternative we can underline with 'Console.UNDERLINED', which looks great too, but with this method
  //we have more freedom how it looks like and we can use it later for more variants of underlining
  abstract sealed class Underline()
  final case class Underline_With_Char(char:Char, colour: AnsiColor_enum) extends Underline

  private def giveIndent(col:Option[Int], colour: AnsiColor_enum,
                         indentationBefore:Int, indentationAfter:Int,
                         charToSeperate:Char='|', charToIndent:Char=' '):String={
    col match {
      case Some(column) => {
        val real_indentationBefore = indentationBefore-column.toString.size
        if(real_indentationBefore<=0) throw new IllegalArgumentException("ident was chosen too small")
        colour.toString + column + give_char_n_times(real_indentationBefore, charToIndent, None) +
          charToSeperate + give_char_n_times(indentationAfter, charToIndent,None) + Console.RESET
      }
      case None =>colour.toString + give_char_n_times(indentationBefore, charToIndent,None) +
        charToSeperate + give_char_n_times(indentationAfter, charToIndent,None) + Console.RESET
    }
  }

  /*
  give the complete column back
  importantRange is important to know what to higlight
  colour defines with which colour it should be highlighted
   */
  private def give_line_of_code(what_exp:String,
                                codeLines:Array[String], column:Int, underl: Option[Underline],
                                important_row_Begin:Int, important_row_End:Int,
                                indentationBefore: Int, indentationAfter: Int, indentColour: AnsiColor_enum): String={
    val row_begin = 0
    val row_end = codeLines(column).length
    val viewed = Range(Location(column, row_begin),Location(column, row_end))
    val important = Range(Location(column, important_row_Begin),Location(column, important_row_End))
    val which_case= viewed.whoIsSubset(important)

    val res = giveIndent(Some(column+1),indentColour, indentationBefore, indentationAfter)
    val res_of_code_underlining = underl match {
      case Some(Underline_With_Char(char, colour)) => {
        val (underlined_code, pos_start_underline,length_to_underline) = which_case match {
          case This_isSubset() =>
            throw new IllegalStateException("this should not happen, because the whole line is always a overset to an part of the line")
          case Other_isSubset() =>
            //println("other_IsSubset:'"+res+"'")
            val code = codeLines(column)
            (code.substring(0,important_row_Begin)+ colour+code.substring(important_row_Begin,important_row_End)+
              Console.RESET+code.substring(important_row_End), important_row_Begin,
              code.substring(important_row_Begin,important_row_End).length)
          case None_isSubset() =>
            (colour.toString + codeLines(column) + Console.RESET, 0, codeLines(column).length)
        }
        val beginNextLineWithoutNumber = giveIndent(None,indentColour, indentationBefore, indentationAfter)
        underlined_code + "\n" + beginNextLineWithoutNumber +
          give_char_n_times(pos_start_underline, ' ',None) +
          colour + give_char_n_times(length_to_underline, char,None)+" " + what_exp + Console.RESET
      }
      case None => codeLines(column)
    }
    res + res_of_code_underlining + "\n"
  }


  def give_char_n_times(n:Int, c:Char, col:Option[AnsiColor_enum]): String={
    val arr = Array.fill(n)(c)
    val res = String.valueOf(arr)
    col match {
      case Some(colour) => colour.toString + res + Console.RESET
      case None => res
    }
  }
}
//__________________________________________ParserError

/*
https://stackoverflow.com/questions/38243530/custom-exception-in-scala is reference
 */
trait ParserError { self: Throwable =>
  val span: Span
  val description_error:String
  val what_exp:String
  val help:Option[String]
  val name_of_error:String

  val begin = span.range.begin
  val end = span.range.end

  /*
  problem: we have multiple lines where we look on, so we have to define for each Exeption individually
  what is important
   */
  def getImportantPos():(Int,Int,Int) = throw new IllegalStateException("this has to be overwritten")
  /*
  if -1 returned then failure
   */
  def getPos():(Int,Int,Int,Int,Int)={
    var (start_column, end_column, important_column,
    important_row_Begin, important_row_End):(Option[Int],
      Option[Int], Option[Int],Option[Int],Option[Int])=(None,None,None,None,None)

    start_column = Some(begin.column)
    end_column = Some(end.column)

    if(begin.column==end.column){
      important_column = Some(begin.column)
      important_row_Begin = Some(begin.row)
      important_row_End = Some(end.row)
    }else{
      val (iC, iRB, iRE) =getImportantPos()
      important_column=Some(iC)
      important_row_Begin=Some(iRB)
      important_row_End=Some(iRE)
    }


    (start_column.get,end_column.get,important_column.get,important_row_Begin.get,important_row_End.get)
  }

  override def toString: String = {
//    val (middle,before,after, loc) = (
//            span.returnMessage(),
//            span.file.sourceLines(begin.column).substring(0, begin.row),
//            span.file.sourceLines(end.column).substring(end.row),
//            span.toString    //exact location of error, related code of error, short description of error
//          )

    val (start_column, end_column, important_column,
    important_row_Begin, important_row_End)= getPos()

    val underl = ErrorMessage.Underline_With_Char('^', RED())

    val error = ErrorMessage.give_error(span.file.fileName, description_error,what_exp,help,name_of_error,
          start_column,end_column,span.file.sourceLines, underl, important_column, important_row_Begin,
          important_row_End)
    error
  }
}


//abstract sealed class ErrorMessage(span:Option[Span]) {
//  val sp = span
//  val s = span.getOrElse(SpanPlaceholder)
//  //def getErrorStackTraceElem():StackTraceElement=Thread.currentThread.getStackTrace().tail.tail.tail.tail.head
//  //val errorStackTraceElem = getErrorStackTraceElem()
//  val fileReader = s.file
//  val begin = s.range.begin
//  val end = s.range.end
//
//
//}

//______________________________________________________________________________________________________
//Lexer

object PreAndErrorToken{
  val underscoreNotAllowed = "Underscore is not allowed at the beginning"
  val unknownSymbol = "unknown symbol"
  val identifierExpectForm = Some("Identifier expect form '"+Identifier.regex+"'")
  val numberExpectForm = Some("Number expect form '"+Identifier.regex+"'")
}

abstract sealed class PreAndErrorToken(override val span: Span) extends Error with ParserError{
  override val name_of_error: String = "LexerError"
}

final case class EndOfLine(override val span:Span) extends PreAndErrorToken(span){
  require(begin == end, "begin is unequal to end")
  override val description_error = "End of Line"
  override val what_exp = "no end of Line here expected"
  override val help = Some("this error is not your fault")
}
final case class EndOfFile(override val span:Span) extends PreAndErrorToken(span){
  require(begin == end, "begin is unequal to end")
  override val description_error: String = "End of File"
  override val what_exp: String = "no end of File here expected"
  override val help: Option[String] = Some("you haven't end the expr")
}
final case class UnknownSymbol(c:Char, override val span:Span) extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "The character '" + c + "' is unknown"
  override val what_exp: String = PreAndErrorToken.unknownSymbol
  val knownChars=RecognizeLexeme.knownCharacters.toString()
  override val help: Option[String] = Some("allowed characters '"+knownChars+"'")
}
final case class OnlyOneEqualSign(override val span:Span) extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String =  "Only one EqualSign"
  override val what_exp: String = "two equal Signs are expected"
  override val help: Option[String] = Some("use '==' for the Operator Equal")
}
final case class IdentifierWithNotAllowedSymbol(unknownSymbol:Char, str:String, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String =  "The Identifier '" + str + "' has an unknown Symbol '" + unknownSymbol + "'"
  override val what_exp: String = PreAndErrorToken.unknownSymbol
  override val help: Option[String] = PreAndErrorToken.identifierExpectForm
}
final case class IdentifierBeginsWithUnderscore(override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String =  "Identifier with an Underscore"
  override val what_exp: String = PreAndErrorToken.underscoreNotAllowed
  override val help: Option[String] = PreAndErrorToken.identifierExpectForm
}
final case class IdentifierBeginsWithDigits(str:String, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String =  "The Identifier '"+ str+ "' begins with an Digits/a Number"
  override val what_exp: String = PreAndErrorToken.underscoreNotAllowed
  override val help: Option[String] = PreAndErrorToken.identifierExpectForm
}
final case class NumberWithUnknownSymbol(unknownSymbol: Char, str:String, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String =  "The Number '" + str + "' has an unknown Symbol '" + unknownSymbol + "'"
  override val what_exp: String = PreAndErrorToken.unknownSymbol
  override val help: Option[String] = PreAndErrorToken.numberExpectForm
}
final case class F32DeclaredAsI8(number:Float, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String =  "The number '" + number + "' is an F32 Number but it is declared as I8"
  override val what_exp: String = "ends with I8"
  override val help: Option[String] = Some("delete I8 at the end")
}
final case class F32DeclaredAsI32(number:Float, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String =  "The number '" + number + "' is an F32 Number but it is declared as I32"
  override val what_exp: String = "ends with I32"
  override val help: Option[String] = Some("delete I32 at the end")
}
final case class IdentifierBeginsWithAF32Number(str:String, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String =  "The Identifier '"+ str+ "' begins with a F32 Number"
  override val what_exp: String = "begins with Number"
  override val help: Option[String] = PreAndErrorToken.identifierExpectForm
}
final case class NumberWithUnderscore(str:String, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String =  "The Number '"+ str+ "' has an underscore in it"
  override val what_exp: String = PreAndErrorToken.underscoreNotAllowed
  override val help: Option[String] = PreAndErrorToken.numberExpectForm
}
final case class NotExpectedToken(expectedToken:String, givenToken:String, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "It is an '"+ expectedToken +"' expected. The Lexeme '" + givenToken +
    "' is not an '"+ expectedToken+ "'!"
  override val what_exp: String = "exptected was '"+expectedToken+"'"
  override val help: Option[String] = Some("Use the Token '"+expectedToken+"'")
}

final case class NotExpectedTwoBackslash(expectedToken:String, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "It is an '"+ expectedToken +"' expected. But we have here two '\\'!"
  override val what_exp: String = "exptected was '"+expectedToken+"'"
  override val help: Option[String] = Some("Use the Token '"+expectedToken+"'")
}

final case class ToShortToBeThisToken(expectedLength:Int, token:String, override val span:Span)
  extends PreAndErrorToken(span){
  require(expectedLength >0, "expectedLength is less or equal to zero")
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "the given length is less than "+ expectedLength +" for "+ token +"!"
  override val what_exp: String = "under " +expectedLength + " length"
  override val help: Option[String] = None
}
final case class NOTanBinOperator(symbol: String, override val span:Span)
  extends PreAndErrorToken(span) {
  require(begin == end, "begin is unequal to end")
  override val description_error: String = "The Symbol '" + symbol + "' is not an Binary Operator"
  override val what_exp: String = "no Binary Operator"
  override val help: Option[String] = Some("allowed binaryOperators '"+RecognizeLexeme.binarySymbol.toString()+"'")
}
final case class NOTanUnOperator(symbol: String, override val span:Span)
  extends PreAndErrorToken(span) {
  require(begin == end, "begin is unequal to end")
  override val description_error: String = "The Symbol '" + symbol + "' is not an Unary Operator"
  override val what_exp: String = "no Unary Operator"
  override val help: Option[String] = Some("allowed unary Operators '"+RecognizeLexeme.unarySymbol.toString()+"'")
}

final case class UnknownType(str: String, override val span:Span) extends PreAndErrorToken(span) {
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "The Type '" + str + "' is not an accepted Type in RISE"
  override val what_exp: String = "not accepted type"
  override val help: Option[String] = Some("allowed scalarTypes are '"+RecognizeLexeme.scalarTypes.toString()+"'")
}

final case class UnknownKind(str: String, override val span:Span) extends PreAndErrorToken(span) {
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "The Kind '" + str + "' is not an accepted Kind in RISE"
  override val what_exp: String = "not accepted Kind"
  override val help: Option[String] = Some("allowed Kinds are '"+RecognizeLexeme.kinds.toString()+"'")
}

abstract sealed class ThisTokenShouldntBeHere(token: Token, override val span:Span)
  extends PreAndErrorToken(span) {
  val begin_token = token.s.range.begin
  val end_token = token.s.range.end
  require(begin.column == end.column, "not in one column")
  require(begin_token.column == end_token.column, "not in one column")
  override val description_error: String = "The Token '" + token.toString + "' was not here expected"
  override val what_exp: String = "wrong place"
  override val help: Option[String] = None
}

final case class ThisTokenShouldntBeHereExpectedArrowOrDots(token: Token, override val span:Span)
  extends ThisTokenShouldntBeHere(token, span){
  require(begin.column == end.column, "not in one column")
  override val what_exp: String = "expect '->' or ':'"
  override val help: Option[String] = Some("Lambda has form '\\Ident:Type->...' or '\\Ident->...'")
}

final case class TypeIdentifierExpectedNotIdentifier(name: String, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "It is an TypeIdentifier expected. '" + name + "' is an Identifier"
  override val what_exp: String = "expected TypeIdent"
  override val help: Option[String] = Some("TypeIdentifier expects form '"+TypeIdentifier.regex+"'")
}
final case class IdentifierExpectedNotTypeIdentifier(name: String, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "It is an Identifier expected. '" + name + "' is an TypeIdentifier"
  override val what_exp: String = "expected Identifier"
  override val help: Option[String] = PreAndErrorToken.identifierExpectForm
}


//__________________________________________________________________________________________________________
//Parser


abstract sealed class PreAndErrorSynElems(override val span: Span, whatToParse: String)extends Error with ParserError{
  override val name_of_error: String = "ParserError"
}

final case class SynListIsEmpty(override val span: Span, whatToParse: String)
  extends PreAndErrorSynElems(span, whatToParse){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "SyntayElementslist is empty"
  override val what_exp: String = "List is empty"
  override val help: Option[String] = None
}

final case class TokListIsEmpty(override val span: Span, whatToParse: String)
  extends PreAndErrorSynElems(span, whatToParse){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "Tokenlist is empty"
  override val what_exp: String = "List is empty"
  override val help: Option[String] = None
}

/*
has to have at least one elem else ListIsEmpty Exeption should be used
 */
final case class TokListTooSmall(list: List[Token], minimumLen: Int, whatToParse: String)
  extends PreAndErrorSynElems(list.head.s, whatToParse){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "Tokenlist size is "+ list.size+ " but min. Lenght is "+ minimumLen
  override val what_exp: String = "List is too small"
  override val help: Option[String] = None
}

final case class NotCorrectToken(token: Token, expectedToken:String, whatToParse: String)
  extends PreAndErrorSynElems(token.s, whatToParse){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "'"+token+"' is not '"+ expectedToken+"'"
  override val what_exp: String = "expected is '"+expectedToken+"'"
  override val help: Option[String] = None
}

final case class NoKindWithThisName(kind: TypeIdentifier, whatToParse: String)
  extends PreAndErrorSynElems(kind.span, whatToParse){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "TypeIdent '"+kind.name+"' is not declared"
  override val what_exp: String = "undeclared KindName"
  override val help: Option[String] = None
}

final case class NotAcceptedScalarType(override val span:Span, notAcceptedType: ConcreteType, whatToParse: String)
  extends  PreAndErrorSynElems(span, whatToParse){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "'"+notAcceptedType+"' is not an ScalarType"
  override val what_exp: String = "not accepted ScalarType"
  override val help: Option[String] = Some("accepted ScalarTypes are '"+RecognizeLexeme.scalarTypes+"'")
}

final case class NotCorrectKind(override val span:Span, kind: ConcreteKind, whatToParse: String)
  extends PreAndErrorSynElems(span, whatToParse){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "'"+kind+"' is not the expected Kind"
  override val what_exp: String = "not correct Kind"
  override val help: Option[String] = None
}

final case class NotCorrectSynElem(wrongSynElem: parser.parse.SyntaxElement,expectedSynElem: String, whatToParse: String)
  extends PreAndErrorSynElems(Span.getSpanSynElem(wrongSynElem).get, whatToParse){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "'"+wrongSynElem+"' is not the expected SynElem '" + expectedSynElem +"'"
  override val what_exp: String = "not accepted SynElem"
  override val help: Option[String] = None
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
        case Right(newE)=> if(e.span.isAfter(newE.span)){
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
      this.errorList(this.deepestError).
        getOrElse(throw new IllegalStateException("Rules should not be the deepest elem")).toString
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
          s = s +retSpace(space)+space+";"+i+".th:\n"+ e.toString + "\n"
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


trait ConstraintError { self: Throwable =>
  val span: Option[Span]
  def description_error():String
  val what_exp:String
  val help:Option[String]
  val name_of_error:String

  //Todo: find better solution than SpanPlaceholder
  var constraintTypes: Option[ConstraintInputTypes] = None
  def getTypes():List[rt.Type]
  def defineTypes(expectedT:rt.Type, foundT:rt.Type): Unit

  /*
  problem: we have multiple lines where we look on, so we have to define for each Exeption individually
  what is important
   */
  def getImportantPos():(Int,Int,Int) = throw new IllegalStateException("this has to be overwritten")
  /*
  if None returned then failure
   */
  def getPos():Option[(Int,Int,Int,Int,Int)]={
    var (start_column, end_column, important_column,
    important_row_Begin, important_row_End):(Option[Int],
      Option[Int], Option[Int],Option[Int],Option[Int])=(None,None,None,None,None)

    val sp = span match {
      case Some(value) => value
      case None => return None
    }
    val (begin,end)=(sp.range.begin,sp.range.end)
    start_column = Some(begin.column)
    end_column = Some(end.column)

    if(begin.column==end.column){
      important_column = Some(begin.column)
      important_row_Begin = Some(begin.row)
      important_row_End = Some(end.row)
    }else{
      val (iC, iRB, iRE) =getImportantPos()
      important_column=Some(iC)
      important_row_Begin=Some(iRB)
      important_row_End=Some(iRE)
    }


    Some((start_column.get,end_column.get,important_column.get,important_row_Begin.get,important_row_End.get))
  }

  override def toString: String = {
    val (start_column, end_column, important_column,
    important_row_Begin, important_row_End)= getPos() match {
      case Some(value) => value
      case None => ???
    }
    val sp = span match {
      case Some(value) => value
      case None => ???
    }

    val underl = ErrorMessage.Underline_With_Char('^', RED())

    val error = ErrorMessage.give_error(sp.file.fileName, description_error(),what_exp,help,name_of_error,
      start_column,end_column,sp.file.sourceLines, underl, important_column, important_row_Begin,
      important_row_End)
    error
  }
}

abstract sealed class AppLikeConstraintError(override val span: Option[Span]) extends TypeConstraintError(span){
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

abstract sealed class TypeConstraintError(override val span: Option[Span]) extends Error with ConstraintError{
  override def defineTypes(expectedT:rt.Type, foundT:rt.Type): Unit ={
    constraintTypes match {
      case Some(_) => throw new IllegalStateException("You are not supposed to call this function two times")
      case None => constraintTypes = Some(ExpectedAndFoundT(expectedT, foundT))
    }
  }
  override def getTypes():List[rt.Type] = constraintTypes match {
    case None => throw new IllegalStateException("ConstraintTypes are not initialised yet")
    case Some(cT)=> cT match {
      case ExpectedAndFoundT(eT, fT) => eT::fT::Nil
      case _ => throw new IllegalStateException("wrong ConstraintType")
    }
  }

  override def description_error(): String = {
    val expectedT::foundT::Nil = getTypes()
    "expected '" + expectedT + "' but found '"+foundT+ "'"
  }

  override val name_of_error: String = "TypeConstraintError"
}

case class EqualityTypeConstraintError(override val span: Option[Span]) extends TypeConstraintError(span){
  override val what_exp: String = "here problems with TypeConstraint"
  override val help: Option[String] = None
}

case class IdentConstraintError(override val span: Option[Span]) extends TypeConstraintError(span){
  override val what_exp: String = "here problems with IdentConstraint"
  override val help: Option[String] = None
}

case class LambdaConstraintError(override val span: Option[Span]) extends TypeConstraintError(span){
  override val what_exp: String = "here problems with LambdaConstraint"
  override val help: Option[String] = None
}

case class AppConstraintError(override val span: Option[Span]) extends AppLikeConstraintError(span){
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
  override def description_error(): String = {
    val expectedT::foundTLeft::foundTRight::Nil = getTypes()
    "expected '" + expectedT + "' but found App('"+foundTLeft+ "','"+
      foundTRight+"')"
  }
  override val what_exp: String = "here problems with AppConstraint"
  override val help: Option[String] = None
}

case class DepLambdaConstraintError(override val span: Option[Span]) extends TypeConstraintError(span){
  override val what_exp: String = "here problems with DepLambdaConstraint"
  override val help: Option[String] = None
}

case class DepAppConstraintError(override val span: Option[Span]) extends AppLikeConstraintError(span){
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
  override def description_error(): String = {
    val (expectedT,foundLeft,foundRight) = getTypesDep()
    val foundRightStr: String = foundRight.toString
    "expected '" + expectedT + "' but found DepApp('"+foundLeft+ "','"+
      foundRightStr+"')"
  }
  override val what_exp: String = "here problems with DepAppConstraint"
  override val help: Option[String] = None
}

case class TypeAnnotationConstraintError(override val span: Option[Span]) extends TypeConstraintError(span){
  override def description_error(): String = {
    val annotatedT::foundT::Nil = getTypes()
    "annotated '" + annotatedT + "' but found '"+foundT+"'"
  }
  override val what_exp: String = "here problems with TypeAnnotationConstraint"
  override val help: Option[String] = None
}

case class TypeAssertionConstraintError(override val span: Option[Span]) extends TypeConstraintError(span){
  override def description_error(): String = {
    val annotatedT::freezeAnnT::Nil = getTypes()
    "annotated '" + annotatedT +"' but found freeze Type '"+freezeAnnT+"'"
  }
  override val what_exp: String = "here problems with TypeAssertionConstraint"
  override val help: Option[String] = None
}

case class NatConstraintError(override val span: Option[Span]) extends Error with ConstraintError{
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
  override def description_error(): String = {
    val expectedT::foundT::Nil = getNats()
    "expected '" + expectedT + "' but found '"+foundT+ "'"
  }

  override val name_of_error: String = "NatConstraintError"
  override val what_exp: String = "here problems with NatConstraint"
  override val help: Option[String] = None
}

  case class BoolConstraintError(override val span: Option[Span]) extends Error with ConstraintError{
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
    override def description_error(): String = {
      val expectedT::foundT::Nil = getBools()
      "expected '" + expectedT + "' but found '"+foundT+ "'"
    }
    override val name_of_error: String = "BoolConstraintError"
    override val what_exp: String = "here problems with BoolConstraint"
    override val help: Option[String] = None
  }

case class AddrConstraintError(override val span: Option[Span]) extends Error with ConstraintError{
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
  override def description_error(): String = {
    val expected::found::Nil = getAddrs()
    "expected '" + expected + "' but found '"+found+ "'"
  }
  override val name_of_error: String = "AddrConstraintError"
  override val what_exp: String = "here problems with AddrConstraint"
  override val help: Option[String] = None
}


case class NatToDataConstraintError(override val span: Option[Span]) extends Error with ConstraintError{
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
  override def description_error(): String = {
    val expected::found::Nil = getNatToDatas()
    "expected '" + expected + "' but found '"+found+ "'"
  }
  override val name_of_error: String = "NatToDataConstraintError"
  override val what_exp: String = "here problems with NatToDataConstraint"
  override val help: Option[String] = None
}

case class NatCollectionConstraintError(override val span: Option[Span]) extends Error with ConstraintError{
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
  override def description_error(): String = {
    val expected::found::Nil = getNatCollection()
    "expected '" + expected + "' but found '"+found+ "'"
  }
  override val name_of_error: String = "NatCollectionError"
  override val what_exp: String = "here problems with NatCollection"
  override val help: Option[String] = None
}

//case class OpaqueConstraintError(span: Span, foundT:rt.Type, annotatedT:rt.Type, freezeAnnT:rt.Type) extends ConstraintTypeError(span){
//  override def description(): String = "annotated '" + annotatedT + "' but found '"+
//    foundT+"' or found freeze Type '"+freezeAnnT+"'"
//}
