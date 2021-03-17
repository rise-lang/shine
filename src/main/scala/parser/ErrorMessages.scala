package parser

/*
https://stackoverflow.com/questions/38243530/custom-exception-in-scala is reference
 */
trait ParserException { self: Throwable =>
  def span: Span
}

abstract sealed class ErrorMessages(span:Span) {
  val fileReader = span.file
  val begin = span.begin
  val end = span.end
  private def underline(str:String):String = {
    var s:String = ""
    for( a <- str){ s = s + a + "\u0332" }
    s
  }

  private def importantPart(str: String): String = {
    val s: String = underline(str)
    s
  }

  def descriptionError():String=
  throw new Exception("descriptionError method must be overridden")

  def throwException():Unit ={
    val m = span.returnMessage()
    val important = importantPart(m)
    val before = span.file.sourceLines(begin.column).substring(0,begin.row)
    val after = span.file.sourceLines(end.column).substring(end.row)

    //exact location of error, related code of error, short description of error
    val loc =  span.toString
    val relatedCode = before + important + after
    val desc = descriptionError()

    val message = desc + " : " + loc + " : "

    val sp = span
    throw new Exception(message) with ParserException {
      override def span: Span = sp
    }
  }
}

abstract sealed class PreAndErrorToken(span: Span) extends ErrorMessages(span)

final case class EndOfLine(span:Span) extends PreAndErrorToken(span){
  require(span.begin == span.end, "span.begin is unequal to span.end")
  override def descriptionError() = "End of Line"
}
final case class EndOfFile(span:Span) extends PreAndErrorToken(span){
  require(span.begin == span.end, "span.begin is unequal to span.end")
  override def descriptionError() = "End of File"
}
final case class UnknownSymbol(c:Char, span:Span) extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def descriptionError() = "The character '" + c + "' is unknown"
}
final case class OnlyOneEqualSign(span:Span) extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def descriptionError() = "Only one EqualSign"
}
final case class NegSign(span:Span) extends PreAndErrorToken(span){
  require(span.begin == span.end, "span.begin is unequal to span.end")
  override def descriptionError() = "A Negative Sign"
}
final case class IdentifierWithNotAllowedSymbol(unknownSymbol:Char, str:String, span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def descriptionError() = "The Identifier '" + str + "' has an unknown Symbol '" + unknownSymbol + "'"
}
final case class IdentifierBeginsWithUnderscore(span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def descriptionError() = "Identifier with an Underscore"
}
final case class IdentifierBeginsWithDigits(str:String, span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def descriptionError() = "The Identifier '"+ str+ "' begins with an Digits/a Number"
}
final case class NumberWithUnknownSymbol(unknownSymbol: Char, str:String, span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def descriptionError() = "The Number '" + str + "' has an unknown Symbol '" + unknownSymbol + "'"
}
final case class F32DeclaredAsI8(number:Float, span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def descriptionError() = "The number '" + number + "' is an F32 Number but it is declared as I8"
}
final case class F32DeclaredAsI32(number:Float, span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def descriptionError() = "The number '" + number + "' is an F32 Number but it is declared as I32"
}
final case class IdentifierBeginsWithAF32Number(str:String, span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def descriptionError() = "The Identifier '"+ str+ "' begins with a F32 Number"
}
final case class NumberWithUnderscore(str:String, span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def descriptionError() = "The Number '"+ str+ "' has an underscore in it"
}
final case class NotExpectedToken(expectedToken:String, givenToken:String, span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def descriptionError() = "It is an '"+ expectedToken +"' expected. The Lexeme '" + givenToken +
    "' is not an '"+ expectedToken+ "'!"
}

final case class NotExpectedTwoBackslash(expectedToken:String, span:Span)
  extends PreAndErrorToken(span){
  require(span.begin.column == span.end.column, "not in one column")
  override def descriptionError() = "It is an '"+ expectedToken +"' expected. But we have here two '\\'!"
}

final case class ToShortToBeThisToken(expectedLength:Int, token:String, span:Span)
  extends PreAndErrorToken(span){
  require(expectedLength >0, "expectedLength is less or equal to zero")
  require(span.begin.column == span.end.column, "not in one column")
  override def descriptionError() = "the given length is less than "+ expectedLength +" for "+ token +"!"
}
final case class NOTanBinOperator(symbol: String, span:Span)
  extends PreAndErrorToken(span) {
  require(span.begin == span.end, "span.begin is unequal to span.end")
  override def descriptionError() = "The Symbol '" + symbol + "' is not an Operator"
}
final case class NOTanUnOperator(symbol: String, span:Span)
  extends PreAndErrorToken(span) {
  require(span.begin == span.end, "span.begin is unequal to span.end")
  override def descriptionError() = "The Symbol '" + symbol + "' is not an Operator"
}

final case class UnknownType(str: String, span:Span) extends PreAndErrorToken(span) {
  require(span.begin.column == span.end.column, "not in one column")
  override def descriptionError() = "The Type '" + str + "' is not an accepted Type in RISE"
}

final case class UnknownKind(str: String, span:Span) extends PreAndErrorToken(span) {
  require(span.begin.column == span.end.column, "not in one column")
  override def descriptionError() = "The Kind '" + str + "' is not an accepted Kind in RISE"
}

abstract sealed class ThisTokenShouldntBeHere(token: Token, span:Span)
  extends PreAndErrorToken(span) {
  require(token.s.begin.column == token.s.end.column, "not in one column")
  override def descriptionError() = "The Token '" + token.toString + "' was not here expected"
}

final case class ThisTokenShouldntBeHereExpectedArrowOrDots(token: Token, span:Span)
  extends ThisTokenShouldntBeHere(token, span)


final case class LeftBraceMissing(span:Span) extends PreAndErrorToken(span){
  override def descriptionError() = "Left Brace is missing!"
}
final case class RightBraceMissing(span:Span) extends PreAndErrorToken(span){
  override def descriptionError() = "Right Brace is missing!"
}
final case class TypeIdentifierExpectedNotIdentifier(name: String, span:Span)
  extends PreAndErrorToken(span){
  override def descriptionError() = "It is an TypeIdentifier expected. '" + name + "' is an Identifier"
}
final case class IdentifierExpectedNotTypeIdentifier(name: String, span:Span)
  extends PreAndErrorToken(span){
  override def descriptionError() = "It is an Identifier expected. '" + name + "' is an TypeIdentifier"
}