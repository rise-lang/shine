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
