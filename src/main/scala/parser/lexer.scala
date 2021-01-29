package parser


import OpType.{BinOpType, UnaryOpType}


//alles muss in ein Try gemappt werden
//Try{ file= openFile(); parse(file);}catch{...}finally{file.close}



abstract sealed class PreAndErrorToken(span:Span, fileReader: FileReader){
  private def underline(str:String):String = {
    var s:String = ""
    for( a <- str){ s = s + a + "\u0332" }
    s
  }

  private def importantPart(str: String): String = {
    val s: String = underline(str)
    s
  }

  def throwException():Unit ={
    val currentColumn:String = fileReader.sourceLines(span.begin.column)
    val important:String = importantPart(currentColumn.substring(span.begin.row, span.end.row))
    val codeLine:String = if (span.begin.row-1 > 0){
      currentColumn.substring(0,span.begin.row) + important + currentColumn.substring(span.end.row, currentColumn.length)
    }else{
      important + currentColumn.substring(span.end.row, currentColumn.length)
    }
    val message:String = "ErrorToken: "+ this.toString +" at " + span.toString + "\n" + codeLine
    throw new Exception(message) //Todo:: Maybe RuntimeException is here better?
  }
}
final case class EndOfLine(span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader){
  require(span.begin == span.end, "span.begin is unequal to span.end")
  override def toString = "End of Line at " + span.toString
}
final case class EndOfFile(span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader){
  require(span.begin == span.end, "span.begin is unequal to span.end")
  override def toString = "End of File at " + span.toString
}
final case class UnknownSymbol(c:Char, span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader){
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "The character '" + c + "' is unknown at " + span.toString
}
final case class OnlyOneEqualSign(span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader){
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "Only one EqualSign at " + span.toString
}
final case class NegSign(span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader){
  require(span.begin == span.end, "span.begin is unequal to span.end")
  override def toString = "A Negative Sign at " + span.toString
}
final case class IdentifierWithNotAllowedSymbol(unknownSymbol:Char, str:String, span:Span, fileReader:FileReader)
  extends PreAndErrorToken(span, fileReader){
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "The Identifier '" + str + "' has an unknown Symbol '" + unknownSymbol + "' at "+
    span.toString()
}
final case class IdentifierBeginsWithUnderscore(span:Span, fileReader:FileReader)
  extends PreAndErrorToken(span, fileReader){
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "Here begins a Identifier with an Underscore at " + span.toString
}
final case class IdentifierBeginsWithDigits(str:String, span:Span, fileReader:FileReader)
  extends PreAndErrorToken(span, fileReader){
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "The Identifier '"+ str+ "' begins with an Digits/a Number at " + span.toString
}
final case class NumberWithUnknownSymbol(unknownSymbol: Char, str:String, span:Span, fileReader:FileReader)
  extends PreAndErrorToken(span, fileReader){
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "The Number '" + str + "' has an unknown Symbol '" + unknownSymbol + "' at "+ span.toString()
}
final case class F32DeclaredAsI8(number:Float, span:Span, fileReader:FileReader)
  extends PreAndErrorToken(span, fileReader){
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "The number '" + number + "' is an F32 Number but it is declared as I8 at "+ span.toString()
}
final case class F32DeclaredAsI32(number:Float, span:Span, fileReader:FileReader)
  extends PreAndErrorToken(span, fileReader){
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "The number '" + number + "' is an F32 Number but it is declared as I32 at "+ span.toString()
}
final case class IdentifierBeginsWithAF32Number(str:String, span:Span, fileReader:FileReader)
  extends PreAndErrorToken(span, fileReader){
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "The Identifier '"+ str+ "' begins with a F32 Number at " + span.toString
}
final case class NumberWithUnderscore(str:String, span:Span, fileReader:FileReader)
  extends PreAndErrorToken(span, fileReader){
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "The Number '"+ str+ "' has an underscore in it at " + span.toString
}
  final case class NotExpectedToken(expectedToken:String, givenToken:String, span:Span, fileReader:FileReader)
    extends PreAndErrorToken(span, fileReader){
    require(span.begin.column == span.end.column, "not in one column")
    override def toString = "It is an '"+ expectedToken +"' expected. The Lexeme '" + givenToken +
      "' is not an '"+ expectedToken+ "'!"
  }

final case class NotExpectedTwoBackslash(expectedToken:String, span:Span, fileReader:FileReader)
  extends PreAndErrorToken(span, fileReader){
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "It is an '"+ expectedToken +"' expected. But we have here two '\\'!"
}

  final case class ToShortToBeThisToken(expectedLength:Int, token:String, span:Span, fileReader:FileReader)
    extends PreAndErrorToken(span, fileReader){
    require(expectedLength >0, "expectedLength is less or equal to zero")
    require(span.begin.column == span.end.column, "not in one column")
    override def toString = "the given length is less than "+ expectedLength +" for "+ token +"!"
  }
final case class NOTanBinOperator(symbol: String, span:Span, fileReader:FileReader)
  extends PreAndErrorToken(span, fileReader) {
  require(span.begin == span.end, "span.begin is unequal to span.end")
  override def toString = "The Symbol '" + symbol + "' is not an Operator"
}
final case class NOTanUnOperator(symbol: String, span:Span, fileReader:FileReader)
  extends PreAndErrorToken(span, fileReader) {
  require(span.begin == span.end, "span.begin is unequal to span.end")
  override def toString = "The Symbol '" + symbol + "' is not an Operator"
}

final case class UnknownType(str: String, span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader) {
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "The Type '" + str + "' is not an accepted Type in RISE"
}

final case class UnknownKind(str: String, span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader) {
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "The Kind '" + str + "' is not an accepted Kind in RISE"
}

abstract sealed class ThisTokenShouldntBeHere(token: Token, span:Span, fileReader:FileReader)
  extends PreAndErrorToken(span, fileReader) {
  require(token.s.begin.column == token.s.end.column, "not in one column")
  override def toString = "The Token '" + token.toString + "' was not here expected"
}

final case class ThisTokenShouldntBeHereExpectedArrowOrDots(token: Token, span:Span, fileReader:FileReader)
  extends ThisTokenShouldntBeHere(token, span, fileReader)


final case class LeftBraceMissing(span:Span, fileReader: FileReader) extends PreAndErrorToken(span, fileReader){
  override def toString = "Left Brace is missing!"
}
final case class RightBraceMissing(span:Span, fileReader: FileReader) extends PreAndErrorToken(span, fileReader){
  override def toString = "Right Brace is missing!"
}
final case class TypeIdentifierExpectedNotIdentifier(name: String, span:Span, fileReader: FileReader)
  extends PreAndErrorToken(span, fileReader){
  override def toString = "It is an TypeIdentifier expected. '" + name + "' is an Identifier"
}
final case class IdentifierExpectedNotTypeIdentifier(name: String, span:Span, fileReader: FileReader)
  extends PreAndErrorToken(span, fileReader){
  override def toString = "It is an Identifier expected. '" + name + "' is an TypeIdentifier"
}
/*
this recognizes the Lexeme in the File which represents the right Token
 */
case class RecognizeLexeme(fileReader: FileReader){
  val tokens:List[Token] = lexer()
  type TokenAndPos = (List[Token],Int,Int)
  type TokenList = List[Either[Token,PreAndErrorToken]]

  private def lexer(): List[Token] = {
    val list:List[Token] = lexNamedExprOrTypAnnotatedIdent(0,0, Nil)
    val newL = list.reverse
    println("Lexer finished: " + newL)
    newL
  }

  private def lexNamedExprOrTypAnnotatedIdent(oldColumn:Int, oldRow:Int, l:List[Token]):List[Token] =  {
    val arr: Array[String]= fileReader.sourceLines
    var row = oldRow
    var column = oldColumn
    require(row>=0, "row is not allowed to be negative")
    require(column>=0, "column is not allowed to be negative")

    val list= l

    isEnd(fileReader, column, row, arr) match {
      case Left((c, r)) => {
        column = c
        row = r
        println("lexing begins at : ( "+ column +" , " + row + " )" )
      }
      case Right(EndOfFile(_, _)) => throw new RuntimeException("Here occoured a EndOfFile Exeption," +
        " but this should not be able to happen")
      case Right(EndOfLine(span, _)) => throw new RuntimeException("At position ("
        + span.begin.column + "," + span.begin.row + " is an expression expected " +
        ", but there is nothing! '" + arr(column) + "'")
      case Right(p) => throw new RuntimeException("This PreAndErrorToken was not expected: " + p)
    }

    if (arr(column)(row).isLetter) {
      lexIdentifier(column, row) match {
        case (Left(a), r) => {
          var newRow = r
          //            val i: Token = a
          skipWhitespaceWhitoutNewLine(column, newRow) match {
            case (c, r) => {
              newRow = r
            }
          }
          arr(column).substring(newRow, newRow + 1) match {
            case ":" => {
              if (arr(column).length >= newRow + 2) {
                arr(column).substring(newRow, newRow + 2) match {
                  case "::" => {
                    return beginTypAnnotatedIdent(column, row, list)._1
                  }
                  case a => throw new IllegalStateException(
                    "Here should be an '::' or '=', but whitout this nothing new can be started")
                }
              } else {
                throw new IllegalStateException(
                  "Here should be an '::' or '=', but whitout this nothing new can be started")
              }
            }
            case "=" => {
              if (arr(column).length >= newRow + 2) {
                arr(column).substring(newRow, newRow + 2) match {
                  case "==" => throw new IllegalStateException(
                    "Here should be an '::' or '=', but whitout this nothing new can be started")
                  case a => {
                    throw new IllegalStateException("You can't start with a NamedExpr")
                  }
                }
              } else {
                throw new IllegalStateException("You can't start with a NamedExpr")
              }
            }
            case a => throw new IllegalStateException(
              "Here should be an '::' or '=', but whitout this nothing new can be started")
          }
        }
        case (Right(a),_)=> a.throwException()
      }
    }else{
    throw new IllegalStateException(
      "Here is at the Beginning in line " + column + " a Identifier expected, but here is no Identifier!")
    }
    throw new IllegalStateException("Until Here should nothing come")
  }


  private def lexerNamedExpr(oldColumn:Int, oldRow:Int, l:List[Token]):TokenAndPos = {
    //println("lexerNamedExpr: "+ l)
    val arr: Array[String]= fileReader.sourceLines
    var row = oldRow
    var column = oldColumn
    require(row>=0, "row is not allowed to be negative")
    require(column>=0, "column is not allowed to be negative")
    require(arr.length > column, "array does not have so much columns")
    require(arr(column).length > row, "arr(column) has less than row chars")

    var list= l

    isEnd(fileReader, column, row, arr) match {
      case Left((c, r)) => {
        column = c
        row = r
      }
      case Right(EndOfFile(_, _)) => throw new RuntimeException("Here occoured a EndOfFile Exeption," +
        " but this should not be able to happen")
      case Right(EndOfLine(span, _)) => throw new RuntimeException("At position ("
        + span.begin.column + "," + span.begin.row + " is an expression expected " +
        ", but there is nothing! '" + arr(column) + "'")
      case Right(p) => throw new RuntimeException("This PreAndErrorToken was not expected: " + p)
    }

    lexIdentifier(column, row) match {
      case (Left(a), r) => {
        row = r
        list=list.::(a)
      }
      case (Right(a), _) => {
        a.throwException()
      }
    }

    skipWhitespaceWhitoutNewLine(column, row) match {
      case (c,r) =>{
        column = c
        row = r
      }
    }

    lexEqualsSign(column, row) match {
      case Left(a) => {
        row = row+1
        list=list.::(a)
      }
      case Right(a) => {
        a.throwException()
      }
    }

    val res = lexerExpression(column, row,list)
    res
  }

  private def lexerTypAnnotatedIdent(oldColumn:Int, oldRow:Int, l:List[Token]):TokenAndPos = {
    //println("lexerTypAnnotatedIdent: "+ l)
    val arr: Array[String]= fileReader.sourceLines
    var row = oldRow
    var column = oldColumn
    require(row>=0, "row is not allowed to be negative")
    require(column>=0, "column is not allowed to be negative")
    require(arr.length > column, "array does not have so much columns")
    require(arr(column).length > row, "arr(column) has less than row chars")

    //in this list we add all
    var list= l

    //ignore whitespaces
    skipWhitespace(column, row) match {
      case (c,r) =>{
        column = c
        row = r
      }
    }

    lexIdentifier(column, row) match {
      case (Left(a), r) => {
        row = r
        list=list.::(a)
      }
      case (Right(a), _) => {
        a.throwException()
      }
    }

    skipWhitespaceWhitoutNewLine(column, row) match {
      case (c,r) =>{
        column = c
        row = r
      }
    }

    lexDoubleDots(column, row) match {
      case Left(a) => {
        row = row+2
        list=list.::(a)
      }
      case Right(a) => {
        a.throwException()
      }
    }

    (lexerTypAnnotationExpression(column, row,list), column, row)
  }



  private def lexerDepLambda(oldColumn:Int, oldRow:Int, l:List[Token]): Either[TokenAndPos, PreAndErrorToken] = {
    //println("lexerDepLambda: "+ l)
    val arr: Array[String]= fileReader.sourceLines
    var row = oldRow
    var column = oldColumn
    require(row>=0, "row is not allowed to be negative")
    require(column>=0, "column is not allowed to be negative")
    require(arr.length > column, "array does not have so much columns")
    require(arr(column).length > row, "arr(column) has less than row chars")

    //in this list we add all
    var list= l

    //ignore whitespaces
    skipWhitespace(column, row, arr) match {
      case (c,r) =>{
        column = c
        row = r
      }
    }

    //only one step
    lexBackslash(column, row) match {
      case Left(a) => {
        row = row +1
        list= list.::(a)
      }
      case Right(a) => {
        return Right(a)
      }
    }

    //ignore whitespaces
    skipWhitespace(column, row) match {
      case (c,r) =>{
        column = c
        row = r
      }
    }
    //more than one step but column keeps the same
    lexIdentifier(column, row) match {
      case (Left(TypeIdentifier(name, span)), r) => {
        row = r
        list=list.::(TypeIdentifier(name, span))
      }
      case (Left(Identifier(name, span)), r) => {
        return Right(TypeIdentifierExpectedNotIdentifier(name, span, fileReader))
      }
      case (Left(a), _) => return Right(NotExpectedToken("Identifier or TypeIdentifier", a.toString, a.s, fileReader))
      case (Right(a), _) => {
        return Right(a)
      }
    }

    //ignore whitespaces
    skipWhitespace(column, row) match {
      case (c,r) =>{
        column = c
        row = r
      }
    }

    //only one or two steps
    lexDotsOrArrow(column, row) match {
      case Left(Colon(span)) => {
        row = row +1
        list= list.::(Colon(span))
      }
      case Left(DepArrow(span)) => {
        row = row +2
        list= list.::(DepArrow(span))
      }
      case Left(Arrow(span)) => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        return Right(NotExpectedToken("=>", "->", new Span(fileReader, loc), fileReader))
      }
      case Left(a) => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        return Right(NotExpectedToken("Arrow or Dots", a.toString, new Span(fileReader, loc), fileReader))
      }
      case Right(a) => {
        return Right(a)
      }
    }

    //ignore whitespaces
    skipWhitespace(column, row) match {
      case (c,r) =>{
        column = c
        row = r
      }
    }

    //match the last Token in the List
    list(0) match {
      case Colon(_) => {
        // :Typ ->
        lexKind(column,row) match {
          case (Left(a), r) => {
            row = r
            list=list.::(a)
          }
          case (Right(a), _) => {
            return Right(a)
          }
        }
        //ignore whitespaces
        skipWhitespace(column, row) match {
          case (c,r) =>{
            column = c
            row = r
          }
        }

        //only two steps
        lexDepArrow(column, row) match {
          case Left(a) => {
            row = row +2
            list= list.::(a)
          }
          case Right(a) => {
            return Right(a)
          }
        }
      }
      case DepArrow(_) => {
        //nothing to do
      }
      case a => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        return Right(ThisTokenShouldntBeHereExpectedArrowOrDots(a, new Span(fileReader, loc),fileReader))
      }
    }
    Left(lexerExpression(column, row,list))
  }

private def lexerLambda(oldColumn:Int, oldRow:Int, l:List[Token]):Either[TokenAndPos, PreAndErrorToken] = {
  //println("lexerLambda: "+ l)
    val arr: Array[String]= fileReader.sourceLines
    var row = oldRow
    var column = oldColumn
    require(row>=0, "row is not allowed to be negative")
    require(column>=0, "column is not allowed to be negative")
    require(arr.length > column, "array does not have so much columns")
    require(arr(column).length > row, "arr(column) has less than row chars")

    //in this list we add all
    var list= l

    //ignore whitespaces
    skipWhitespace(column, row, arr) match {
      case (c,r) =>{
        column = c
        row = r
      }
    }

    //only one step
  lexBackslash(column, row) match {
    case Left(a) => {
      row = row +1
      list= list.::(a)
    }
    case Right(a) => {
      return Right(a)
    }
  }

  //ignore whitespaces
      skipWhitespace(column, row) match {
      case (c,r) =>{
        column = c
        row = r
      }
    }

  //more than one step but column keeps the same
  lexIdentifier(column, row) match {
    case (Left(Identifier(name, span)), r) => {
      row = r
      list=list.::(Identifier(name, span))
    }
    case (Left(TypeIdentifier(name, span)), r) => {
      return Right(IdentifierExpectedNotTypeIdentifier(name, span, fileReader))
    }
    case (Left(a), _) => return Right(NotExpectedToken("Identifier or TypeIdentifier", a.toString, a.s, fileReader))
    case (Right(a), _) => {
      return Right(a)
    }
  }

  //ignore whitespaces
      skipWhitespace(column, row) match {
      case (c,r) =>{
        column = c
        row = r
      }
    }

  //only one or two steps
  lexDotsOrArrow(column, row) match {
    case Left(Colon(span)) => {
      row = row +1
      list= list.::(Colon(span))
    }
    case Left(Arrow(span)) => {
      row = row +2
      list= list.::(Arrow(span))
    }
    case Left(DepArrow(span)) => {
      val loc:Location = Location(column, row) //endLocation is equal to startLocation
      return Right(NotExpectedToken("->", "=>", new Span(fileReader, loc), fileReader))
    }
    case Left(a) => {
      val loc:Location = Location(column, row) //endLocation is equal to startLocation
      return Right(NotExpectedToken("Arrow or Dots", a.toString, new Span(fileReader, loc), fileReader))
    }
    case Right(a) => {
      return Right(a)
    }
  }

  //ignore whitespaces
      skipWhitespace(column, row) match {
      case (c,r) =>{
        column = c
        row = r
      }
    }

  //match the last Token in the List
  list(0) match {
    case Colon(_) => {
      // :Typ ->
      lexTypAnnotationTokenLoopInNamedExpr(column,row,list) match {
        case (c,r,l)=> {
          column = c
          row = r
          list = l
        }
      }
      //ignore whitespaces
          skipWhitespace(column, row) match {
      case (c,r) =>{
        column = c
        row = r
      }
    }
      list(0) match {
        case Arrow(_) => {
          //nothing to do
        }
        case t => NotExpectedToken("=>", t.toString, t.s, fileReader).throwException()
      }
    }
    case Arrow(_) => {
      //nothing to do
    }
    case a => {
      val loc:Location = Location(column, row) //endLocation is equal to startLocation
      return Right(ThisTokenShouldntBeHereExpectedArrowOrDots(a, new Span(fileReader, loc),fileReader))
    }
  }
  Left(lexerExpression(column, row,list))
}


  private def isEnd(fileReader: FileReader, c: Int, r: Int, arr:Array[String]): Either[(Int, Int), PreAndErrorToken] ={
    //ignore whitespaces
    val (column, row) = skipWhitespace(c, r)
    //are you able to take arr(column)(row)?
    if(arr.length <= column){
      var h:String = "'\n"
      for(x <-arr){
        h = h ++ x ++ "\n"
      }
      h = h++ "'"
      val loc:Location = Location(column, row) //endLocation is equal to startLocation
      Right(EndOfFile(new Span(fileReader, loc),fileReader))
    }else if(arr(column).length <= row ){
      val loc:Location = Location(column, row) //endLocation is equal to startLocation
      Right(EndOfLine(new Span(fileReader, loc),fileReader))
    }else{
      Left((column, row))
    }
  }

  private def lexTypAnnotationToken(oldColumn:Int, oldRow:Int, l:List[Token]):Either[(Int, Int, List[Token]),
    PreAndErrorToken] = {
    println("lexerTypAnnotationExpression: "+ l + " ( "+ oldColumn + " , " + oldRow + " )")
    val arr: Array[String] = fileReader.sourceLines
    var row = oldRow
    var column = oldColumn
    require(row >= 0, "row is not allowed to be negative")
    require(column >= 0, "column is not allowed to be negative")

    var list = l
    skipWhitespaceWhitoutNewLine(column, row) match {
      case (c, r) => {
        column = c
        row = r
      }
    }
    if (arr(column).length <= row) {
      val loc: Location = Location(column, row)
      return Right(EndOfLine(new Span(fileReader, loc), fileReader))
    }
    arr(column)(row) match {
      case '(' => {
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        list = list.::(LParentheses(new Span(fileReader, loc)))
        row = row + 1
      }
      case ')' => {
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        list = list.::(RParentheses(new Span(fileReader, loc)))
        row = row + 1
      }
      case '[' => {
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        list = list.::(LBracket(new Span(fileReader, loc)))
        row = row + 1
      }
      case ']' => {
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        list = list.::(RBracket(new Span(fileReader, loc)))
        row = row + 1
      }
      case ',' => {
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        list = list.::(Comma(new Span(fileReader, loc)))
        row = row + 1
      }
      case '.' => {
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        list = list.::(Dot(new Span(fileReader, loc)))
        row = row + 1
      }
      case ':' => {
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        list = list.::(Colon(new Span(fileReader, loc)))
        row = row + 1
      }
      case '-' => {
        if (arr(column).length <= row + 1) {
          val loc: Location = Location(column, row) //endLocation is equal to startLocation
          return Right(ToShortToBeThisToken(2, "->", Span(fileReader, loc, loc), fileReader))
        } else {
          val beginLoc: Location = Location(column, row)
          val endLoc: Location = Location(column, row + 1)
          val span: Span = Span(fileReader, beginLoc, endLoc)
          arr(column).substring(row, row + 2) match {
            case "->" => {
              list = list.::(Arrow(span))
              row = row + 2
            }
            case a => {
              return Right(NotExpectedToken("->", a, span, fileReader))
            }
          }
        }
      }
      case a =>return differentCasesSymbolInTypeRecognizing(column, row, list, a, arr)
    }
    Left((column, row, list))
  }

  def differentCasesSymbolInTypeRecognizing(c:Int, r:Int, li:List[Token], symbol: Char, arr:Array[String]):Either[(Int, Int, List[Token]),
    PreAndErrorToken] = {
    var column = c
    var row = r
    var list = li
    if (symbol.isDigit) {
      lexVectorType(column,row) match {
        case (Left(a), r) => {
          row = r
          list = list.::(a)
        }
        case (Right(e), r) => {
          lexNatNumber(column, row) match {
            case (nat, r) => {
              row = r
              skipWhitespaceWhitoutNewLine(column, row) match {
                case (c1, r1) => {
                  column = c1
                  row = r1
                }
              }
              if(arr(column).length <= row){
                val loc = Location(column, row)
                return Right(EndOfLine(new Span(fileReader, loc), fileReader))
              }
              lexDot(column, row) match {
                case Left(dot) => {
                  row = row + 1
                  skipWhitespaceWhitoutNewLine(column, row) match {
                    case (c, r) => {
                      column = c
                      row = r
                    }
                  }
                  list = list.::(nat).::(dot)
                }
                case Right(e) => return Right(e)
              }
            }
          }
        }
      }
    } else if (arr(column).length > row + 2 &&
      arr(column).substring(row, row + 2).equals("=>")) {
      val beginLoc = Location(column, row)
      val endLoc = Location(column, row + 1)
      val depArrow = DepArrow(Span(fileReader, beginLoc, endLoc))
      row = row + 2
      list = list.::(depArrow)
    } else if (arr(column).length > row + 3 &&
      arr(column).substring(row, row + 3).equals("Idx")) {
      val beginLoc = Location(column, row)
      val endLoc = Location(column, row + 2)
      val idx = TypeIdentifier("Idx", Span(fileReader, beginLoc, endLoc))
      row = row + 3
      val lB = lexLBracket(column, row) match {
        case Right(e) => return Right(e)
        case Left(t) => t
      }
      row = row + 1
      skipWhitespaceWhitoutNewLine(column, row) match {
        case (c, r) => {
          column = c
          row = r
        }
      }
      val (nat, r) = lexNatNumber(column, row)
      row = r
      skipWhitespaceWhitoutNewLine(column, row) match {
        case (c, r) => {
          column = c
          row = r
        }
      }
      val rB = lexRBracket(column, row) match {
        case Right(e) => return Right(e)
        case Left(t) => t
      }
      row = row + 1

      list = list.::(idx).::(lB).::(nat).::(rB)
    } else {
      lexScalarType(column, row) match {
        case (Left(a), r) => {
          row = r
          list = list.::(a)
        }
        case (Right(_), _) => lexKind(column, row) match {
          case (Left(a), r) => {
            row = r
            skipWhitespaceWhitoutNewLine(column, row) match {
              case (c, r) => {
                column = c
                row = r
              }
            }
            lexDepArrow(column, row) match {
              case Left(b) => {
                row = row + 2
                list = list.::(a).::(b)
              }
              case Right(e) => {
                return Right(e)
              }
            }
          }
          case (Right(e), _) => {
//            println("column: "+ column + ", row: " + row)
            if(!arr(column)(row).isLetterOrDigit){
              if(arr(column)(row).isWhitespace && arr(column).length <=row+1){
                val loc: Location = Location(column, row) //endLocation is equal to startLocation
//                println("column: "+ column + ", row: " + row)
                return Right(EndOfLine(new Span(fileReader, loc),fileReader))
              }
              if(arr(column)(row).equals('#')){
                throw new IllegalStateException(
                  "Every '#' should have been removed in the preLexer. This is an IllegalState of the Lexer.")
              }else{
                val loc: Location = Location(column, row) //endLocation is equal to startLocation
                return Right(NotExpectedToken("Some Number or String",
                  arr(column)(row).toString, new Span(fileReader, loc), fileReader))
              }
            }
            lexIdentifier(column, row) match {
              case (Left(ident), r) => {
                row = r
                if (ident.isInstanceOf[TypeIdentifier]) {
                  list = list.::(ident)
                } else {
                  return Right(TypeIdentifierExpectedNotIdentifier(ident.toString, ident.s, fileReader))
                }
              }
              case (Right(e), _) => return Right(e)
            }
          }
        }
      }
    }
    Left((column, row, list))
  }

  private def lexTypAnnotationTokenLoopInNamedExpr(oldColumn:Int, oldRow:Int, l:List[Token]):(Int, Int, List[Token]) = {
    //println("lexerTypAnnotationExpression: "+ l + " ( "+ oldColumn + " , " + oldRow + " )")
    var (column, row, list) = lexTypAnnotationToken(oldColumn, oldRow, l) match {
      case Left(a) => a
      case Right(_) => return (oldColumn, oldRow, l)
    }

    skipWhitespaceWhitoutNewLine(column, row) match {
      case (c, r) => {
        column = c
        row = r
      }
    }
    lexTypAnnotationTokenLoopInNamedExpr(column,row, list)
  }

  private def startNewTypAnnoationOrNewExpr(column:Int, row:Int, list:List[Token],arr: Array[String] =
  fileReader.sourceLines):List[Token]={
    if (arr(column)(row).isLetter) {
      lexIdentifier(column, row) match {
        case (Left(_), r) => {
          var newRow = r
          skipWhitespaceWhitoutNewLine(column, newRow) match {
            case (_, r) => {
              newRow = r
            }
          }

          arr(column).substring(newRow, newRow + 1) match {
            case ":" => {
              if (arr(column).length >= newRow + 2) {
                arr(column).substring(newRow, newRow + 2) match {
                  case "::" => {
                    return endTypAnnotatedIdentBeginTypAnnotatedIdent(column, row, list)._1
                  }
                  case _ =>throw new IllegalStateException("Here should be an '::' or '=', but whitout this nothing new can be started")
                }
              } else {
                throw new IllegalStateException("Here should be an '::' or '=', but whitout this nothing new can be started")
              }
            }
            case "=" => {
              if (arr(column).length >= row + 2) {
                arr(column).substring(row, row + 2) match {
                  case "==" =>throw new IllegalStateException("Here should be an '::' or '=', but whitout this nothing new can be started")
                  case a => {
                    return endTypAnnotatedIdentBeginNamedExpr(column, row, list)._1
                  }
                }
              } else {
                return endTypAnnotatedIdentBeginNamedExpr(column, row, list)._1
              }
            }
            case a =>throw new IllegalStateException("Here should be an '::' or '=', but whitout this nothing new can be started")
          }
        }
        case (Right(_), _) =>throw new IllegalStateException("Here should be an '::' or '=', but whitout this nothing new can be started")
      }
    }else {
      throw new IllegalStateException("Here should be an Identifier, but whitout an Identifier nothing new can be started")
    }
  }

  private def howToContinueLexerTypAnnoationExpression(oldColumn:Int, oldRow:Int, list:List[Token],arr: Array[String] = fileReader.sourceLines):List[Token] = {
    var column = oldColumn
    var row = oldRow

    if(row >= arr(column).length){
      //end of Line is reached is reached and TypAnnotatedIdent has to be in one line
      //println("before isEnd1: " + column + " , " + row )
      isEnd(fileReader, column, row, arr) match {
        case Left((c, r)) => {
          column = c
          row = r
        }
        case Right(EndOfFile(_, _)) => return list
        case Right(EndOfLine(span, _)) => return list
        case Right(p) => throw new RuntimeException("This PreAndErrorToken was not expected: " + p)
      }
      //println("after isEnd1: " + column + " , " + row )
      startNewTypAnnoationOrNewExpr(column,row,list,arr)
    }else{ //a second expression is accepted
      lexerTypAnnotationExpression(column, row, list)
    }
  }

  private def lexerTypAnnotationExpression(oldColumn:Int, oldRow:Int, l:List[Token]):List[Token] = {
    //println("lexerTypAnnotationExpression: "+ l + " ( "+ oldColumn + " , " + oldRow + " )")
    var (column, row, list):(Int,Int,List[Token]) = lexTypAnnotationToken(oldColumn, oldRow, l) match{
      case Left(a) => a
      case Right(EndOfLine(_,_)) => return startNewTypAnnoationOrNewExpr(oldColumn+1,0,l)
      case Right(e) => {
        e.throwException()
        return l
      }
    }

    skipWhitespaceWhitoutNewLine(column, row) match {
      case (c,r) =>{
        column = c
        row = r
      }
    }
    howToContinueLexerTypAnnoationExpression(column,row,list)
  }

  private def lexerExpression(oldColumn:Int, oldRow:Int, l:List[Token]):(List[Token], Int, Int) = {
    //println("lexerExpression: "+ l + " ( "+ oldColumn + " , " + oldRow + " )")
    val arr: Array[String] = fileReader.sourceLines
    var row = oldRow
    var column = oldColumn
    require(row >= 0, "row is not allowed to be negative")
    require(column >= 0, "column is not allowed to be negative")

    var list = l
    isEnd(fileReader, column, row, arr) match {
      case Left((c, r)) => {
        column = c
        row = r
      }
      case Right(EndOfFile(_, _)) => throw new RuntimeException("Here occoured a EndOfFile Exeption," +
        " but this should not be able to happen")
      case Right(EndOfLine(span, _)) => throw new RuntimeException("At position ("
        + span.begin.column + "," + span.begin.row + " is an expression expected " +
        ", but there is nothing! '" + arr(column) + "'")
      case Right(p) => throw new RuntimeException("This PreAndErrorToken was not expected: " + p)
    }
    if (isBinaryOperatorSymbol(arr(column)(row))) {
      lexBinOperator(column, row) match {
        case Left(BinOp(BinOpType.EQ, span)) => {
          row = row + 2
          list = list.::(BinOp(BinOpType.EQ, span))
        }
        case Left(a) => {
          row = row + 1
          //   //println("\n\n"+ a.toString)
          list = list.::(a)
        }
        case Right(a) => {
          lexDotsOrArrow(column,row) match {
            case Left(arrow) =>{
              row = row+2
              list = list.::(arrow)
            }
            case Right(e) => e.throwException()
          }
        }
      }
    } else {
      //println("mitte: "+ arr(column)(row))
      arr(column)(row) match {
        case '\\' => {
          lexerLambda(column, row, list) match {
            case Right(eLambda) => if(!eLambda.isInstanceOf[IdentifierExpectedNotTypeIdentifier]) {
              eLambda.throwException()
            }else{
              lexerDepLambda(column, row, list) match {
                case Right(eDepLambda) => eDepLambda.throwException()
                case Left(p) => return p
              }
            }
            case Left(p) => return p
          }
        }
        case '.' => {
          val loc: Location = Location(column, row) //endLocation is equal to startLocation
          list = list.::(Dot(new Span(fileReader, loc)))
          row = row + 1
        }
        case '(' => {
          val loc: Location = Location(column, row) //endLocation is equal to startLocation
          list = list.::(LParentheses(new Span(fileReader, loc)))
          row = row + 1
        }
        case ')' => {
          val loc: Location = Location(column, row) //endLocation is equal to startLocation
          list = list.::(RParentheses(new Span(fileReader, loc)))
          row = row + 1
        }
        case '!' => {
          val loc: Location = Location(column, row) //endLocation is equal to startLocation
          list = list.::(UnOp(UnaryOpType.NOT, new Span(fileReader, loc)))
          row = row + 1

          isEnd(fileReader, column, row, arr) match {
            case Left((c, r)) => {
              column = c
              row = r
            }
            case Right(EndOfFile(_, _)) => throw new RuntimeException("an Negation needs an Expression after it")
            case Right(EndOfLine(span, _)) => throw new RuntimeException("At position ("
              + span.begin.column + "," + span.begin.row + " is an expression expected " +
              ", but there is nothing! '" + arr(column) + "'")
            case Right(p) => throw new RuntimeException("This PreAndErrorToken was not expected: " + p)
          }
          return lexerExpression(column, row, list)
        }
        case '~' => { //TODO: Is this symbol as a Neg-Sign ok?
          val loc: Location = Location(column, row) //endLocation is equal to startLocation
          list = list.::(UnOp(UnaryOpType.NEG, new Span(fileReader, loc)))
          row = row + 1

          isEnd(fileReader, column, row, arr) match {
            case Left((c, r)) => {
              column = c
              row = r
            }
            case Right(EndOfFile(_, _)) => throw new RuntimeException("an Negation needs an Expression after it")
            case Right(EndOfLine(span, _)) => throw new RuntimeException("At position ("
              + span.begin.column + "," + span.begin.row + " is an expression expected " +
              ", but there is nothing! '" + arr(column) + "'")
            case Right(p) => throw new RuntimeException("This PreAndErrorToken was not expected: " + p)
          }
          return lexerExpression(column, row, list)
        }
        case a => {
          if (a.isDigit) {
            //more than one step but column keeps the same
            lexNumber(column, row) match {
              case (Left(a), r) => {
                row = r
                list = list.::(a)
              }
              case (_, _) => {
                  typeRecognizingInNoAppExpr(column,row, list, a, arr) match{
                    case Left((c,r,l))=>{
                      column =c
                      row = r
                      list = l
                    }
                    case Right(e) => e.throwException()
              }
            }
          }
        }else{
            if (a.isWhitespace && column+1>=arr.length && arr(column).length-1 <=row) {
              println("exit typeRecognizingInNoAppExpr:: "+"column: "+ column + ", row: " + row)
              return (list,column, row)
            }
              typeRecognizingInNoAppExpr(column,row, list, a, arr) match{
                case Left((c,r,l))=>{
                  column =c
                  row = r
                  list = l
                }
                case Right(e) => e.throwException()
              }
          }
        }
      }
    }
    //ignore whitespaces
    skipWhitespaceWhitoutNewLine(column, row) match {
      case (c, r) => {
        column = c
        row = r
      }
    }
    if (arr(column).length >= row) {
      //println("before isEnd: " + column + " , " + row )
      isEnd(fileReader, column, row, arr) match {
        case Left((c, r)) => {
          column = c
          row = r
        }
        case Right(EndOfFile(_, _)) => throw new RuntimeException("Here occoured a EndOfFile Exeption," +
          " but this should not be able to happen")
        case Right(EndOfLine(span, _)) => {
          //println("EndOfLine in lexerExpression: " + list)
          return (list, column, row)
        } //end is reached
        case Right(p) => throw new RuntimeException("This PreAndErrorToken was not expected: " + p)
      }
      //println("after isEnd: " + column + " , " + row + " arr(column)(row)= '" + arr(column)(row)+ "'")

      if (arr(column)(row).isLetter) {
        lexIdentifier(column, row) match {

          case (Left(a), r) => {
            var newRow = r
            //            val i: Token = a
            skipWhitespaceWhitoutNewLine(column, newRow) match {
              case (c, r) => {
                newRow = r
              }
            }
            //println("Alice Wonderland: "+ list + " <<<<>>>> " + column + " , " + row)
            if (newRow >= arr(column).length) {
              //println("escape: " + row + " , "+ column)
              return lexerExpression(column, row, list)
            }
            arr(column).substring(newRow, newRow + 1) match {
              case ":" => {
                if (arr(column).length >= newRow + 2) {
                  arr(column).substring(newRow, newRow + 2) match {
                    case "::" => {
                      return endNamedExprBeginTypAnnotatedIdent(column, row, list)
                    }
                    case a =>
                  }
                } else {

                }
              }
              case "=" => {
                if (arr(column).length >= row + 2) {
                  arr(column).substring(row, row + 2) match {
                    case "==" =>
                    case a => {
                      //println("endNamedExpr1: " + list)
                      return endNamedExprBeginNamedExpr(column, row, list)
                    }
                  }
                } else {
                  //println("endNamedExpr2: " + list)
                  return endNamedExprBeginNamedExpr(column, row, list)
                }
              }
              case a =>
            }
          }
          case (Right(_), _) =>
        }
      } else {
        //println("not in the end of Line: " + list)
      }
    }


    if (column >= arr.length || (column == arr.length - 1 && row >= arr(column).length)) {
      return (list, column, row) //end is reached
    } else { //a second expression is accepted
      //println("Neustart: " + list + "  ( "+  column + " , " + row + " )")
      return lexerExpression(column, row, list)
    }
  }

  def typeRecognizingInNoAppExpr(c:Int, r:Int, li:List[Token], symbol: Char, arr:Array[String]):Either[(Int, Int, List[Token]),
    PreAndErrorToken] = {
    var column = c
    var row = r
    var list = li
    println("typeRecognizingInNoAppExpr:: "+ "column: "+ column + ", row: " + row)
  differentCasesSymbolInTypeRecognizing(column, row, list, symbol, arr) match{
    case Left((c,r,l))=>{
      column =c
      row = r
      list = l
    }
    case Right(_)=>{
      if (symbol.isLetter) { //Todo:Add here lexType
        lexIdentifier(column, row) match {
          case (Left(a), r) => {
            row = r
            list = list.::(a)
          }
          case (Right(a), _) => {
            a.throwException()
          }
        }
      } else if (otherKnownSymbol(symbol)) {
        //Todo:Maybe this with lexTypAnnotationToken works fine, so that I can delete this here
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        val ex = NotExpectedToken("an Identifier or a Number or \\ or a Brace or a UnOperator or a BinOperator",
          "" + symbol, new Span(fileReader, loc), fileReader)
        ex.throwException()
      } else if (symbol.isWhitespace && column+1>=arr.length && arr(column).length-1 <=row) {
        println("exit typeRecognizingInNoAppExpr:: "+"column: "+ column + ", row: " + row)
        return Left((column, row,list))
      } else {
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        val ex = UnknownSymbol(symbol, new Span(fileReader, loc), fileReader)
        ex.throwException()
      }
    }
  }
    Left((column, row, list))
}

  private def endNamedExprBeginTypAnnotatedIdent(column: Int, row: Int, l: List[Token]):TokenAndPos = {
    var list = l
    val loc: Location = Location(column, row)
    val span = new Span(fileReader, loc)
    list = list.::(EndNamedExpr(span))
    list = list.::(BeginTypAnnotatedIdent(span))
    var (newList, c, r) = lexerTypAnnotatedIdent(column, row, list)
    if((!newList.head.isInstanceOf[EndTypAnnotatedIdent])&&(!newList.head.isInstanceOf[EndNamedExpr])){
      newList = newList.::(EndTypAnnotatedIdent(new Span(fileReader, Location(c, r))))
    }
    //println("endNamedExprBeginTypAnnotatedIdent ended: "+  newList)
    (newList, c, r)
  }
  private def endNamedExprBeginNamedExpr(column: Int, row: Int, l: List[Token]):TokenAndPos = {
    var list = l
    val loc: Location = Location(column, row)
    val span = new Span(fileReader, loc)
    list = list.::(EndNamedExpr(span))
    list = list.::(BeginNamedExpr(span))
    var (newList, c, r) = lexerNamedExpr(column, row, list)
    if((!newList.head.isInstanceOf[EndTypAnnotatedIdent])&&(!newList.head.isInstanceOf[EndNamedExpr])){
      newList = newList.::(EndNamedExpr(new Span(fileReader, Location(c, r))))
    }
    //println("endNamedExprBeginNamedExpr ended: "+  newList)
    (newList, c, r)
  }

  private def endTypAnnotatedIdentBeginTypAnnotatedIdent(column: Int, row: Int, l: List[Token]):TokenAndPos = {
    var list = l
    val loc: Location = Location(column, row)
    val span = new Span(fileReader, loc)
    list = list.::(EndTypAnnotatedIdent(span))
    list = list.::(BeginTypAnnotatedIdent(span))
    var (newList, c, r) = lexerTypAnnotatedIdent(column, row, list)
    if((!newList.head.isInstanceOf[EndTypAnnotatedIdent])&&(!newList.head.isInstanceOf[EndNamedExpr])){
      newList = newList.::(EndTypAnnotatedIdent(new Span(fileReader, Location(c, r))))
    }
    //println("endTypAnnotatedIdentBeginTypAnnotatedIdent ended: "+  newList)
    (newList, c, r)
  }
  private def endTypAnnotatedIdentBeginNamedExpr(column: Int, row: Int, l: List[Token]):TokenAndPos = {
    var list = l
    val loc: Location = Location(column, row)
    val span = new Span(fileReader, loc)
    list = list.::(EndTypAnnotatedIdent(span))
    list = list.::(BeginNamedExpr(span))
    var (newList, c, r) = lexerNamedExpr(column, row, list)
    if((!newList.head.isInstanceOf[EndTypAnnotatedIdent])&&(!newList.head.isInstanceOf[EndNamedExpr])){
      newList = newList.::(EndNamedExpr(new Span(fileReader, Location(c, r))))
    }
    //println("endTypAnnotatedIdentBeginNamedExpr ended: "+  newList)
    (newList, c, r)
  }
  private def beginTypAnnotatedIdent(column: Int, row: Int, l: List[Token]):TokenAndPos = {
    var list = l
    val loc: Location = Location(column, row)
    val span = new Span(fileReader, loc)
    list = list.::(BeginTypAnnotatedIdent(span))
    lexerTypAnnotatedIdent(column, row, list)
  }

  /*
  skip the Whitespaces
  return (column, row)
   */
  private def skipWhitespace(column:Int, row: Int, arr: Array[String]= fileReader.sourceLines):(Int, Int)= {
  if(arr(column).length <= row){
      if(arr.length <= column+1){
        return (column, row)
      }else{
        val c = column +1
        val r = 0
        return skipWhitespace(c,r)
      }
    }
//  println("öööö"+ arr(column).length + " : '"+ arr(column)+ "'")
    if (arr(column)(row).isWhitespace) {
      if (arr(column).length > row + 1) {
        skipWhitespace(column, row + 1)
      } else if (arr.length > column + 1) {
        skipWhitespace(column + 1, 0)
      } else {
        (column, row)
      }
    }else{
      (column, row)
    }
  }

  private def skipWhitespaceWhitoutNewLine(column:Int, row: Int, arr: Array[String]= fileReader.sourceLines):(Int, Int)
    = {
    if(arr(column).length<=row){
      return (column, row)
    }
    if (arr(column)(row).isWhitespace) {
      if (arr(column).length > row + 1) {
        skipWhitespaceWhitoutNewLine(column, row + 1)
      } else {
        (column, row)
      }
    }else{
      (column, row)
    }
  }

  private def isTwoBackslash(column:Int, row: Int, arr: Array[String]= fileReader.sourceLines):Boolean= {
    arr(column).length> row+1 && arr(column).substring(row, row+2).equals("\\\\")
  }

  /*
  we expect to see a Backslash
  requirements:  no whitespace at arr(column)(row)
   */
  private def lexBackslash(column:Int, row: Int, arr: Array[String]= fileReader.sourceLines):
  Either[Token,PreAndErrorToken]= {
    arr(column)(row) match {
      case '\\' => {
        val loc:Location = Location(column, row)
        if(isTwoBackslash(column,row)){
          val endLoc:Location = Location(column, arr(column).length)
          Right(NotExpectedTwoBackslash("\\", Span(fileReader,loc,endLoc), fileReader))
        }
        Left(Backslash(new Span(fileReader,loc)))
      }
      case a => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        Right(NotExpectedToken("\\", ""+ a, Span(fileReader,loc, loc), fileReader))
      }
    }
  }

  private def lexLBracket(column:Int, row: Int, arr: Array[String]= fileReader.sourceLines):
  Either[Token,PreAndErrorToken]= {
    arr(column)(row) match {
      case '[' => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        Left(LBracket(new Span(fileReader,loc)))
      }
      case a => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        Right(NotExpectedToken("[", ""+ a, Span(fileReader,loc, loc), fileReader))
      }
    }
  }

  private def lexRBracket(column:Int, row: Int, arr: Array[String]= fileReader.sourceLines):
  Either[Token,PreAndErrorToken]= {
    arr(column)(row) match {
      case ']' => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        Left(RBracket(new Span(fileReader,loc)))
      }
      case a => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        Right(NotExpectedToken("]", ""+ a, Span(fileReader,loc, loc), fileReader))
      }
    }
  }


//  private def lexColon(column:Int, row: Int, arr: Array[String]= fileReader.sourceLines):Either[Token,PreAndErrorToken]={
//    arr(column)(row) match {
//      case ':' => {
//        val loc:Location = Location(column, row) //endLocation is equal to startLocation
//        Left(Colon(new Span(fileReader,loc)))
//      }
//      case a => {
//        val loc:Location = Location(column, row) //endLocation is equal to startLocation
//        Right(NotExpectedToken(":", ""+ a, Span(fileReader,loc, loc), fileReader))
//      }
//    }
//  }

  private def lexDot(column:Int, row: Int, arr: Array[String]= fileReader.sourceLines):Either[Token,PreAndErrorToken]={
    arr(column)(row) match {
      case '.' => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        Left(Dot(new Span(fileReader,loc)))
      }
      case a => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        Right(NotExpectedToken(".", ""+ a, Span(fileReader,loc, loc), fileReader))
      }
    }
  }


private def lexDeporNormalArrow(column:Int, row: Int, arr: Array[String], symbol: String):
Either[Token,PreAndErrorToken]={
  if(arr(column).length <= row +1){
    val loc:Location = Location(column, row) //endLocation is equal to startLocation
    Right(ToShortToBeThisToken(2, symbol, Span(fileReader,loc, loc), fileReader))
  }else{
    val beginLoc:Location = Location(column, row)
    val endLoc:Location = Location(column, row+1)
    val span:Span = Span(fileReader,beginLoc, endLoc)
    arr(column).substring(row, row+2) match {
      case "->" => {
        Left(Arrow(span))
      }
      case "=>" => {
        Left(DepArrow(span))
      }
      case a => {
        Right(NotExpectedToken(symbol, a, span, fileReader))
      }
    }
  }
}

  /*
we expect to see Dots or an Arrow
requirements:  no whitespace at arr(column)(row)
 */
  private def lexDotsOrArrow(column:Int, row: Int, arr: Array[String]= fileReader.sourceLines):
  Either[Token,PreAndErrorToken]= {
    if(arr(column).length<=row){
        val loc = Location(column, row)
      return Right(EndOfLine(new Span(fileReader, loc), fileReader))
    }
    arr(column)(row) match {
      case ':' => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        Left(Colon(new Span(fileReader,loc)))
      }
      case '-' => {
        lexDeporNormalArrow(column, row, arr, "->")
      }
      case '=' => {
        lexDeporNormalArrow(column, row, arr, "=>")
      }
      case a => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        Right(NotExpectedToken(":", ""+ a, Span(fileReader,loc, loc), fileReader))
      }
    }
  }

  private def lexEqualsSign(column:Int, row: Int, arr: Array[String]= fileReader.sourceLines):
  Either[Token,PreAndErrorToken]= {
    arr(column)(row) match {
      case '=' => {
        if(arr(column).length <= row +1){
          val loc:Location = Location(column, row) //endLocation is equal to startLocation
          Left(EqualsSign(Span(fileReader,loc, loc)))
        }else{
          val beginLoc:Location = Location(column, row)
          val endLoc:Location = Location(column, row+1)
          val span:Span = Span(fileReader,beginLoc, endLoc)
          arr(column).substring(row, row+2) match {
            case "==" => {
              Right(NotExpectedToken("=", "==", span, fileReader))
            }
            case a => {
              val loc:Location = Location(column, row) //endLocation is equal to startLocation
              Left(EqualsSign(Span(fileReader,loc, loc)))
            }
          }
        }
      }
      case a => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        Right(NotExpectedToken("=", ""+ a, Span(fileReader,loc, loc), fileReader))
      }
    }
  }

  private def lexDoubleDots(column:Int, row: Int, arr: Array[String]= fileReader.sourceLines):
  Either[Token,PreAndErrorToken]= {
    if(arr(column).length>=row+2){
      val loc:Location = Location(column, row)
      Right(EndOfLine(new Span(fileReader, loc), fileReader))
    }
    val beginLoc:Location = Location(column, row)
    val endLoc:Location = Location(column, row+1)
    arr(column).substring(row,row+2) match {
      case "::" => {
        Left(DoubleColons(Span(fileReader,beginLoc, endLoc)))
      }
      case a => {
        Right(NotExpectedToken("::", ""+ a, Span(fileReader,beginLoc, endLoc), fileReader))
      }
    }
  }

/*
this lexes if it is an binary operator

if '==' then two steps else only one step
 */
  private def lexBinOperator(column:Int, row:Int, arr: Array[String] = fileReader.sourceLines):
  Either[Token,PreAndErrorToken]= {
    arr(column)(row) match {
      case '-' => {
        if (arr(column).length <= row + 1 || arr(column).substring(row, row + 2) != "->") { // -
          val loc: Location = Location(column, row) //endLocation is equal to startLocation
          Left(BinOp(OpType.BinOpType.SUB, new Span(fileReader, loc)))
        } else { // ->
          val locStart: Location = Location(column, row)
          val locEnd: Location = Location(column, row + 1)
          Right(NotExpectedToken("-", "->", Span(fileReader, locStart, locEnd), fileReader))
        }
      }
      case '+' => { // +
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        Left(BinOp(OpType.BinOpType.ADD, new Span(fileReader, loc)))
      }
      case '*' => { // *
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        Left(BinOp(OpType.BinOpType.MUL, new Span(fileReader, loc)))
      }
      case '/' => { // /
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        Left(BinOp(OpType.BinOpType.DIV, new Span(fileReader, loc)))
      }
      case '%' => { // %
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        Left(BinOp(OpType.BinOpType.MOD, new Span(fileReader, loc)))
      }
      case '<' => { // <
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        Left(BinOp(OpType.BinOpType.LT, new Span(fileReader, loc)))
      }
      case '>' => { // >
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        Left(BinOp(OpType.BinOpType.GT, new Span(fileReader, loc)))
      }
      case '=' => {
        if (arr(column).length <= row + 1 || arr(column).substring(row, row + 2) != "==") {
          val loc: Location = Location(column, row)
          Right(OnlyOneEqualSign(new Span(fileReader, loc), fileReader))
        } else { // ==
          val beginLoc: Location = Location(column, row)
          val endLoc: Location = Location(column, row + 1)
          Left(BinOp(OpType.BinOpType.EQ, Span(fileReader, beginLoc, endLoc)))
        }
      }
      case a => {
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        Right(NOTanBinOperator("" + a  , new Span(fileReader, loc), fileReader))
      }
    }
  }

  private def getConcreteScalarType(substring:String, span:Span):Either[ConcreteType, PreAndErrorToken]={
    substring match {//different Types in RISE //Todo: not completed yet
      //Types
      case "Bool" => Left(BoolType())
      case "I16"   => Left(ShortTyp())
      case "I32"  => Left(IntTyp())
      case "F32"  => Left(FloatTyp())
      case "F64"  => Left(DoubleType())
      case "NatTyp"  => Left(NatTyp())
      case _ => Right(UnknownType(substring, span, fileReader))
    }
  }

  private def lexScalarType(column:Int, row:Int,  arr:Array[String] = fileReader.sourceLines):
  (Either[Token,PreAndErrorToken],Int) = {
    val (pos, substring, locStart) = lexName(column, row, arr)
    if(pos < arr(column).length && !(arr(column)(pos).isWhitespace | otherKnownSymbol(arr(column)(pos)))){
      val locEnd:Location = Location(column, pos+1)
      (Right(UnknownType(substring, Span(fileReader,locStart, locEnd), fileReader)),pos+1)
    }else{
      val locEnd:Location = Location(column, pos)
      val span =  Span(fileReader,locStart, locEnd)
      getConcreteScalarType(substring,span) match{
        case Left(concreteType)=>(Left(ScalarType(concreteType, span)),pos)
        case Right(error) => {
          println("In lexScalarType: "+ error)
          (Right(error), pos)
        }
      }
    }
  }

  private def lexVectorType(column:Int, row:Int,  arr:Array[String] = fileReader.sourceLines):
  (Either[Token,PreAndErrorToken],Int) = {
    if(row+3 >= arr(column).length){
      val loc = Location(column, row)
      return (Right(EndOfLine(new Span(fileReader, loc), fileReader)),row)
    }
    //println("lexVectorType: "+ arr(column).substring(row,row+2))
    arr(column).substring(row,row+2) match{
      case "2x" => lexVectorTypeWithGivenLength(column, row+2, 2)
      case "4x" => lexVectorTypeWithGivenLength(column, row+2, 4)
      case _ => arr(column).substring(row,row+3) match{
        case "8x" =>lexVectorTypeWithGivenLength(column, row+3, 8)
        case "16x" =>lexVectorTypeWithGivenLength(column, row+3, 16)
        case _ =>{
          val locBegin = Location(column, row)
          val locEnd = Location(column, row+3)
          (Right(UnknownType(arr(column).substring(row,row+3),Span(fileReader, locBegin, locEnd),fileReader)),row)
        }
      }
    }
  }

  private def lexVectorTypeWithGivenLength(column:Int, row:Int, len:Int, arr:Array[String] = fileReader.sourceLines):
  (Either[Token,PreAndErrorToken],Int) = {
    val (pos, substring, locStart) = lexName(column, row, arr)
    if(pos < arr(column).length && !(arr(column)(pos).isWhitespace | otherKnownSymbol(arr(column)(pos)))){
      val locEnd:Location = Location(column, pos+1)
      //print("Error in lexVectorType: ("+column + " , " + row + ")" + " :: Pos is " + pos)
      (Right(UnknownType(substring, Span(fileReader,locStart, locEnd), fileReader)),pos+1)
    }else{
      val locEnd:Location = Location(column, pos)
      val span =  Span(fileReader,locStart, locEnd)

      val scalarType = getConcreteScalarType(substring,span) match{
        case Left(concreteType)=> concreteType
        case Right(error) => return (Right(error), pos)
      }
      //print("VectorType("+len+" , " + scalarType + " , " + span + ")")
       (Left(VectorType(len, scalarType, span)),pos)
    }
  }

  private def lexKind(column:Int, row:Int,  arr:Array[String] = fileReader.sourceLines):
  (Either[Token,PreAndErrorToken],Int) = {
    val (pos, substring, locStart) = lexName(column, row, arr)
    if(pos < arr(column).length && !(arr(column)(pos).isWhitespace | otherKnownSymbol(arr(column)(pos)))){
      val locEnd:Location = Location(column, pos+1)
      (Right(UnknownKind(substring, Span(fileReader,locStart, locEnd), fileReader)),pos+1)
    }else{
      val locEnd:Location = Location(column, pos)
      val span =  Span(fileReader,locStart, locEnd)
      //different Types in RISE //Todo: not completed yet
      substring match {
        case "Data" => (Left(Kind(Data(), span)), pos)
        case "AddrSpace" => (Left(Kind(AddrSpace(), span)), pos)
        case "Nat" => (Left(Kind(Nat(), span)), pos)

        case a => (Right(UnknownKind(substring, span, fileReader)),pos)
      }
    }
  }

  private def lexName(column:Int, row:Int,arr:Array[String]):(Int, String, Location) = {
    var r: Int = row + 1
    var substring: String = arr(column).substring(row, r)
    while (r-1 < arr(column).length && arr(column).substring(row, r).matches("[a-zA-Z][a-zA-Z0-9_]*")) {
      substring= arr(column).substring(row, r)
      r = r + 1
    }
    val locStart:Location = Location(column, row)
    val pos:Int = r-1
    (pos, substring, locStart)
  }
  /*
    for example "split", "go", "def", "while"

   */
  private def lexIdentifier( column:Int, row:Int, arr:Array[String] = fileReader.sourceLines):
  (Either[Token,PreAndErrorToken],Int) = {
    val (pos, substring, locStart) = lexName(column, row, arr)
    if(pos < arr(column).length && !(arr(column)(pos).isWhitespace | otherKnownSymbol(arr(column)(pos)))){
      val locEnd:Location = Location(column, pos+1)
      (Right(IdentifierWithNotAllowedSymbol(arr(column)(pos), arr(column).substring(row, pos+1),
        Span(fileReader,locStart, locEnd), fileReader)), pos+1)
    }else{
      val locEnd:Location = Location(column, pos)
      val span = Span(fileReader,locStart, locEnd)
    substring match {
      case "Local" => (Left(AddrSpaceType(substring, span)),pos)
      case "Global" => (Left(AddrSpaceType(substring, span)),pos)
      case "Private" => (Left(AddrSpaceType(substring, span)),pos)
      case "Constant" => (Left(AddrSpaceType(substring, span)),pos)
        //keywords are handled in the parser (matchPrimitiveOrIdentifier)
      case _ => {
        if (substring.matches("[a-z][a-zA-Z0-9_]*")){
          (Left(Identifier(substring, span )), pos)
        }else{
          (Left(TypeIdentifier(substring, span)), pos)
        }
      }
    }
    }
  }

  /*
  span.end.row - span.begin.row is the number of steps
 */
  private def lexNumber(column:Int, row:Int,  arr:Array[String] = fileReader.sourceLines):
  (Either[Token,PreAndErrorToken],Int) = {
    var r: Int = row + 1
    var substring: String = arr(column).substring(row, r)
    while (r-1 < arr(column).length && arr(column).substring(row, r).matches("[0-9]+[.]?[0-9]*")) {
      substring= arr(column).substring(row, r)
      r = r + 1
    }
    val locStart:Location = Location(column, row)
    val pos:Int = r-1
    if(pos < arr(column).length && !(arr(column)(pos).isWhitespace | otherKnownSymbol(arr(column)(pos)))) {
      lexNumberComplexMatch(column, row, arr, substring, locStart, pos)
    } else if(substring.matches("[0-9]+")){
      val locEnd:Location = Location(column, pos)
      (Left(I32(substring.toInt, Span(fileReader,locStart, locEnd))),pos)
    }else{
      val locEnd:Location = Location(column, pos)
      (Left(F32(substring.toFloat, Span(fileReader,locStart, locEnd))),pos)
    }
  }

  private def lexNatNumber(column:Int, row:Int,  arr:Array[String] = fileReader.sourceLines):(NatNumber,Int) = {
    var r: Int = row + 1
    var substring: String = arr(column).substring(row, r)
    while (r-1 < arr(column).length && arr(column).substring(row, r).matches("[0-9]+")) {
      substring= arr(column).substring(row, r)
      r = r + 1
    }
    val loc = Location(column, r)
    (NatNumber(substring.toInt, new Span(fileReader, loc)),r-1)
  }

  /*
  requirement: substring has the form: [0-9]+.?[0-9]*
  arr(column)(pos).isWhitespace | otherKnownSymbol(arr(column)(pos))
   */
private def lexNumberComplexMatch(column: Int, row: Int,  arr: Array[String], substring:String,
                                  locStart:Location, pos:Int):(Either[Token,PreAndErrorToken],Int) =
  arr(column)(pos) match {
  case 'I' => {
    if (substring.matches("[0-9]+")) {
      if (arr(column).substring(pos, pos + 2) == "I8") {
        val locEnd: Location = Location(column, pos + 2)
        (Left(I8(substring.toInt.toShort, Span(fileReader, locStart, locEnd))),pos+2)
      } else if (arr(column).substring(pos, pos + 3) == "I32") {
        val locEnd: Location = Location(column, pos + 3)
        (Left(I32(substring.toInt, Span(fileReader, locStart, locEnd))),pos + 3)
      } else {
        val a = createIdentifierBeginsWithDigits(column, row, pos, locStart)
        (Right(a._1),a._2)
      }
    } else { //it has an '.' in it and because of that it is not an accepted Integer-Type
      if (arr(column).substring(pos, pos + 2) == "I8") {
        val locEnd: Location = Location(column, pos + 2)
        (Right(F32DeclaredAsI8(substring.toFloat, Span(fileReader, locStart, locEnd), fileReader)),pos + 2)
      } else if (arr(column).substring(pos, pos + 3) == "I32") {
        val locEnd: Location = Location(column, pos + 3)
        (Right(F32DeclaredAsI32(substring.toFloat, Span(fileReader, locStart, locEnd), fileReader)),pos + 3)
      } else {
        val a= createIdentifierBeginsWithAF32Number(column, row, pos, locStart)
        (Right(a._1),a._2)
      }
    }
  }
  case 'F' => {
    //Todo: should there be an extra warning for if (substring.matches("[0-9]+")) {
    if (arr(column).substring(pos, pos + 3) == "F32") {
      val locEnd: Location = Location(column, pos + 3)
      (Left(F32(substring.toFloat, Span(fileReader, locStart, locEnd))),pos + 3)
    }else if (arr(column).substring(pos, pos + 3) == "F64") {
      val locEnd: Location = Location(column, pos + 3)
      (Left(F64(substring.toDouble, Span(fileReader, locStart, locEnd))),pos + 3)
    } else{
      val a = createIdentifierBeginsWithAF32Number(column, row, pos, locStart)
      (Right(a._1),a._2)
    }
  }
  case 'N' => {
    val loc = Location(column, pos+1)
    (Left(NatNumber(substring.toInt, new Span(fileReader, loc))),pos+1)
  }
  case a => {
    if(a.isLetter){
      val a= createIdentifierBeginsWithAF32Number(column, row, pos, locStart)
      (Right(a._1),a._2)
    }else if(a == '_'){
      if (substring.matches("[0-9]+")) {//it has not an '.' in it and because of that it is I32
        val a= createIdentifierBeginsWithDigits(column, row, pos, locStart)
        (Right(a._1),a._2)
      }else{//it has an '.' in it and because of that it is F32
        val a = createIdentifierBeginsWithAF32Number(column, row, pos, locStart)
        (Right(a._1),a._2)
      }
    }else{ //it is not an whitespace or an other known symbol!
      val locEnd: Location = Location(column, pos + 1)
      (Right(NumberWithUnknownSymbol(a, arr(column).substring(row, pos + 1),
        Span(fileReader, locStart, locEnd), fileReader)),pos + 1)
    }
  }
}

/*
requirements:   arr(column).substring(row, pos) has the from [0-9]+
 */
private def createIdentifierBeginsWithDigits(column:Int,row:Int,   pos:Int, locStart:Location,
                                             arr:Array[String] = fileReader.sourceLines):(PreAndErrorToken,Int) ={
  val locEnd:Location = Location(column, pos+1)
  (IdentifierBeginsWithDigits(arr(column).substring(row, pos+1), Span(fileReader, locStart, locEnd), fileReader),pos+1)
}

/*
requirements:   arr(column).substring(row, pos) has the from [0-9]+.?[0-9]*
 */
private def createIdentifierBeginsWithAF32Number(column:Int,row:Int,  pos:Int, locStart:Location,
                                                 arr:Array[String] = fileReader.sourceLines):(PreAndErrorToken,Int) ={
  val locEnd:Location = Location(column, pos+1)
  (IdentifierBeginsWithAF32Number(arr(column).substring(row, pos+1), Span(fileReader, locStart, locEnd), fileReader),
    pos+1)
}

  /*
  is the Symbol '(', ')',  '\', ':', '-', '+', '*', '/', '%' , '>', '<', '=' or '!'
  It is not relevant here, that "asdf=3234" is not allowed,
  it is only relevant here, that '=' is a known symbol
   */
  def otherKnownSymbol(c:Char): Boolean = {
    val set:Set[Char] = Set('(', ')', '\\', ':', '-', '+', '*', '/', '%' , '>', '<', '=' ,'!', ',', '.', '[', ']')
    set(c) //set.contains(c)
  }

  /*
  it is only relevant here, that '=' is a known symbol
   */
  def isBinaryOperatorSymbol(c:Char): Boolean = {
    val set:Set[Char] = Set('-', '+', '*', '/', '%' , '>', '<', '=')
    set(c) //set.contains(c)
  }


  /*
we expect to see an Arrow
requirements:  no whitespace at arr(column)(row)

two steps
*/
//  private def lexArrow(column:Int, row: Int, arr: Array[String]= fileReader.sourceLines):Either[Token,PreAndErrorToken]= {
//    lexDeporNormalArrow(column,row,arr, "->") match {
//      case Left(Arrow(span)) => {
//        Left(Arrow(span))
//      }
//      case Left(DepArrow(span)) => {
//        Right(NotExpectedToken("->", "=>", span, fileReader))
//      }
//      case Left(a) => {
//        Right(NotExpectedToken("->", a.toString, a.s, fileReader))
//      }
//      case Right(e) => Right(e)
//    }
//  }

  private def lexDepArrow(column:Int, row: Int, arr: Array[String]= fileReader.sourceLines):
  Either[Token,PreAndErrorToken]= {
    lexDeporNormalArrow(column,row,arr, "=>") match {
      case Left(DepArrow(span)) => {
        Left(DepArrow(span))
      }
      case Left(Arrow(span)) => {
        return Right(NotExpectedToken("=>", "->", span, fileReader))
      }
      case Left(a) => {
        Right(NotExpectedToken("=>", a.toString, a.s, fileReader))
      }
      case Right(e) => Right(e)
    }
    }

}