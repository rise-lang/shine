package parser.lexer

import java.io._

import OpType.{BinOpType, UnaryOpType}


//alles muss in ein Try gemappt werden
//Try{ file= openFile(); parse(file);}catch{...}finally{file.close}

/*
reads the File and saves the name and the content of the file as an Array of Strings
 */
case class FileReader(fileName: String) {
  require(fileName != null, "FileName should not be null")
  require(fileName.endsWith(".rise"), "not a RISE file") //if not, it's not a RISE file

  val sourceLines:Array[String] = readFile(fileName)
  /*
  returns the Content of the File with name fileName in
  an String-Array

  requirement: FileName is not null //TODO: more stability
   */
  private def readFile(fileName: String): Array[String] ={
    //create File
    val f:File = try {
      new File(fileName)
    }catch {
      case ex: NullPointerException => throw readFileExNullPointerException(fileName,ex) //Error File //Todo: Nullpointer entfernen
    }
    //requirements of the File to be read
    if(!f.exists()){
      //file does not exist
      val s: String= "The File " + fileName + " does not exist!"
      throw new IllegalArgumentException(s)
    }
    if(!f.isFile){
      //file is not a File
      if(f.isDirectory){
        //file is a directory
        val s: String= "The File " + fileName + " is a directory and not a File!"
        throw new IllegalArgumentException(s)
      }else{
        //file is not a File and not a directory //Todo: is this a possible case? <- It should't be possible
        val s: String= "The File " + fileName + " is neither a File nor a directory"
        throw new IllegalArgumentException(s)
      }
    }
    if(!f.canRead){
      //file can not be read
      val s: String= "The File " + fileName + " is not readable!"
      throw new IllegalArgumentException(s)
    }
    val input:FileInputStream = try{ //Todo: Schau dir die Try-Klasse von Scala an, weil so kännten unerwartete Fehler auftreten und dann schließen wir ihn nicht
      //read the File with the Java-InputStream
      new FileInputStream(f)
    }catch {
            //both errors should not be able to appear
      case ex: FileNotFoundException => throw readFileExFileNotFound(fileName, ex) //Error FileInputStream
      case ex: SecurityException => throw readFileExSecurityException(fileName, ex) //Error FileInputStream
    }/* finally {
      val b:Boolean = f.delete() //deletes the File (not only the scala object but the whole File)
      if(!b){
        //File is not deleted
        val s: String= "\n\n!!!The File " + fileName + " is not deleted!!!\n\n"
        println(s)
      }
    } */

    val arr:Array[String] = try {
      //Todo:Try with resources
      val arr:Array[String] = scala.io.Source.fromInputStream(input).getLines().toArray
      arr
    }finally{
        input.close //close the InputStream (very important) //throw the IOException if occours
    }
    arr //return the Content of the File
  }

  /*
  returns a List of Files which have similar names
   */
  /*private def isSimilarFile(fileName: String):Array[File]={
    //in the fileName we extract, if contains, the path in which it should be
    val pathName:String = if(fileName.contains('/')){
      fileName.substring(0,fileName.lastIndexWhere(p => p =='/'))
    }else{
      "."
    }
    //Todo: check if path exists
    //takes all Files in the same path
    val filesHere:Array[File] = listAllFilesInDirectory(pathName)
    //returns all Files which FileNames are less than 5 chars different than fileName //https://alvinalexander.com/scala/scala-strings-differences-intersection-distinct-characters/
    filesHere.filter(p => p.getName.toSeq.diff(fileName.toSeq).length<5)
  }*/
  /*
  list all Files in the Directory
   */
/*  private def listAllFilesInDirectory(pathname: String = "."):Array[File]={
    val filesHere:Array[File] = (new java.io.File(pathname)).listFiles()
    filesHere.filter(file => file.isFile && file.getName().endsWith(".rise"))
  }*/

  // if the file does not exist, is a directory rather than a regular file, or for some other reason cannot be opened for reading.
  private def readFileExFileNotFound(fileName: String, exception: Exception):Exception ={
    exception
  }
  // if a security manager exists and its checkRead method denies read access to the file.
  private def readFileExSecurityException(fileName: String, exception: Exception):Exception ={
    exception
  }
  //If the pathname argument is null
  private def readFileExNullPointerException(fileName: String, exception: Exception):Exception ={
    exception
  }


  override def toString = "fileName: '" + fileName + "'; fileContent: {\n"+ sourceLines.mkString + "\n}"

}

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
    throw new Exception(message)
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
final case class IdentifierWithNotAllowedSymbol(unknownSymbol:Char, str:String, span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader){
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "The Identifier '" + str + "' has an unknown Symbol '" + unknownSymbol + "' at "+ span.toString()
}
final case class IdentifierBeginsWithUnderscore(span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader){
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "Here begins a Identifier with an Underscore at " + span.toString
}
final case class IdentifierBeginsWithDigits(str:String, span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader){
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "The Identifier '"+ str+ "' begins with an Digits/a Number at " + span.toString
}
final case class NumberWithUnknownSymbol(unknownSymbol: Char, str:String, span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader){
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "The Number '" + str + "' has an unknown Symbol '" + unknownSymbol + "' at "+ span.toString()
}
final case class F32DeclaredAsI8(number:Float, span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader){
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "The number '" + number + "' is an F32 Number but it is declared as I8 at "+ span.toString()
}
final case class F32DeclaredAsI32(number:Float, span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader){
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "The number '" + number + "' is an F32 Number but it is declared as I32 at "+ span.toString()
}
final case class IdentifierBeginsWithAF32Number(str:String, span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader){
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "The Identifier '"+ str+ "' begins with a F32 Number at " + span.toString
}
final case class NumberWithUnderscore(str:String, span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader){
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "The Number '"+ str+ "' has an underscore in it at " + span.toString
}
//Todo: anstatt missing expectedToken(expectedToken:String, givenToken:String, span:Span, message:String) und Issue erstellen
  final case class NotExpectedToken(expectedToken:String, givenToken:String, span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader){
    require(span.begin.column == span.end.column, "not in one column")
    override def toString = "It is an '"+ expectedToken +"' expected. The Lexeme '" + givenToken + "' is not an '"+ expectedToken+ "'!"
  }

  final case class ToShortToBeThisToken(expectedLength:Int, token:String, span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader){
    require(expectedLength >0, "expectedLength is less or equal to zero")
    require(span.begin.column == span.end.column, "not in one column")
    override def toString = "the given length is less than "+ expectedLength +" for "+ token +"!"
  }
final case class NOTanBinOperator(symbol: String, span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader) {
  require(span.begin == span.end, "span.begin is unequal to span.end")
  override def toString = "The Symbol '" + symbol + "' is not an Operator"
}
final case class NOTanUnOperator(symbol: String, span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader) {
  require(span.begin == span.end, "span.begin is unequal to span.end")
  override def toString = "The Symbol '" + symbol + "' is not an Operator"
}

final case class UnknownType(str: String, span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader) {
  require(span.begin.column == span.end.column, "not in one column")
  override def toString = "The Type '" + str + "' is not an accepted Type in RISE"
}

abstract sealed class ThisTokenShouldntBeHere(token: Token, span:Span, fileReader:FileReader) extends PreAndErrorToken(span, fileReader) {
  require(token.s.begin.column == token.s.end.column, "not in one column")
  override def toString = "The Token '" + token.toString + "' was not here expected"
}

final case class ThisTokenShouldntBeHereExpectedArrowOrDots(token: Token, span:Span, fileReader:FileReader) extends ThisTokenShouldntBeHere(token, span, fileReader)


final case class LeftBraceMissing(span:Span, fileReader: FileReader) extends PreAndErrorToken(span, fileReader){
  override def toString = "Left Brace is missing!"
}
final case class RightBraceMissing(span:Span, fileReader: FileReader) extends PreAndErrorToken(span, fileReader){
  override def toString = "Right Brace is missing!"
}
/*
this recognizes the Lexeme in the File which represents the right Token
 */
case class RecognizeLexeme(fileReader: FileReader){
  val tokens:List[Either[PreAndErrorToken, Token]] = lexer().reverse //lexe the contents of the File


  private def lexer():List[Either[PreAndErrorToken, Token]] = {
    val list:List[Either[PreAndErrorToken, Token]] = lexerLambda(0,0, Nil)._1
    //are here enought RightBraces
    //enough leftBraces are controlled in lexerExpression and shouldn't happen
    val arr: Array[String]= fileReader.sourceLines
    val lB: Int = list.count(a => a match {
      case Right(LBrace(_)) => true
      case b => false
    } )
    val rB: Int = list.count(a => a match {
      case Right(RBrace(_)) => true
      case b => false
    } )
    if(lB>rB){ //for example "( .. ( ... )"
      val loc:Location = Location(arr.length-1, arr(arr.length-1).length)
      val ex = RightBraceMissing(new Span(fileReader, loc), fileReader)
      ex.throwException()
    }
    list
  }
  /*
  it lexes a Lambda

  (List[Either[PreAndErrorToken, Token]],column,row)
   */
private def lexerLambda(oldColumn:Int, oldRow:Int, l:List[Either[PreAndErrorToken, Token]]):(List[Either[PreAndErrorToken, Token]],Int,Int) = {
    val arr: Array[String]= fileReader.sourceLines
    var row = oldRow
    var column = oldColumn
    require(row>=0, "row is not allowed to be negative")
    require(column>=0, "column is not allowed to be negative")
    require(arr.length > column, "array does not have so much columns")
    require(arr(column).length > row, "arr(column) has less than row chars")

    //in this list we add all
    var list:List[Either[PreAndErrorToken, Token]] = l

    //ignore whitespaces
    skipWhitespace(column, row, arr) match {
      case (c,r) =>{
        column = c
        row = r
      }
    }

    //only one step
  lexBackslash(column, row) match {
    case Right(a) => {
      row = row +1
      list= list.::(Right(a))
    }
    case Left(a) => {
      a.throwException()
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
    case (Right(a), r) => {
      row = r
      list=list.::(Right(a))
    }
    case (Left(a), _) => {
      a.throwException()
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
    case Right(Dots(span)) => {
      row = row +1
      list= list.::(Right(Dots(span)))
    }
    case Right(Arrow(span)) => {
      row = row +2
      list= list.::(Right(Arrow(span)))
    }
    case Right(a) => {
      val loc:Location = Location(column, row) //endLocation is equal to startLocation
      NotExpectedToken("Arrow or Dots", a.toString, new Span(fileReader, loc), fileReader).throwException()
    }
    case Left(a) => {
      a.throwException()
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
    case Right(Dots(_)) => {
      // :Typ ->
      lexType(column,row) match {
        case (Right(a), r) => {
          row = r
          list=list.::(Right(a))
        }
        case (Left(a), _) => {
          a.throwException()
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
      lexArrow(column, row) match {
        case Right(a) => {
          row = row +2
          list= list.::(Right(a))
        }
        case Left(a) => {
          a.throwException()
        }
      }
    }
    case Right(Arrow(_)) => {
      //nothing to do
    }
    case Right(a) => {
      val loc:Location = Location(column, row) //endLocation is equal to startLocation
      ThisTokenShouldntBeHereExpectedArrowOrDots(a, new Span(fileReader, loc),fileReader).throwException()
    }
    case Left(a) => {
      //should not be happening
      a.throwException()
    }
  }

  lexerExpression(column, row,list)
}


  /*
    (List[Either[PreAndErrorToken, Token]],column,row)
   */
  private def lexerExpression(oldColumn:Int, oldRow:Int, l:List[Either[PreAndErrorToken, Token]]):(List[Either[PreAndErrorToken, Token]], Int, Int) = {
    val arr: Array[String]= fileReader.sourceLines
    var row = oldRow
    var column = oldColumn
    require(row >= 0, "row is not allowed to be negative")
    require(column >= 0, "column is not allowed to be negative")
    require(arr.length > column, "array does not have so much columns")
    require(arr(column).length > row, "arr(column) has less than row chars")

    var list = l

    //ignore whitespaces
        skipWhitespace(column, row) match {
      case (c,r) =>{
        column = c
        row = r
      }
    }

    arr(column)(row) match {
      case '\\' =>{
        lexerLambda(column,row,list) match {
          case (l, c,r) =>{
            column = c
            row = r
            list = l
          }
        }
      }
      case '(' => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        list = list.::(Right(LBrace(new Span(fileReader,loc))))
        row = row +1
        //ignore whitespaces
        skipWhitespace(column, row) match {
          case (c,r) =>{
            column = c
            row = r
          }
        }
        lexerExpression(column,row, list) match {
          case (l, c, r) =>{
            l(0) match {
              case Right(RBrace(_)) => {
                list = l
                column = c
                row = r
              }
              case Right(a) => { //last symbol not a RBrace => failure
                val beginLoc:Location = Location(column, row)
                val endLoc:Location = Location(c, r)
                val span:Span = Span(fileReader,beginLoc, endLoc)
                val ex = RightBraceMissing(span,fileReader)
                ex.throwException()
              }
              case Left(a) => {
                //should not be happening
                a.throwException()
              }
            }
          }
        }
      }
      case ')' => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        list = list.::(Right(RBrace(new Span(fileReader, loc))))
        row = row +1
        val lB: Int = list.count(a => a match {
          case Right(LBrace(_)) => true
          case b => false
        } )
        val rB: Int = list.count(a => a match {
          case Right(RBrace(_)) => true
          case b => false
        } )
        if(lB<rB){ //for example "( ... ) .. )"
          val ex = LeftBraceMissing(new Span(fileReader, loc), fileReader)
          ex.throwException()
        }
        return (list, column, row) //i have to return, because the LBrace wants to see a RBrace
      }
      case '!' => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        list = list.::(Right(UnOp(UnaryOpType.NOT,new Span(fileReader, loc))))
        row = row +1
        //ignore whitespaces
        skipWhitespace(column, row) match {
          case (c,r) =>{
            column = c
            row = r
          }
        }
        lexerExpression(column,row,list) match {
          case (l, c, r) =>{
            list = l
            column = c
            row = r
          }
        }
      }
      case '-' => {
        lexNEGOperator(column, row) match {
          case Right(a) => {
            row = row +1
            list = list.::(Right(a))
            //ignore whitespaces
            skipWhitespace(column, row) match {
              case (c,r) =>{
                column = c
                row = r
              }
            }
            lexerExpression(column,row,list) match {
              case (l, c, r) =>{
                list = l
                column = c
                row = r
              }
            }
          }
          case Left(a) => {
            a.throwException()
          }
        }
      }
      case a => {
        if(a.isDigit){
          //more than one step but column keeps the same
          lexNumber(column, row) match {
            case (Right(a), r) => {
              row = r
              list=list.::(Right(a))
            }
            case (Left(a), _) => {
              a.throwException()
            }
          }
        }else if(a.isLetter){
          //more than one step but column keeps the same
          lexIdentifier(column, row) match {
            case (Right(a), r) => {
              row = r
              list=list.::(Right(a))
            }
            case (Left(a), _) => {
              a.throwException()
            }
          }
        }else if(otherKnownSymbol(a)){
          val loc:Location = Location(column, row) //endLocation is equal to startLocation
          val ex = NotExpectedToken("an Identifier or a Number or \\ or a Brace or a UnOperator", ""+ a, new Span(fileReader, loc), fileReader)
          ex.throwException()
        }else{
          val loc:Location = Location(column, row) //endLocation is equal to startLocation
          val ex = UnknownSymbol(a, new Span(fileReader, loc), fileReader)
          ex.throwException()
        }
      }
    }

    //ignore whitespaces
    skipWhitespace(column, row) match {
      case (c,r) =>{
        column = c
        row = r
      }
    }

    //now only a BinOperator could be here or a second expression or nothing
    if(column >= arr.length || (column == arr.length-1 && row >= arr(column).length)){
      return (list, column, row) //end is reached
    }

    if(isBinaryOperatorSymbol(arr(column)(row))){ //it is a binary operator
      lexBinOperator(column, row) match {
        case Right(BinOp(BinOpType.EQ, span)) =>{
          row = row+2
          list=list.::(Right(BinOp(BinOpType.EQ, span)))
        }
        case (Right(a)) => {
          row = row +1
       //   println("\n\n"+ a.toString)
          list=list.::(Right(a))
        }
        case (Left(a)) => {
          a.throwException()
        }
      }
      return lexerExpression(column,row, list)
    }else{
      return lexerExpression(column,row, list)
    }
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
  /*
  we expect to see a Backslash
  requirements:  no whitespace at arr(column)(row)
   */
  private def lexBackslash(column:Int, row: Int, arr: Array[String]= fileReader.sourceLines):Either[PreAndErrorToken, Token]= {
    arr(column)(row) match {
      case '\\' => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        Right(Backslash(new Span(fileReader,loc)))
      }
      case a => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        Left(NotExpectedToken("\\", ""+ a, Span(fileReader,loc, loc), fileReader))
      }
    }
  }



  /*
we expect to see Dots or an Arrow
requirements:  no whitespace at arr(column)(row)
 */
  private def lexDotsOrArrow(column:Int, row: Int, arr: Array[String]= fileReader.sourceLines):Either[PreAndErrorToken, Token]= {
    arr(column)(row) match {
      case ':' => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        Right(Dots(new Span(fileReader,loc)))
      }
      case '-' => {
        if(arr(column).length <= row +1){
          val loc:Location = Location(column, row) //endLocation is equal to startLocation
          Left(ToShortToBeThisToken(2, "->", Span(fileReader,loc, loc), fileReader))
        }else{
          val beginLoc:Location = Location(column, row)
          val endLoc:Location = Location(column, row+1)
          val span:Span = Span(fileReader,beginLoc, endLoc)
          arr(column).substring(row, row+2) match {
            case "->" => {
              Right(Arrow(span))
            }
            case a => {
              Left(NotExpectedToken("->", a, span, fileReader))
            }
          }
        }
      }
      case a => {
        val loc:Location = Location(column, row) //endLocation is equal to startLocation
        Left(NotExpectedToken(":", ""+ a, Span(fileReader,loc, loc), fileReader))
      }
    }
  }


  /*
  this lexes if it is an unary operator

  only one step
   */
  /*private def lexUnOperator(column:Int, row:Int, arr: Array[String] = fileReader.sourceLines):Either[PreAndErrorToken, Token]= {
    arr(column)(row) match {
      case '-' => {
        if (arr(column).length <= row + 1 || arr(column).substring(row, row + 2) != "->") { // -
          val loc: Location = Location(column, row) //endLocation is equal to startLocation
          Right(Lexeme.UnOp(OpType.UnaryOpType.NEG, new Span(fileReader, loc))) //it is possible that it is OpType.UnaryType.Neg, but here it is not decideable
          //so we save it at first as NegSign so, that we can decide later
        } else { // ->
          val locStart: Location = Location(column, row)
          val locEnd: Location = Location(column, row + 1)
          Left(NOTanUnOperator("->", Span(fileReader, locStart, locEnd), fileReader))
        }
      }
      case '!' => { // !
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        Right(Lexeme.UnOp(OpType.UnaryOpType.NOT, new Span(fileReader, loc)))
      }
      case a => {
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        Left(NOTanUnOperator("" + a  , new Span(fileReader, loc),fileReader))
      }
    }
  }*/

  private def lexNEGOperator(column:Int, row:Int, arr: Array[String] = fileReader.sourceLines):Either[PreAndErrorToken, Token]= {
    arr(column)(row) match {
      case '-' => {
        if (arr(column).length <= row + 1 || arr(column).substring(row, row + 2) != "->") { // -
          val loc: Location = Location(column, row) //endLocation is equal to startLocation
          Right(UnOp(OpType.UnaryOpType.NEG, new Span(fileReader, loc))) //it is possible that it is OpType.UnaryType.Neg, but here it is not decideable
          //so we save it at first as NegSign so, that we can decide later
        } else { // ->
          val locStart: Location = Location(column, row)
          val locEnd: Location = Location(column, row + 1)
          Left(NotExpectedToken("-", "->", Span(fileReader, locStart, locEnd), fileReader))
        }
      }
      case a => {
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        Left(NotExpectedToken("-", ""+a, new Span(fileReader, loc), fileReader))
      }
    }
  }

/*
this lexes if it is an binary operator

if '==' then two steps else only one step
 */
  private def lexBinOperator(column:Int, row:Int, arr: Array[String] = fileReader.sourceLines):Either[PreAndErrorToken, Token]= {
    arr(column)(row) match {
      case '-' => {
        if (arr(column).length <= row + 1 || arr(column).substring(row, row + 2) != "->") { // -
          val loc: Location = Location(column, row) //endLocation is equal to startLocation
          Right(BinOp(OpType.BinOpType.SUB, new Span(fileReader, loc))) //it is possible that it is OpType.UnaryType.Neg, but here it is not decideable
          //so we save it at first as NegSign so, that we can decide later
        } else { // ->
          val locStart: Location = Location(column, row)
          val locEnd: Location = Location(column, row + 1)
          Left(NOTanBinOperator("->", Span(fileReader, locStart, locEnd), fileReader))
        }
      }
      case '+' => { // +
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        Right(BinOp(OpType.BinOpType.ADD, new Span(fileReader, loc)))
      }
      case '*' => { // *
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        Right(BinOp(OpType.BinOpType.MUL, new Span(fileReader, loc)))
      }
      case '/' => { // /
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        Right(BinOp(OpType.BinOpType.DIV, new Span(fileReader, loc)))
      }
      case '%' => { // %
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        Right(BinOp(OpType.BinOpType.MOD, new Span(fileReader, loc)))
      }
      case '<' => { // <
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        Right(BinOp(OpType.BinOpType.LT, new Span(fileReader, loc)))
      }
      case '>' => { // >
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        Right(BinOp(OpType.BinOpType.GT, new Span(fileReader, loc)))
      }
      case '=' => {
        if (arr(column).length <= row + 1 || arr(column).substring(row, row + 2) != "==") {
          val loc: Location = Location(column, row)
          Left(OnlyOneEqualSign(new Span(fileReader, loc), fileReader))
        } else { // ==
          val beginLoc: Location = Location(column, row)
          val endLoc: Location = Location(column, row + 1)
          Right(BinOp(OpType.BinOpType.EQ, Span(fileReader, beginLoc, endLoc)))
        }
      }
      case a => {
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        Left(NOTanBinOperator("" + a  , new Span(fileReader, loc), fileReader))
      }
    }
  }


  /*
  for example "I32", "Identifier"
 */
  private def lexType(column:Int, row:Int,  arr:Array[String] = fileReader.sourceLines):(Either[PreAndErrorToken,Token],Int) = {
    var r: Int = row + 1
    var substring: String = arr(column).substring(row, r)
    while (r-1 < arr(column).length && arr(column).substring(row, r).matches("[a-zA-Z][a-zA-Z0-9_]*")) {
      substring= arr(column).substring(row, r)
      r = r + 1
    }
    val locStart:Location = Location(column, row)
    val pos:Int = r-1
    if(pos < arr(column).length && !(arr(column)(pos).isWhitespace | otherKnownSymbol(arr(column)(pos)))){
      val locEnd:Location = Location(column, pos+1)
      (Left(UnknownType(substring, Span(fileReader,locStart, locEnd), fileReader)),pos+1)
    }else{
      val locEnd:Location = Location(column, pos)
      //different Types in RISE //Todo: not completed yet
      substring match {
        case "bool" => (Right(parser.lexer.Type(BoolType(), Span(fileReader,locStart, locEnd))),pos)
        case "Bool" => (Right(parser.lexer.Type(BoolType(), Span(fileReader,locStart, locEnd))),pos)
        case "I8"   => (Right(parser.lexer.Type(ShortTyp(), Span(fileReader,locStart, locEnd))),pos)
        case "I32"  => (Right(parser.lexer.Type(IntTyp(), Span(fileReader,locStart, locEnd))),pos)
        case "F32"  => (Right(parser.lexer.Type(FloatTyp(), Span(fileReader,locStart, locEnd))),pos)
        case "F64"  => (Right(parser.lexer.Type(DoubleType(), Span(fileReader,locStart, locEnd))),pos)
        case "nat"  => (Right(parser.lexer.Type(NatTyp(), Span(fileReader,locStart, locEnd))),pos)
        //unknown Type
        case a => (Left(UnknownType(substring, Span(fileReader,locStart, locEnd), fileReader)),pos)
      }
    }
  }
  
  /*
    for example "split", "go", "def", "while"

   */
  private def lexIdentifier( column:Int, row:Int, arr:Array[String] = fileReader.sourceLines):(Either[PreAndErrorToken,Token],Int) = {
    var r: Int = row + 1
    var substring: String = arr(column).substring(row, r)
    while (r-1 < arr(column).length && arr(column).substring(row, r).matches("[a-zA-Z][a-zA-Z0-9_]*")) {
      substring= arr(column).substring(row, r)
      r = r + 1
    }
    val locStart:Location = Location(column, row)
    val pos:Int = r-1
    if(pos < arr(column).length && !(arr(column)(pos).isWhitespace | otherKnownSymbol(arr(column)(pos)))){
      val locEnd:Location = Location(column, pos+1)
      (Left(IdentifierWithNotAllowedSymbol(arr(column)(pos), arr(column).substring(row, pos+1), Span(fileReader,locStart, locEnd), fileReader)), pos+1)
    }else{
      val locEnd:Location = Location(column, pos)
      (Right(Identifier(IdentifierType(substring), Span(fileReader,locStart, locEnd))), pos)
    }
  }

  /*
  span.end.row - span.begin.row is the number of steps
 */
  private def lexNumber(column:Int, row:Int,  arr:Array[String] = fileReader.sourceLines):(Either[PreAndErrorToken,Token],Int) = {
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
      (Right(I32(substring.toInt, Span(fileReader,locStart, locEnd))),pos)
    }else{
      val locEnd:Location = Location(column, pos)
      (Right(F32(substring.toFloat, Span(fileReader,locStart, locEnd))),pos)
    }
  }

  /*
  requirement: substring has the form: [0-9]+.?[0-9]*
  arr(column)(pos).isWhitespace | otherKnownSymbol(arr(column)(pos))
   */
private def lexNumberComplexMatch(column: Int, row: Int,  arr: Array[String], substring:String, locStart:Location, pos:Int):(Either[PreAndErrorToken,Token],Int) = arr(column)(pos) match {
  case 'I' => {
    if (substring.matches("[0-9]+")) {
      if (arr(column).substring(pos, pos + 2) == "I8") {
        val locEnd: Location = Location(column, pos + 2)
        (Right(I8(substring.toInt.toShort, Span(fileReader, locStart, locEnd))),pos+2)
      } else if (arr(column).substring(pos, pos + 3) == "I32") {
        val locEnd: Location = Location(column, pos + 3)
        (Right(I32(substring.toInt, Span(fileReader, locStart, locEnd))),pos + 3)
      } else {
        val a = createIdentifierBeginsWithDigits(column, row, pos, locStart)
        (Left(a._1),a._2)
      }
    } else { //it has an '.' in it and because of that it is not an accepted Integer-Type
      if (arr(column).substring(pos, pos + 2) == "I8") {
        val locEnd: Location = Location(column, pos + 2)
        (Left(F32DeclaredAsI8(substring.toFloat, Span(fileReader, locStart, locEnd), fileReader)),pos + 2)
      } else if (arr(column).substring(pos, pos + 3) == "I32") {
        val locEnd: Location = Location(column, pos + 3)
        (Left(F32DeclaredAsI32(substring.toFloat, Span(fileReader, locStart, locEnd), fileReader)),pos + 3)
      } else {
        val a= createIdentifierBeginsWithAF32Number(column, row, pos, locStart)
        (Left(a._1),a._2)
      }
    }
  }
  case 'F' => {
    //Todo: should there be an extra warning for if (substring.matches("[0-9]+")) {
    if (arr(column).substring(pos, pos + 3) == "F32") {
      val locEnd: Location = Location(column, pos + 3)
      (Right(F32(substring.toFloat, Span(fileReader, locStart, locEnd))),pos + 3)
    }else if (arr(column).substring(pos, pos + 3) == "F64") {
      val locEnd: Location = Location(column, pos + 3)
      (Right(F64(substring.toDouble, Span(fileReader, locStart, locEnd))),pos + 3)
    } else{
      val a = createIdentifierBeginsWithAF32Number(column, row, pos, locStart)
      (Left(a._1),a._2)
    }
  }
  case a => {
    if(a.isLetter){
      val a= createIdentifierBeginsWithAF32Number(column, row, pos, locStart)
      (Left(a._1),a._2)
    }else if(a == '_'){
      if (substring.matches("[0-9]+")) {//it has not an '.' in it and because of that it is I32
        val a= createIdentifierBeginsWithDigits(column, row, pos, locStart)
        (Left(a._1),a._2)
      }else{//it has an '.' in it and because of that it is F32
        val a = createIdentifierBeginsWithAF32Number(column, row, pos, locStart)
        (Left(a._1),a._2)
      }
    }else{ //it is not an whitespace or an other known symbol!
      val locEnd: Location = Location(column, pos + 1)
      (Left(NumberWithUnknownSymbol(a, arr(column).substring(row, pos + 1), Span(fileReader, locStart, locEnd), fileReader)),pos + 1)
    }
  }
}




  //Todo: "234asdf" gives only "234a" back but not "234asdf", change the code so it works
/*
requirements:   arr(column).substring(row, pos) has the from [0-9]+
 */
private def createIdentifierBeginsWithDigits(column:Int,row:Int,   pos:Int, locStart:Location, arr:Array[String] = fileReader.sourceLines):(PreAndErrorToken,Int) ={
  val locEnd:Location = Location(column, pos+1)
  (IdentifierBeginsWithDigits(arr(column).substring(row, pos+1), Span(fileReader, locStart, locEnd), fileReader),pos+1)
}

/*
requirements:   arr(column).substring(row, pos) has the from [0-9]+.?[0-9]*
 */
private def createIdentifierBeginsWithAF32Number(column:Int,row:Int,  pos:Int, locStart:Location, arr:Array[String] = fileReader.sourceLines):(PreAndErrorToken,Int) ={
  val locEnd:Location = Location(column, pos+1)
  (IdentifierBeginsWithAF32Number(arr(column).substring(row, pos+1), Span(fileReader, locStart, locEnd), fileReader),pos+1)
}


  /*
  recognizes the Tokens in one String
   */
  /*def recognizeTokens(row: Int, str:String) ={
    require(row>=0, "row is negative")
    require(!str.isEmpty, "the String is empty")



  }*/

  /*
  is the Symbol '(', ')',  '\', ':', '-', '+', '*', '/', '%' , '>', '<', '=' or '!'
  It is not relevant here, that "asdf=3234" is not allowed,
  it is only relevant here, that '=' is a known symbol
   */
  def otherKnownSymbol(c:Char): Boolean = {
    val set:Set[Char] = Set('(', ')', '\\', ':', '-', '+', '*', '/', '%' , '>', '<', '=' ,'!')
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
  private def lexArrow(column:Int, row: Int, arr: Array[String]= fileReader.sourceLines):Either[PreAndErrorToken, Token]= {
    if(arr(column).length <= row +1){
      val loc:Location = Location(column, row) //endLocation is equal to startLocation
      Left(ToShortToBeThisToken(2, "->", Span(fileReader,loc, loc), fileReader))
    }else{
      val beginLoc:Location = Location(column, row)
      val endLoc:Location = Location(column, row+1)
      val span:Span = Span(fileReader,beginLoc, endLoc)
      arr(column).substring(row, row+2) match {
        case "->" => {
          Right(Arrow(span))
        }
        case a => {
          Left(NotExpectedToken("->", a, span, fileReader))
        }
      }
    }
  }


  //______________________________________________________________________
  //everything under here is not used


  /*
we expect to see Dots
requirements:  no whitespace at arr(column)(row)
 */
  /*private def lexDots(column:Int, row: Int, arr: Array[String]= fileReader.sourceLines):Either[PreAndErrorToken, Token]= {
    arr(column)(row) match {
      case ':' => {
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        Right(Lexeme.Backslash(new Span(fileReader, loc)))
      }
      case a => {
        val loc: Location = Location(column, row) //endLocation is equal to startLocation
        Left(NotExpectedToken(":", "" + a, Span(fileReader, loc, loc), fileReader))
      }
    }
  }*/

}