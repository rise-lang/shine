package parser.lexer

import shine.DPIA.Phrases.{Lambda, Phrase, Identifier => Id}
import shine.DPIA.Types.{DataType, ExpType, PhraseType, ScalarType, f32, f64, i32, i8}

abstract sealed class SyntacticErrors(pos:Int, tokens:List[Either[PreAndErrorToken, Token]]) extends Throwable{
  require(pos >=0, "pos is less than zero")
  require(pos<tokens.length)
//  def throwException():Unit ={
//    val message:String = this.toString
//    throw new Exception(message)
//  }
}
final case class NoPreAndErrorTokenIsExpected(pos:Int, tokens:List[Either[PreAndErrorToken, Token]]) extends SyntacticErrors(pos, tokens){
  override def toString = "at postion " + pos + " is not PreAndErrorToken '"+ tokens(pos) +"' expected!\n"+ tokens.toString()
}
final case class WrongToken(pos:Int, tokens:List[Either[PreAndErrorToken, Token]], expectedToken:String) extends SyntacticErrors(pos, tokens){
  override def toString = "at postion " + pos + " is "+ expectedToken + " expected, but found '"+ tokens(pos) +"'!\n"+ tokens.toString()
}

case class parse(lexer:RecognizeLexeme){
  private val fileReader:FileReader = lexer.fileReader
  private val tokens:List[Either[PreAndErrorToken, Token]] = lexer.tokens
   val shineLambda = createSyntaxTree()
    private def checkBracesNumber():Unit = {
      val list:List[Either[PreAndErrorToken, Token]] = tokens
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
      }else if(lB<rB){ //for example "( .. ) ... )"
        val loc:Location = Location(arr.length-1, arr(arr.length-1).length)
        val ex = LeftBraceMissing(new Span(fileReader, loc), fileReader)
        ex.throwException()
      }
    }


  /*
  is the whole Syntax-Tree.
  the syntax-Tree has on top an Lambda-Expression
   */
    private def createShineExpression():Lambda[PhraseType, PhraseType]= {
      var pos: Int = 0 //beginningPosition
      require(isBacklash(pos), s"on position $pos is no Backslash!")
      //We are at the beginning of the SyntaxTree and a Type has to be defined
      require(isDots(pos+2), s"on position $pos are no Dots!")
      require(isArrow(pos+4), s"on position $pos is no Arrow!")
      checkBracesNumber() //check if the Braces Number of left and right Braces is equal
      val identifierType = giveIdentifierType(pos+3)._1 //type of the Identifier
      val identifierName:String = giveIdentifierName(pos+1)._1 //the name of the Identifier
      //creates the Identifier (first PhraseType) and the expression after the Arrow(second PhraseType)
      val identifier= Id[ScalarType](identifierName, identifierType)
      val exp = createExpression(pos+5, identifier)
      val lambda= Lambda[PhraseType, ExpType](identifier, exp)
      lambda
   }

  /*
  create an syntaxTree for an expression, which begins at postion pos
   */
  private def createExpression(pos: Int,oldIdentifier:Id[ScalarType]):Phrase[ExpType]={
    val identifierName:String = giveIdentifierName(pos)._1
    require(identifierName==oldIdentifier.name, "the Identifier is not declared yet")
    return oldIdentifier

  }



  /*
give me the StringName of the Identifier in the Position in pos
no PreAndErrorToken is expected
*/
  private def giveIdentifierName(pos:Int):(String, Span)={
    tokens(pos) match {
      case Right(Identifier(identifierType,span))=> (identifierType.identifier, span)
      case Right(a) => throw WrongToken(pos, tokens, "Identifier")
      case Left(a) => throw NoPreAndErrorTokenIsExpected(pos, tokens)
    }
  }

  /*
give me the StringName of the Identifier in the Position in pos
no PreAndErrorToken is expected
*/
  private def giveIdentifierType(pos:Int):(DataType, Span)={
    tokens(pos) match {
      case Right(Type(typType, span))=> typType match {
        case ShortTyp() => (i8, span)
        case IntTyp() => (i32, span)
        case FloatTyp() => (f32, span)
        case DoubleType() => (f64, span)
      }
      case Right(a) => throw WrongToken(pos, tokens, "Identifier")
      case Left(a) => throw NoPreAndErrorTokenIsExpected(pos, tokens)
    }
  }

  /*
give me the Token in Position pos
no PreAndErrorToken is expected
 */
  private def giveToken(pos:Int):Token={
    tokens(pos) match {
      case Right(a) => a
      case Left(a) => throw NoPreAndErrorTokenIsExpected(pos, tokens)
    }
  }

  /*
  check if the token in position pos is an Arrow
   */
  private def isArrow(pos:Int):Boolean={
    tokens(pos) match {
      case Right(Arrow(_)) => true
      case Right(a) => false
      case Left(a) => false
    }
  }

  /*
check if the token in position pos is are Dots
 */
  private def isDots(pos:Int):Boolean={
    tokens(pos) match {
      case Right(Dots(_)) => true
      case Right(a) => false
      case Left(a) => false
    }
  }

  /*
check if the token in position pos is an Backlash
 */
  private def isBacklash(pos:Int):Boolean={
    tokens(pos) match{
      case Right(Backslash(_)) => true
      case Right(a) => false
      case Left(a) => false
    }
  }
}
