package parser

abstract sealed class SyntacticError (
  pos: Int,
  tokens: List[Either[PreAndErrorToken, Token]]
) extends RuntimeException {
  require(pos >=0, "pos is less than zero")
  require(pos<tokens.length)
//  def throwException():Unit ={
//    val message:String = this.toString
//    throw new Exception(message)
//  }
}

final case class NoPreAndErrorTokenIsExpected(
  pos:Int,
  tokens:List[Either[PreAndErrorToken, Token]]
) extends SyntacticError(pos, tokens) {
  override def toString =
    "at postion " + pos + " is not PreAndErrorToken '" + tokens(pos) +
      "' expected!\n"+ tokens.toString()
}

final case class WrongToken(
  pos: Int,
  tokens:List[Either[PreAndErrorToken, Token]],
  expectedToken: String
) extends SyntacticError(pos, tokens){
  override def toString =
    "at postion " + pos + " is "+ expectedToken + " expected, but found '"+
      tokens(pos) +"'!\n"+ tokens.toString()
}
