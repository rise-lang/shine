package parser.lexer

case class syntacticAnalysis(lexer:RecognizeLexeme){
//  private val fileReader:FileReader = lexer.fileReader
//  private val tokens:List[Either[PreAndErrorToken, Token]] = lexer.tokens
//    private def checkBracesNumber():Unit = {
//      val list:List[Either[PreAndErrorToken, Token]] = tokens
//      val arr: Array[String]= fileReader.sourceLines
//      val lB: Int = list.count(a => a match {
//        case Right(LBrace(_)) => true
//        case b => false
//      } )
//      val rB: Int = list.count(a => a match {
//        case Right(RBrace(_)) => true
//        case b => false
//      } )
//      if(lB>rB){ //for example "( .. ( ... )"
//        val loc:Location = Location(arr.length-1, arr(arr.length-1).length)
//        val ex = RightBraceMissing(new Span(fileReader, loc), fileReader)
//        ex.throwException()
//      }else if(lB<rB){ //for example "( .. ) ... )"
//        val loc:Location = Location(arr.length-1, arr(arr.length-1).length)
//        val ex = LeftBraceMissing(new Span(fileReader, loc), fileReader)
//        ex.throwException()
//      }
//    }
}
