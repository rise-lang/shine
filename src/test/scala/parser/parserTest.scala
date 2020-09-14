package parser
import org.scalatest.flatspec.AnyFlatSpec
//import org.scalatest.matchers.should.Matchers.equal
import org.scalatest.matchers.should.Matchers._
import parser.lexer.{FileReader, RecognizeLexeme, parse}
import rise.{core => r}
import util.Execute.Exception


class parserTest extends  AnyFlatSpec {

//  "parser" should "not be able to parse 'Identity.rise'" in {
//    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/identity.rise"
//    val file: FileReader = new FileReader(fileName)
//    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
//    val thrown = intercept[Exception] {
//      parse(lexer.tokens)
//    }
//    thrown should equal(Exception(""))
//  }

  "parser" should "be able to parse 'longIdentityWithI32.rise'" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/longIdentityWithI32.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("jens"), r.Identifier("jens")) => true
      case r.Lambda(x,e) => throw Exception("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => throw Exception("not a lambda: "+ a)
    }
  }

}
