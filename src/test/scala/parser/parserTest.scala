package parser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.equal
import org.scalatest.matchers.should.Matchers._
import parser.lexer.{FileReader, RecognizeLexeme, parse}
import rise.{core => r}
import util.Execute.Exception


class parserTest extends  AnyFlatSpec {
  val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/identity.rise"
  val file: FileReader = new FileReader(fileName)
  val lexer: RecognizeLexeme = new RecognizeLexeme(file)
  val ex: r.Expr = parse(lexer.tokens)
  ex.t should equal(r.Lambda)
  ex match {
    case r.Lambda(r.Identifier("x"), r.Identifier("x")) => true
    case r.Lambda(x,e) => throw Exception("not correct Identifier or not correct expression: "+ x + " , " + e)
    case a => throw Exception("not a lambda: "+ a)
  }
}
