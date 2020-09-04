package parser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.equal
import org.scalatest.matchers.should.Matchers._
import parser.lexer.{FileReader, RecognizeLexeme}


class parserTest extends  AnyFlatSpec {
  val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/identity.rise"
  val file: FileReader = new FileReader(fileName)
  val lexer: RecognizeLexeme = new RecognizeLexeme(file)
  val parser:

}
