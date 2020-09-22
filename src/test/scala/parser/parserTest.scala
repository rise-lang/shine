package parser
import org.scalatest.flatspec.AnyFlatSpec
import parser.parse.HereIsATypeAnnotationExpected
//import org.scalatest.matchers.should.Matchers.equal
import org.scalatest.matchers.should.Matchers._
import rise.{core => r}
import rise.core.{types => rt}
import util.Execute.Exception
import rise.core.{semantics => rS}


class parserTest extends  AnyFlatSpec {
  val testFilePath = "src/test/scala/parser/readFiles/filesToLex/"


  "parser" should "not be able to parse 'Identity.rise'" in {
    val fileName: String = testFilePath + "identity.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val thrown = intercept[HereIsATypeAnnotationExpected] {
      parse(lexer.tokens)
    }
    //Todo: that is horrible, that I don't have an Error-Message
    thrown should equal(null)
  }

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

  "parser" should "be able to parse 'minus.rise'" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/minus.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("x"), r.App(r.App(r.primitives.Sub(), r.Literal(rS.IntData(5))),r.Identifier("x"))) => true
      case r.Lambda(x,e) => throw Exception("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => throw Exception("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'negation.rise'" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/negation.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("y"), r.App(r.primitives.Neg(), r.Identifier("y"))) => true
      case r.Lambda(x,e) => throw Exception("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => throw Exception("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'plus.rise'" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/plus.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("x"), r.App(r.App(r.primitives.Add(), r.Literal(rS.IntData(5))),r.Identifier("x"))) => true
      case r.Lambda(x,e) => throw Exception("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => throw Exception("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'not.rise'" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/not.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("b"), r.App(r.primitives.Not(), r.Identifier("b"))) => true
      case r.Lambda(x,e) => throw Exception("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => throw Exception("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'braces.rise'" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/braces.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("b"), r.Identifier("b")) => true
      case r.Lambda(x,e) => throw Exception("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => throw Exception("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'bracesWithNot.rise'" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/bracesWithNot.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("b"), r.App(r.primitives.Not(), r.Identifier("b"))) => true
      case r.Lambda(x,e) => throw Exception("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => throw Exception("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'lessComplexInOneLine.rise'" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/lessComplexInOneLine.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("y"), r.App(r.primitives.Neg(), r.App(r.App(r.primitives.Mul(), r.Identifier("y")), r.Identifier("x"))))) => true
      case r.Lambda(x,e) => {
        println("not correct Identifier or not correct expression: "+ x + " , " + e)
        throw new RuntimeException("not correct Identifier or not correct expression: "+ x + " , " + e)
      }
      case a => throw Exception("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'lessComplexInOneLineWithType.rise'" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/lessComplexInOneLineWithType.rise"
    val file: FileReader = FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("y"), r.App(r.primitives.Neg(), r.App(r.App(r.primitives.Mul(), r.Identifier("y")), r.Identifier("x"))))) => true
      case r.Lambda(x,e) => {
        println("not correct Identifier or not correct expression: "+ x + " , " + e)
        throw new RuntimeException("not correct Identifier or not correct expression: "+ x + " , " + e)
      }
      case a => throw Exception("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'littleComplexLine.rise'" in {
    val fileName: String = testFilePath + "littleComplexLine.rise"
    val file: FileReader = FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(x@r.Identifier("x"),
             r.Lambda(r.Identifier("y"),
               r.App(r.primitives.Neg(),
                     r.App(r.App(r.primitives.Add(), r.Literal(rS.IntData(42))),
                           r.App(r.App(r.primitives.Mul(), r.Identifier("y")), r.Identifier("x")))
               ))) if x.t == rt.i32 => true
      case r.Lambda(x,e) => fail("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => throw Exception("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'complexInOneLine.rise'" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/complexInOneLine.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("y"), r.App(r.primitives.Neg(), r.App(r.App(r.primitives.Add(), r.App(r.App(r.primitives.Mul(), r.Identifier("y")), r.Identifier("x"))), r.App(r.App(r.primitives.Mod(), r.Literal(rS.IntData(5))), r.Literal(rS.IntData(42))))))) => true
      case r.Lambda(x,e) => throw Exception("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => throw Exception("not a lambda: "+ a)
    }
  }

}
