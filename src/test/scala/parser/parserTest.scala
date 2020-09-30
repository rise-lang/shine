package parser
import org.scalatest.flatspec.AnyFlatSpec
//import parser.parse.ParseError
//import org.scalatest.matchers.should.Matchers.equal
import org.scalatest.matchers.should.Matchers._
import rise.{core => r}
import rise.core.{types => rt}
import rise.core.{semantics => rS}


class parserTest extends  AnyFlatSpec {
  val testFilePath = "src/test/scala/parser/readFiles/filesToLex/"


  "parser" should "not be able to parse 'Identity.rise'" in {
    val fileName: String = testFilePath + "identity.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val thrown = intercept[RuntimeException] {
      parse(lexer.tokens)
    }
    //Todo: that is horrible, that I don't have an Error-Message
    thrown.getMessage should equal("failed [[parsing : ParseError(failed to parse Type: A TypeAnnotation is expected, but -> is not an Colon)]]")
  }

  "parser" should "be able to parse 'longIdentityWithI32.rise'" in {
    val fileName: String = testFilePath + "longIdentityWithI32.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("jens"), r.Identifier("jens")) => true
      case r.Lambda(x,e) => fail("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => fail("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'minus.rise'" in {
    val fileName: String = testFilePath + "minus.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("x"), r.App(r.App(r.primitives.Sub(), r.Identifier("x")),r.Literal(rS.IntData(5)))) => true
      case r.Lambda(x,e) => fail("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => fail("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'negation.rise'" in {
    val fileName: String = testFilePath + "negation.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("y"), r.App(r.primitives.Neg(), r.Identifier("y"))) => true
      case r.Lambda(x,e) => fail("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => fail("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'plus.rise'" in {
    val fileName: String = testFilePath + "plus.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("x"), r.App(r.App(r.primitives.Add(), r.Identifier("x")),r.Literal(rS.IntData(5)))) => true
      case r.Lambda(x,e) => fail("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => fail("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'not.rise'" in {
    val fileName: String = testFilePath + "not.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("b"), r.App(r.primitives.Not(), r.Identifier("b"))) => true
      case r.Lambda(x,e) => fail("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => fail("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'braces.rise'" in {
    val fileName: String = testFilePath + "braces.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("b"), r.Identifier("b")) => true
      case r.Lambda(x,e) => fail("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => fail("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'bracesWithNot.rise'" in {
    val fileName: String = testFilePath + "bracesWithNot.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("b"), r.App(r.primitives.Not(), r.Identifier("b"))) => true
      case r.Lambda(x,e) => fail("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => fail("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'lessComplexInOneLine.rise'" in {
    val fileName: String = testFilePath + "lessComplexInOneLine.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("y"), r.App(r.primitives.Neg(), r.App(r.App(r.primitives.Mul(), r.Identifier("x")), r.Identifier("y"))))) => true
      case r.Lambda(x,e) => {
        println("not correct Identifier or not correct expression: "+ x + " , " + e)
        fail("not correct Identifier or not correct expression: "+ x + " , " + e)
      }
      case a => fail("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'lessComplexInOneLineWithType.rise'" in {
    val fileName: String = testFilePath + "lessComplexInOneLineWithType.rise"
    val file: FileReader = FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("y"), r.App(r.primitives.Neg(), r.App(r.App(r.primitives.Mul(), r.Identifier("x")), r.Identifier("y"))))) => true
      case r.Lambda(x,e) => {
        println("not correct Identifier or not correct expression: "+ x + " , " + e)
        fail("not correct Identifier or not correct expression: "+ x + " , " + e)
      }
      case a => fail("not a lambda: "+ a)
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
             r.Lambda(y@r.Identifier("y"),
                     r.App(r.App(r.primitives.Add(),
                      r.App(r.App(r.primitives.Mul(), r.Identifier("x")), r.Identifier("y"))),
                     r.Literal(rS.IntData(42)))
               )) if x.t == rt.i32 && y.t == rt.i32 => true
      case r.Lambda(x,r.Lambda(y,e)) => fail("not correct Identifier or not correct expression: "+ "Lambda(" + x +",Lambda,"+y+ " , " + e + " , x.t= "+ x.t + " , y.t= "+ y.t)
      case a => fail("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'complexInOneLine.rise'" in {
    val fileName: String = testFilePath + "complexInOneLine.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("y"), r.App(r.primitives.Neg(),
      r.App(r.App(r.primitives.Add(), r.App(r.App(r.primitives.Mul(), r.Identifier("x")), r.Identifier("y"))),
      r.App(r.App(r.primitives.Mod(), r.Literal(rS.IntData(42))), r.Literal(rS.IntData(5))))))) => true
      case r.Lambda(x,e) => fail("not correct Identifier or not correct expression: "+ x + " , " + e + ": expected: " +
        r.Lambda(r.Identifier("x")() , r.Lambda(r.Identifier("y")() , r.App(r.primitives.Neg()(),
          r.App(r.App(r.primitives.Add()(), r.App(r.App(r.primitives.Mul()(), r.Identifier("x")())(), r.Identifier("y")())())(),
            r.App(r.App(r.primitives.Mod()(), r.Literal(rS.IntData(42)))(), r.Literal(rS.IntData(5)))())())())())())
      case a => fail("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'Complex2.rise'" in {
    val fileName: String = testFilePath + "Complex2.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("x"), r.App(r.App(r.primitives.Mul(), r.App(r.App(r.primitives.Add(),
      r.Identifier("x")), r.Identifier("x"))), r.Identifier("x")))=> true
      case r.Lambda(x,e) => fail("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => fail("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'Complex3.rise'" in {
    val fileName: String = testFilePath + "Complex3.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("x"), r.App(r.App(r.primitives.Mul(), r.App(r.App(r.primitives.Add(),
      r.Identifier("x")), r.Identifier("x"))), r.Identifier("x")))=> true
      case r.Lambda(x,e) => fail("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => fail("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'Complex4.rise'" in {
    val fileName: String = testFilePath + "Complex4.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("x"), r.App(r.App(r.primitives.Div(), r.App(r.App(r.primitives.Mul(),
      r.Identifier("x")), r.Identifier("x"))), r.App(r.App(r.primitives.Mul(),
      r.Identifier("x")), r.Identifier("x"))))=> true
      case r.Lambda(x,e) => fail("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => fail("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'Complex5.rise'" in {
    val fileName: String = testFilePath + "Complex5.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("x"), r.App(r.App(r.primitives.Div(), r.App(r.App(r.primitives.Mul(),
      r.Identifier("x")), r.Identifier("x"))), r.App(r.App(r.primitives.Mul(),
      r.Identifier("x")), r.Identifier("x"))))=> true
      case r.Lambda(x,e) => fail("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => fail("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'Complex6.rise'" in {
    val fileName: String = testFilePath + "Complex6.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("x"), r.App(r.App(r.primitives.Div(), r.App(r.App(r.primitives.Mul(),
      r.Identifier("x")), r.Identifier("x"))), r.App(r.App(r.primitives.Mul(),
      r.Identifier("x")), r.Identifier("x"))))=> true
      case r.Lambda(x,e) => fail("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => fail("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'Complex7.rise'" in {
    val fileName: String = testFilePath + "Complex7.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("x"), r.App(r.App(r.primitives.Div(), r.App(r.App(r.primitives.Mul(),
      r.Identifier("x")), r.Identifier("x"))), r.App(r.App(r.primitives.Mul(),
      r.Identifier("x")), r.Identifier("x"))))=> true
      case r.Lambda(x,e) => fail("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => fail("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'fx.rise'" in {
    val fileName: String = testFilePath + "fx.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("x"), r.App(r.Identifier("f"), r.Identifier("x"))) => true
      case r.Lambda(x,e) => fail("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => fail("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'FunctionInBraces.rise'" in {
    val fileName: String = testFilePath + "FunctionInBraces.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("x"), r.App(r.Lambda(r.Identifier("y"), r.Identifier("y")), r.Identifier("x"))) => true
      case r.Lambda(x,e) => fail("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => fail("not a lambda: "+ a)
    }
  }

  "parser" should "be able to parse 'Brace5.rise'" in {
    val fileName: String = testFilePath + "Brace5.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val ex: r.Expr = parse(lexer.tokens)
    //ex.t should equal(r.Lambda) //TODO: Why does this not work!?
    ex match {//rt.i32
      case r.Lambda(r.Identifier("x"), r.Identifier("x")) => true
      case r.Lambda(x,e) => fail("not correct Identifier or not correct expression: "+ x + " , " + e)
      case a => fail("not a lambda: "+ a)
    }
  }
}
