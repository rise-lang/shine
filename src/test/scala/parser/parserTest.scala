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
  //HashMap<r.Identifier, Option[r.Expr]> ist das oberste
  type MapFkt = parser.MapFkt


  "parser" should "be able to parse 'Brace5.rise'" in {
    val fileName: String = testFilePath + "Brace5.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("x"), r.Identifier("x")) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'braces.rise'" in {
    val fileName: String = testFilePath + "braces.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("b"), r.Identifier("b")) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'bracesWithNot.rise'" in {
    val fileName: String = testFilePath + "bracesWithNot.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("b"), r.App(r.primitives.Not(), r.Identifier("b"))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'Complex1.rise'" in {
    val fileName: String = testFilePath + "Complex1.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("y"), r.Lambda(r.Identifier("z"), r.App(r.App(r.primitives.Mul(), r.Identifier("x")), r.App(r.App(r.primitives.Add(), r.Identifier("y")), r.Identifier("z")))))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'Complex2.rise'" in {
    val fileName: String = testFilePath + "Complex2.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("x"), r.App(r.App(r.primitives.Mul(), r.App(r.App(r.primitives.Add(),
      r.Identifier("x")), r.Identifier("x"))), r.Identifier("x"))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'Complex3.rise'" in {
    val fileName: String = testFilePath + "Complex3.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("x"), r.App(r.App(r.primitives.Mul(), r.App(r.App(r.primitives.Add(),
      r.Identifier("x")), r.Identifier("x"))), r.Identifier("x"))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'Complex4.rise'" in {
    val fileName: String = testFilePath + "Complex4.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("x"), r.App(r.App(r.primitives.Div(), r.App(r.App(r.primitives.Mul(),
      r.Identifier("x")), r.Identifier("x"))), r.App(r.App(r.primitives.Mul(),
      r.Identifier("x")), r.Identifier("x")))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'Complex5.rise'" in {
    val fileName: String = testFilePath + "Complex5.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("x"), r.App(r.App(r.primitives.Div(), r.App(r.App(r.primitives.Mul(),
      r.Identifier("x")), r.Identifier("x"))), r.App(r.App(r.primitives.Mul(),
      r.Identifier("x")), r.Identifier("x")))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'Complex6.rise'" in {
    val fileName: String = testFilePath + "Complex6.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("x"), r.App(r.App(r.primitives.Div(), r.App(r.App(r.primitives.Mul(),
      r.Identifier("x")), r.Identifier("x"))), r.App(r.App(r.primitives.Mul(),
      r.Identifier("x")), r.Identifier("x")))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'Complex7.rise'" in {
    val fileName: String = testFilePath + "Complex7.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("x"), r.App(r.App(r.primitives.Div(), r.App(r.App(r.primitives.Mul(),
      r.Identifier("x")), r.Identifier("x"))), r.App(r.App(r.primitives.Mul(),
      r.Identifier("x")), r.Identifier("x")))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'ComplexIdentifier.rise'" in {
    val fileName: String = testFilePath + "ComplexIdentifier.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("hans_Georg"), r.Identifier("hans_Georg")) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'complexInOneLine.rise'" in {
    val fileName: String = testFilePath + "complexInOneLine.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("y"), r.App(r.primitives.Neg(),
      r.App(r.App(r.primitives.Add(), r.App(r.App(r.primitives.Mul(), r.Identifier("x")), r.Identifier("y"))),
      r.App(r.App(r.primitives.Mod(), r.Literal(rS.IntData(42))), r.Literal(rS.IntData(5))))))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e + ": expected: " +
        r.Lambda(r.Identifier("x")(), r.Lambda(r.Identifier("y")(), r.App(r.primitives.Neg()(),
          r.App(r.App(r.primitives.Add()(), r.App(r.App(r.primitives.Mul()(), r.Identifier("x")())(), r.Identifier("y")())())(),
            r.App(r.App(r.primitives.Mod()(), r.Literal(rS.IntData(42)))(), r.Literal(rS.IntData(5)))())())())())())
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'complexInThreeLines.rise'" in {
    val fileName: String = testFilePath + "complexInThreeLines.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("y"), r.App(r.primitives.Neg(),
      r.App(r.App(r.primitives.Add(), r.App(r.App(r.primitives.Mul(), r.Identifier("x")), r.Identifier("y"))),
      r.App(r.App(r.primitives.Mod(), r.Literal(rS.IntData(42))), r.Literal(rS.IntData(5))))))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e + ": expected: " +
        r.Lambda(r.Identifier("x")(), r.Lambda(r.Identifier("y")(), r.App(r.primitives.Neg()(),
          r.App(r.App(r.primitives.Add()(), r.App(r.App(r.primitives.Mul()(), r.Identifier("x")())(), r.Identifier("y")())())(),
            r.App(r.App(r.primitives.Mod()(), r.Literal(rS.IntData(42)))(), r.Literal(rS.IntData(5)))())())())())())
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'constant42.rise'" in {
    val fileName: String = testFilePath + "constant42.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("c"), r.Literal(rS.IntData(42))) => true
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'FunctionInBraces.rise'" in {
    val fileName: String = testFilePath + "FunctionInBraces.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("x"), r.App(r.Lambda(r.Identifier("y"), r.Identifier("y")), r.Identifier("x"))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'fx.rise'" in {
    val fileName: String = testFilePath + "fx.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("x"), r.App(r.Identifier("fkt"), r.Identifier("x"))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "not be able to parse 'IdentityWithI32.rise'" in {
    val fileName: String = testFilePath + "identityWithI32.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("x"), r.Identifier("x")) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'lessComplexInOneLine.rise'" in {
    val fileName: String = testFilePath + "lessComplexInOneLine.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("y"), r.App(r.primitives.Neg(), r.App(r.App(r.primitives.Mul(), r.Identifier("x")), r.Identifier("y"))))) => true
      case r.Lambda(x, e) => {
        println("not correct Identifier or not correct expression: " + x + " , " + e)
        fail("not correct Identifier or not correct expression: " + x + " , " + e)
      }
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'lessComplexInOneLineWithDifferentType.rise'" in {
    val fileName: String = testFilePath + "lessComplexInOneLineWithDifferentType.rise"
    val file: FileReader = FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("y"), r.App(r.primitives.Neg(), r.App(r.App(r.primitives.Mul(), r.Identifier("x")), r.Identifier("y"))))) => true
      case r.Lambda(x, e) => {
        println("not correct Identifier or not correct expression: " + x + " , " + e)
        fail("not correct Identifier or not correct expression: " + x + " , " + e)
      }
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'littleComplexLine.rise'" in {
    val fileName: String = testFilePath + "littleComplexLine.rise"
    val file: FileReader = FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(x@r.Identifier("x"),
      r.Lambda(y@r.Identifier("y"),
      r.App(r.App(r.primitives.Add(),
      r.App(r.App(r.primitives.Mul(), r.Identifier("x")), r.Identifier("y"))),
      r.Literal(rS.IntData(42)))
      )) if x.t == rt.i32 && y.t == rt.i32 => true
      case r.Lambda(x@r.Identifier("x"),
      r.Lambda(y@r.Identifier("y"),
      r.App(r.App(r.primitives.Add(),
      r.App(r.App(r.primitives.Mul(), r.Identifier("x")), r.Identifier("y"))),
      r.Literal(rS.IntData(42)))
      )) => fail("almost correct, but Types don't match!  " + x.t + " != rt.i32 oder/und " + y.t + " != rt.i32")
      case r.Lambda(x, r.Lambda(y, e)) => fail("not correct Identifier or not correct expression: " + "Lambda(" + x + ",Lambda," + y + " , " + e + " , x.t= " + x.t + " , y.t= " + y.t)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'longIdentityWithI32.rise'" in {
    val fileName: String = testFilePath + "longIdentityWithI32.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("jens"), r.Identifier("jens")) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'minus.rise'" in {
    val fileName: String = testFilePath + "minus.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("x"), r.App(r.App(r.primitives.Sub(), r.Identifier("x")), r.Literal(rS.IntData(5)))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'negation.rise'" in {
    val fileName: String = testFilePath + "negation.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("y"), r.App(r.primitives.Neg(), r.Identifier("y"))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'negationWithBool.rise'" in {
    val fileName: String = testFilePath + "negationWithBool.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("b"), r.App(r.primitives.Neg(), r.Identifier("b"))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "not be able to parse 'noExpressionInBraces.rise'" in {
    val fileName: String = testFilePath + "noExpressionInBraces.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val thrown = intercept[RuntimeException] {
      parser(lexer.tokens)
    }

    thrown.getMessage should equal("There was no Expression in Braces at posstion (0 , 1 : List((, ), <EndNamedExpr>)")
  }

  "parser" should "be able to parse 'not.rise'" in {
    val fileName: String = testFilePath + "not.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("b"), r.App(r.primitives.Not(), r.Identifier("b"))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'plus.rise'" in {
    val fileName: String = testFilePath + "plus.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = map.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }

    ex match {
      case r.Lambda(r.Identifier("x"), r.App(r.App(r.primitives.Add(), r.Identifier("x")), r.Literal(rS.IntData(5)))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "be able to parse 'twoplus1extraDefintion.rise'" in {
    val fileName: String = testFilePath + "twoplus1extraDefintion.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName_h: String = "h"
    val ex_h: r.Expr = map.get(functionName_h).getOrElse(fail("The function '" + functionName_h + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }
    val functionName_f: String = "f"
    val ex_f: r.Expr = map.get(functionName_f).getOrElse(fail("The function '" + functionName_f + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }
    val functionName_z: String = "z"
    val ex_z: r.types.Type = map.get(functionName_z).getOrElse(fail("The function '" + functionName_z + "' does not exist!!!")) match {
      case Left(lambda) => fail("it is no definition expected: " + lambda)
      case Right(t) =>
        t
    }

    ex_h match {
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("fkt"), r.App(r.Identifier("fkt"), r.Identifier("x")))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }

    ex_f match {
      case r.Lambda(r.Identifier("y"), r.App(r.App(r.primitives.Add(), r.Identifier("y")), r.Literal(rS.IntData(5)))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
    ex_z match {
      case rt.FunType(rt.i32, rt.i32) => true
      case a => fail("not correct Type: " + a)
    }

  }

  "parser" should "not be able to parse 'twoplus1extraDefintionButSameNameInLocalVariable.rise'" in {
    val fileName: String = testFilePath + "twoplus1extraDefintionButSameNameInLocalVariable.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val thrown = intercept[RuntimeException] {
      parser(lexer.tokens)
    }

    thrown.getMessage should equal("A variable or function with the exact same name 'x' is already declared! <- Some(Left( x))")
  }

  "parser" should "be able to parse 'twoSimpleFunctions.rise'" in {
    val fileName: String = testFilePath + "twoSimpleFunctions.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName_h: String = "h"
    val ex_h: r.Expr = map.get(functionName_h).getOrElse(fail("The function '" + functionName_h + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }
    val functionName_f: String = "f"
    val ex_f: r.Expr = map.get(functionName_f).getOrElse(fail("The function '" + functionName_f + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }
    val functionName_z: String = "z"
    if (map.contains(functionName_z)) {
      fail("no Function with name '" + functionName_z + "' was declared")
    }

    ex_h match {
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("fkt"), r.App(r.Identifier("fkt"), r.Identifier("x")))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }

    ex_f match {
      case r.Lambda(r.Identifier("y"), r.App(r.App(r.primitives.Add(), r.Identifier("y")), r.Literal(rS.IntData(5)))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  "parser" should "not be able to parse 'twoSimpleFunctionsButWithSameLocalVarName.rise'" in {
    val fileName: String = testFilePath + "twoSimpleFunctionsButWithSameLocalVarName.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val thrown = intercept[RuntimeException] {
      parser(lexer.tokens)
    }

    thrown.getMessage should equal("A variable or function with the exact same name 'x' is already declared! <- Some(Left( x))")
  }

  "parser" should "be able to parse 'veryComplicated.rise'" in {
    val fileName: String = testFilePath + "veryComplicated.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val map: MapFkt = parser(lexer.tokens)

    val functionName_f: String = "f"
    val ex_f: r.Expr = map.get(functionName_f).getOrElse(fail("The function '" + functionName_f + "' does not exist!!!")) match {
      case Left(lambda) => lambda
      case Right(types) => fail("no definition is in map: " + types)
    }
    val functionName_2: String = "specialFunctionOfChaos"
    val type_2:r.types.Type= map.get(functionName_2).getOrElse(fail("The function '" + functionName_f + "' does not exist!!!")) match {
      case Left(lambda) => fail("no definition is expected: "+ lambda)
      case Right(t) => t
    }

    type_2 match {
      case rt.FunType(rt.bool, rt.FunType(rt.bool, rt.FunType(rt.i32,
      rt.FunType(rt.f32, rt.f32)
      ))) => true
      case a => fail("it was a different definition expected, but we see: "+ a)
    }

    ex_f match {
      case r.Lambda(r.Identifier("michael"), r.Lambda(r.Identifier("heinrich"),
      r.App(r.primitives.Not(), r.App(r.App(r.primitives.Equal(),r.App(
      r.App(r.primitives.Mod(),

      r.Lambda(r.Identifier("varX"), r.Lambda(r.Identifier("varY"),
      r.App(r.App(r.primitives.Mul(), r.Identifier("varX")),
      r.App(r.App(r.primitives.Mul(), r.Identifier("varY"))
      ,

      r.App(r.App(r.primitives.Div(), r.App(r.App(r.primitives.Sub(),
      r.Literal(rS.IntData(25))), r.Literal(rS.FloatData(a)))
      ), r.Literal(rS.FloatData(b)))

      ))))),r.Literal(rS.IntData(42)))),
      r.Literal(rS.IntData(0))))
      )) if a == 10.5000 && b == 2.3000f=> true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

}