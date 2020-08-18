import parser.lexer.OpType.{BinOpType, UnaryOpType}
import parser.lexer.FloatTyp
import parser.lexer.FloatTyp
import parser.lexer.{Arrow, Backslash, BinOp, BoolType, Dots, F32, FileReader, FloatTyp, I32, Identifier, IdentifierType, IntTyp, LBrace, Location, RBrace, RecognizeLexeme, Span, Type, UnOp}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.equal
import org.scalatest.matchers.should.Matchers._

class LexerTest extends  AnyFlatSpec{

  "RecognizeLexeme" should "work for the identity" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/identity.rise"
    val file:FileReader = new FileReader(fileName)
    val lexer:RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case  Right(Identifier(IdentifierType("x"), span1)) :: Right(Arrow(span2)):: Right(Identifier(IdentifierType("x"), span3)) :: Right(Backslash(span4)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the constant42" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/constant42.rise"
    val file:FileReader = new FileReader(fileName)
    val lexer:RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case  Right(I32(42, span1)) :: Right(Arrow(span2)):: Right(Identifier(IdentifierType("c"), span3)) :: Right(Backslash(span4)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the longIdentity" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/longIdentity.rise"
    val file:FileReader = new FileReader(fileName)
    val lexer:RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case  Right(Identifier(IdentifierType("Kevin"), span1)) :: Right(Arrow(span2)):: Right(Identifier(IdentifierType("Kevin"), span3)) :: Right(Backslash(span4)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the identityWithI32" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/identityWithI32.rise"
    val file:FileReader = new FileReader(fileName)
    val lexer:RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case  Right(Identifier(IdentifierType("x"), span1)) :: Right(Arrow(span2)):: Right(Type(IntTyp(), span)) :: Right(Dots(span0)) :: Right(Identifier(IdentifierType("x"), span3)) :: Right(Backslash(span4)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the longIdentityWithI32" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/longIdentityWithI32.rise"
    val file:FileReader = new FileReader(fileName)
    val lexer:RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case  Right(Identifier(IdentifierType("jens"), span1)) :: Right(Arrow(span2)):: Right(Type(IntTyp(), span)) :: Right(Dots(span0)) :: Right(Identifier(IdentifierType("jens"), span3)) :: Right(Backslash(span4)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the ComplexIdentifier" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/ComplexIdentifier.rise"
    val file:FileReader = new FileReader(fileName)
    val lexer:RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case  Right(Identifier(IdentifierType("Hans_Georg"), span1)) :: Right(Arrow(span2)):: Right(Type(FloatTyp(), span3)) :: Right(Dots(span4)) :: Right(Identifier(IdentifierType("Hans_Georg"), span5)) :: Right(Backslash(span6)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for TypWith-" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/TypWith-.rise"
    val file:FileReader = new FileReader(fileName)
    val thrown = intercept[Exception] {
      new RecognizeLexeme(file)
    }
    val expected:String = "ErrorToken: It is an '->' expected. The Lexeme '--' is not an '->'! at FileReader: fileName: 'src/test/scala/riseParser/readFiles/filesToLexe/TypWith-.rise'; fileContent: {\n\\x:I32-->x\n}; beginLocation: (column: 0 ; row: 6); endLocation: (column: 0 ; row: 7)\n\\x:I32-̲->x"
    thrown.getMessage should equal (expected)
  }

  "RecognizeLexeme" should "work for noBacklashAtBeginning.rise" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/noBacklashAtBeginning.rise"
    val file:FileReader = new FileReader(fileName)
    val thrown = intercept[Exception] {
      new RecognizeLexeme(file)
    } //Todo: should we have a underline of the important code here too? yes, but here is no
    val expected:String = "ErrorToken: It is an '\\' expected. The Lexeme 'x' is not an '\\'! at FileReader: fileName: 'src/test/scala/riseParser/readFiles/filesToLexe/noBacklashAtBeginning.rise'; fileContent: {\nx:I32->x+5\n}; beginLocation: (column: 0 ; row: 0); endLocation: (column: 0 ; row: 0)\nx:I32->x+5"
    thrown.getMessage should equal (expected)
  }
  "RecognizeLexeme" should "work for plus" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/plus.rise"
    val file:FileReader = new FileReader(fileName)
    val lexer:RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case  Right(I32(5, span1))::Right(BinOp(BinOpType.ADD, span2))::Right(Identifier(IdentifierType("x"), span3)) :: Right(Arrow(span4)):: Right(Type(IntTyp(), span5)) :: Right(Dots(span6)) :: Right(Identifier(IdentifierType("x"), span7)) :: Right(Backslash(span8)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for minus" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/minus.rise"
    val file:FileReader = new FileReader(fileName)
    val lexer:RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case  Right(I32(5, span1))::Right(BinOp(BinOpType.SUB, span2))::Right(Identifier(IdentifierType("x"), span3)) :: Right(Arrow(span4)):: Right(Type(IntTyp(), span5)) :: Right(Dots(span6)) :: Right(Identifier(IdentifierType("x"), span7)) :: Right(Backslash(span8)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for negation" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/negation.rise"
    val file:FileReader = new FileReader(fileName)
    val lexer:RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case  Right(Identifier(IdentifierType("y"), span1)):: Right(UnOp(UnaryOpType.NEG, span2)):: Right(Arrow(span4)):: Right(Type(IntTyp(), span5)) :: Right(Dots(span6)) :: Right(Identifier(IdentifierType("y"), span7)) :: Right(Backslash(span8)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for negationWithBool" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/negationWithBool.rise"
    val file:FileReader = new FileReader(fileName)
    val lexer:RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case  Right(Identifier(IdentifierType("b"), span1)):: Right(UnOp(UnaryOpType.NEG, span2)):: Right(Arrow(span4)):: Right(Type(BoolType(), span5)) :: Right(Dots(span6)) :: Right(Identifier(IdentifierType("b"), span7)) :: Right(Backslash(span8)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for not" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/not.rise"
    val file:FileReader = new FileReader(fileName)
    val lexer:RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case  Right(Identifier(IdentifierType("b"), span1)):: Right(UnOp(UnaryOpType.NOT, span2)):: Right(Arrow(span4)):: Right(Type(BoolType(), span5)) :: Right(Dots(span6)) :: Right(Identifier(IdentifierType("b"), span7)) :: Right(Backslash(span8)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for braces" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/braces.rise"
    val file:FileReader = new FileReader(fileName)
    val lexer:RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case  Right(RBrace(span1)):: Right(Identifier(IdentifierType("b"), span2)):: Right(LBrace(span3)):: Right(Arrow(span4)):: Right(Type(BoolType(), span5)) :: Right(Dots(span6)) :: Right(Identifier(IdentifierType("b"), span7)) :: Right(Backslash(span8)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for bracesWithNot" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/bracesWithNot.rise"
    val file:FileReader = new FileReader(fileName)
    val lexer:RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case  Right(RBrace(span0)):: Right(Identifier(IdentifierType("b"), span1)):: Right(UnOp(UnaryOpType.NOT, span2))::Right(LBrace(span3)):: Right(Arrow(span4)):: Right(Type(BoolType(), span5)) :: Right(Dots(span6)) :: Right(Identifier(IdentifierType("b"), span7)) :: Right(Backslash(span8)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for LeftBraceMissing.rise" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/LeftBraceMissing.rise"
    val file:FileReader = new FileReader(fileName)
    val thrown = intercept[Exception] {
      new RecognizeLexeme(file)
    } //Todo: should we have a underline of the important code here too? yes, but here is no
    val expected:String = "ErrorToken: Left Brace is missing! at FileReader: fileName: 'src/test/scala/riseParser/readFiles/filesToLexe/LeftBraceMissing.rise'; fileContent: {\n\\b:bool-> b )\n}; beginLocation: (column: 0 ; row: 12); endLocation: (column: 0 ; row: 12)\n\\b:bool-> b )"
    thrown.getMessage should equal (expected)
  }

  "RecognizeLexeme" should "work for RightBraceMissing.rise" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/RightBraceMissing.rise"
    val file:FileReader = new FileReader(fileName)
    val thrown = intercept[Exception] {
      new RecognizeLexeme(file)
    }
    val expected:String = "ErrorToken: Right Brace is missing! at FileReader: fileName: 'src/test/scala/riseParser/readFiles/filesToLexe/RightBraceMissing.rise'; fileContent: {\n\\b:bool->(   b\n}; beginLocation: (column: 0 ; row: 13); endLocation: (column: 0 ; row: 14)\n\\b:bool->(   b̲"
    thrown.getMessage should equal (expected)
  }

  "RecognizeLexeme" should "work for tooMuchRightBraces.rise" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/tooMuchRightBraces.rise"
    val file:FileReader = new FileReader(fileName)
    val thrown = intercept[Exception] {
      new RecognizeLexeme(file)
    } //Todo: should we have a underline of the important code here too? yes, but here is no
    val expected:String = "ErrorToken: Left Brace is missing! at FileReader: fileName: 'src/test/scala/riseParser/readFiles/filesToLexe/tooMuchRightBraces.rise'; fileContent: {\n\\b:bool->( b )      )\n}; beginLocation: (column: 0 ; row: 20); endLocation: (column: 0 ; row: 20)\n\\b:bool->( b )      )"
    thrown.getMessage should equal (expected)
  }

  "RecognizeLexeme" should "work for tooMuchLeftBraces.rise" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/tooMuchLeftBraces.rise"
    val file:FileReader = new FileReader(fileName)
    val thrown = intercept[Exception] {
      new RecognizeLexeme(file)
    }
    val expected:String = "ErrorToken: Right Brace is missing! at FileReader: fileName: 'src/test/scala/riseParser/readFiles/filesToLexe/tooMuchLeftBraces.rise'; fileContent: {\n\\b:bool->(( b )\n}; beginLocation: (column: 0 ; row: 15); endLocation: (column: 0 ; row: 15)\n\\b:bool->(( b )"
    thrown.getMessage should equal (expected)
  }

  "RecognizeLexeme" should "complexInOneLine" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/complexInOneLine.rise"
    val file:FileReader = new FileReader(fileName)
    val lexer:RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {//\x:I32->\y->-(x*y)+42%5
      case  Right(I32(5, span20))::Right(BinOp(BinOpType.MOD, span19)):: Right(I32(42, span18))::Right(BinOp(BinOpType.ADD, span17))::Right(RBrace(span16)):: Right(Identifier(IdentifierType("y"), span15)):: Right(BinOp(BinOpType.MUL, span14))::Right(Identifier(IdentifierType("x"), span13)):: Right(LBrace(span12)):: Right(UnOp(UnaryOpType.NEG, span11)):: Right(Arrow(span10)):: Right(Identifier(IdentifierType("y"), span7)) :: Right(Backslash(span6)) ::Right(Arrow(span5)):: Right(Type(IntTyp(), span4)) :: Right(Dots(span3)) :: Right(Identifier(IdentifierType("x"), span2)) :: Right(Backslash(span1)) :: Nil => true
      case a => {
        val loc = Location(0,0)
        val loc2 = Location(0,1)
        val span = new Span(file, loc)
        val span2 = Span(file, loc, loc2)
//        val l = Right(I32(5, span))::Right(BinOp(BinOpType.MOD, span)):: Right(I32(42, span))::Right(BinOp(BinOpType.ADD, span))::Right(RBrace(span)):: Right(Identifier(IdentifierType("y"), span)):: Right(BinOp(BinOpType.MUL, span))::Right(Identifier(IdentifierType("x"), span)):: Right(LBrace(span)):: Right(UnOp(UnaryOpType.NEG, span)):: Right(Arrow(span2)):: Right(Typ(IntTyp(), span)) :: Right(Dots(span)) :: Right(Identifier(IdentifierType("y"), span)) :: Right(Backslash(span)) ::Right(Arrow(span2)):: Right(Typ(IntTyp(), span)) :: Right(Dots(span)) :: Right(Identifier(IdentifierType("x"), span)) :: Right(Backslash(span)) :: Nil
val l = Right(I32(5, span))::Right(BinOp(BinOpType.MOD, span)):: Right(I32(42, span))::Right(BinOp(BinOpType.ADD, span))::Right(RBrace(span)):: Right(Identifier(IdentifierType("y"), span)):: Right(BinOp(BinOpType.MUL, span))::Right(Identifier(IdentifierType("x"), span)):: Right(LBrace(span)):: Right(UnOp(UnaryOpType.NEG, span)):: Right(Arrow(span2)):: Right(Identifier(IdentifierType("y"), span)) :: Right(Backslash(span)) ::Right(Arrow(span2)):: Right(Type(IntTyp(), span)) :: Right(Dots(span)) :: Right(Identifier(IdentifierType("x"), span)) :: Right(Backslash(span)) :: Nil
        throw new Exception(a.toString() + "\n" + l.toString())
      }
    }
  }

  "RecognizeLexeme" should "complexInThreeLines" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/complexInThreeLines.rise"
    val file:FileReader = new FileReader(fileName)
    val lexer:RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case  Right(I32(5, _))::Right(BinOp(BinOpType.MOD, _)):: Right(I32(42, _))::Right(BinOp(BinOpType.ADD, _))::Right(RBrace(_)):: Right(Identifier(IdentifierType("y"), _)):: Right(BinOp(BinOpType.MUL, _))::Right(Identifier(IdentifierType("x"), _)):: Right(LBrace(_)):: Right(UnOp(UnaryOpType.NEG, _)):: Right(Arrow(_)):: Right(Type(IntTyp(), _)) :: Right(Dots(_)) :: Right(Identifier(IdentifierType("y"), _)) :: Right(Backslash(_)) ::Right(Arrow(_)):: Right(Type(IntTyp(), _)) :: Right(Dots(_)) :: Right(Identifier(IdentifierType("x"), _)) :: Right(Backslash(_)) :: Nil => true
      case a => {
        val loc = Location(0,0)
        val loc2 = Location(0,1)
        val span = new Span(file, loc)
        val span2 = Span(file, loc, loc2)
        val l = Right(I32(5, span))::Right(BinOp(BinOpType.MOD, span)):: Right(I32(42, span))::Right(BinOp(BinOpType.ADD, span))::Right(RBrace(span)):: Right(Identifier(IdentifierType("y"), span)):: Right(BinOp(BinOpType.MUL, span))::Right(Identifier(IdentifierType("x"), span)):: Right(LBrace(span)):: Right(UnOp(UnaryOpType.NEG, span)):: Right(Arrow(span2)):: Right(Type(IntTyp(), span)) :: Right(Dots(span)) :: Right(Identifier(IdentifierType("y"), span)) :: Right(Backslash(span)) ::Right(Arrow(span2)):: Right(Type(IntTyp(), span)) :: Right(Dots(span)) :: Right(Identifier(IdentifierType("x"), span)) :: Right(Backslash(span)) :: Nil
        throw new Exception(a.toString() + "\n" + l.toString())
      }
    }
  }

  "RecognizeLexeme" should "veryComplicated.rise" in {
    val fileName:String = "src/test/scala/riseParser/readFiles/filesToLexe/veryComplicated.rise"
    val file:FileReader = new FileReader(fileName)
    val lexer:RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case Right(RBrace(_))::Right(I32(0,_))::Right(BinOp(BinOpType.EQ, _))::Right(I32(42,_))::Right(BinOp(BinOpType.MOD, _))::Right(RBrace(_))::Right(F32(b,_))::Right(BinOp(BinOpType.DIV, _))::Right(RBrace(_))::Right(F32(a,_))::Right(BinOp(BinOpType.SUB, _))::Right(I32(25, _))::Right(LBrace(_))::Right(BinOp(BinOpType.MUL, _))::Right(Identifier(IdentifierType("varY"),_)):: Right(BinOp(BinOpType.MUL, _))::Right(Identifier(IdentifierType("varX"),_))::Right(Arrow(_)):: Right(Type(FloatTyp(), _)) :: Right(Dots(_)) :: Right(Identifier(IdentifierType("varY"), _)) :: Right(Backslash(_))::Right(Arrow(_)):: Right(Type(IntTyp(), _)) :: Right(Dots(_)) :: Right(Identifier(IdentifierType("varX"), _)) :: Right(Backslash(_))::Right(LBrace(_))::Right(LBrace(_))::Right(UnOp(UnaryOpType.NOT, _))::Right(Arrow(_)):: Right(Type(BoolType(), _)) :: Right(Dots(_)) :: Right(Identifier(IdentifierType("Heinrich"), _)) :: Right(Backslash(_))::Right(Arrow(_)):: Right(Type(BoolType(), _)) :: Right(Dots(_)) :: Right(Identifier(IdentifierType("Michael"), _)) :: Right(Backslash(_)) :: Nil =>{
        a==10.5 && b==2.3 //I can't write 2.3 directly in the pattern match, because then it would be unequal
      }
      case a => {
        val loc = Location(0,0)
        val loc2 = Location(0,1)
        val span = new Span(file, loc)
        val span2 = Span(file, loc, loc2)
        val l = Right(RBrace(span))::Right(I32(0,span))::Right(BinOp(BinOpType.EQ, span))::Right(I32(42,span))::Right(BinOp(BinOpType.MOD, span))::Right(RBrace(span))::Right(F32(2.3f,span))::Right(BinOp(BinOpType.DIV, span))::Right(RBrace(span))::Right(F32(10.5f,span))::Right(BinOp(BinOpType.SUB, span))::Right(I32(25, span))::Right(LBrace(span))::Right(BinOp(BinOpType.MUL, span))::Right(Identifier(IdentifierType("varY"),span)):: Right(BinOp(BinOpType.MUL, span))::Right(Identifier(IdentifierType("varX"),span))::Right(Arrow(span2)):: Right(Type(FloatTyp(), span)) :: Right(Dots(span)) :: Right(Identifier(IdentifierType("varY"), span)) :: Right(Backslash(span))::Right(Arrow(span2)):: Right(Type(IntTyp(), span)) :: Right(Dots(span)) :: Right(Identifier(IdentifierType("varX"), span)) :: Right(Backslash(span))::Right(LBrace(span))::Right(LBrace(span))::Right(UnOp(UnaryOpType.NOT, span))::Right(Arrow(span2)):: Right(Type(BoolType(), span)) :: Right(Dots(span)) :: Right(Identifier(IdentifierType("Heinrich"), span)) :: Right(Backslash(span))::Right(Arrow(span2)):: Right(Type(BoolType(), span)) :: Right(Dots(span)) :: Right(Identifier(IdentifierType("Michael"), span)) :: Right(Backslash(span)) :: Nil
        throw new Exception(a.toString() + "\n" + l.toString())
      }
    }
  }
}
