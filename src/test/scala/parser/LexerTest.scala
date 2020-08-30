package parser

import parser.lexer.OpType.{BinOpType, UnaryOpType}
import parser.lexer.FloatTyp
import parser.lexer.FloatTyp
import parser.lexer.{Arrow, Backslash, BinOp, BoolType, Dots, F32, FileReader, FloatTyp, I32, Identifier, IdentifierType, IntTyp, LBrace, Location, RBrace, RecognizeLexeme, Span, Type, UnOp}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.equal
import org.scalatest.matchers.should.Matchers._

class LexerTest extends  AnyFlatSpec {

  "RecognizeLexeme" should "work for the identity" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/identity.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case Right(Backslash(span4)) :: Right(Identifier(IdentifierType("x"), span3)) :: Right(Arrow(span2)) :: Right(Identifier(IdentifierType("x"), span1)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the constant42" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/constant42.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case Right(Backslash(span4)) :: Right(Identifier(IdentifierType("c"), span3)) :: Right(Arrow(span2)) :: Right(I32(42, span1)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the longIdentity" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/longIdentity.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case Right(Backslash(span4)) :: Right(Identifier(IdentifierType("Kevin"), span3)) :: Right(Arrow(span2)) :: Right(Identifier(IdentifierType("Kevin"), span1)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the identityWithI32" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/identityWithI32.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case Right(Backslash(span4)) :: Right(Identifier(IdentifierType("x"), span3)) :: Right(Dots(span0)) :: Right(Type(IntTyp(), span)) :: Right(Arrow(span2)) :: Right(Identifier(IdentifierType("x"), span1)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the longIdentityWithI32" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/longIdentityWithI32.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case Right(Backslash(span4)) :: Right(Identifier(IdentifierType("jens"), span3)) :: Right(Dots(span0)) :: Right(Type(IntTyp(), span)) :: Right(Arrow(span2)) :: Right(Identifier(IdentifierType("jens"), span1)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the ComplexIdentifier" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/ComplexIdentifier.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case Right(Backslash(span6)) :: Right(Identifier(IdentifierType("Hans_Georg"), span5)) ::  Right(Dots(span4)) ::Right(Type(FloatTyp(), span3)) :: Right(Arrow(span2)) :: Right(Identifier(IdentifierType("Hans_Georg"), span1)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for TypWith-" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/TypWith-.rise"
    val file: FileReader = new FileReader(fileName)
    val thrown = intercept[Exception] {
      new RecognizeLexeme(file)
    }
    val expected: String = "ErrorToken: It is an '->' expected. The Lexeme '--' is not an '->'! at FileReader: fileName: 'src/test/scala/parser/readFiles/filesToLexe/TypWith-.rise'; fileContent: {\n\\x:I32-->x\n}; beginLocation: (column: 0 ; row: 6); endLocation: (column: 0 ; row: 7)\n\\x:I32-̲->x"
    thrown.getMessage should equal(expected)
  }

  "RecognizeLexeme" should "work for noBacklashAtBeginning.rise" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/noBacklashAtBeginning.rise"
    val file: FileReader = new FileReader(fileName)
    val thrown = intercept[Exception] {
      new RecognizeLexeme(file)
    } //Todo: should we have a underline of the important code here too? yes, but here is no
    val expected: String = "ErrorToken: It is an '\\' expected. The Lexeme 'x' is not an '\\'! at FileReader: fileName: 'src/test/scala/parser/readFiles/filesToLexe/noBacklashAtBeginning.rise'; fileContent: {\nx:I32->x+5\n}; beginLocation: (column: 0 ; row: 0); endLocation: (column: 0 ; row: 0)\nx:I32->x+5"
    thrown.getMessage should equal(expected)
  }
  "RecognizeLexeme" should "work for plus" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/plus.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case Right(Backslash(span8)) :: Right(Identifier(IdentifierType("x"), span7)) :: Right(Dots(span6)) :: Right(Type(IntTyp(), span5)) :: Right(Arrow(span4)) :: Right(Identifier(IdentifierType("x"), span3)) :: Right(BinOp(BinOpType.ADD, span2)) :: Right(I32(5, span1)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for minus" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/minus.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case Right(Backslash(span8)) :: Right(Identifier(IdentifierType("x"), span7)) :: Right(Dots(span6)) :: Right(Type(IntTyp(), span5)) :: Right(Arrow(span4)) :: Right(Identifier(IdentifierType("x"), span3)) :: Right(BinOp(BinOpType.SUB, span2)) :: Right(I32(5, span1)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for negation" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/negation.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case Right(Backslash(span8)) :: Right(Identifier(IdentifierType("y"), span7)) :: Right(Dots(span6)) :: Right(Type(IntTyp(), span5)) :: Right(Arrow(span4)) :: Right(UnOp(UnaryOpType.NEG, span2)) :: Right(Identifier(IdentifierType("y"), span1)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for negationWithBool" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/negationWithBool.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case Right(Backslash(span8)) :: Right(Identifier(IdentifierType("b"), span7)) :: Right(Dots(span6)) :: Right(Type(BoolType(), span5)) :: Right(Arrow(span4)) :: Right(UnOp(UnaryOpType.NEG, span2)) :: Right(Identifier(IdentifierType("b"), span1)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for not" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/not.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case Right(Backslash(span8)) :: Right(Identifier(IdentifierType("b"), span7)) :: Right(Dots(span6)) :: Right(Type(BoolType(), span5)) :: Right(Arrow(span4)) :: Right(UnOp(UnaryOpType.NOT, span2)) :: Right(Identifier(IdentifierType("b"), span1)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for braces" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/braces.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case Right(Backslash(span8)) :: Right(Identifier(IdentifierType("b"), span7)) :: Right(Dots(span6)) :: Right(Type(BoolType(), span5)) :: Right(Arrow(span4)) :: Right(LBrace(span3)) :: Right(Identifier(IdentifierType("b"), span2)) :: Right(RBrace(span1)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for bracesWithNot" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/bracesWithNot.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case Right(Backslash(span8)) :: Right(Identifier(IdentifierType("b"), span7)) :: Right(Dots(span6)) :: Right(Type(BoolType(), span5)) :: Right(Arrow(span4)) :: Right(LBrace(span3)) :: Right(UnOp(UnaryOpType.NOT, span2)) :: Right(Identifier(IdentifierType("b"), span1)) :: Right(RBrace(span0)) :: Nil => true
      case a => throw new Exception(a.toString())
    }
  }

  "RecognizeLexeme" should "work for LeftBraceMissing.rise" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/LeftBraceMissing.rise"
    val file: FileReader = new FileReader(fileName)
    val thrown = intercept[Exception] {
      new RecognizeLexeme(file)
    } //Todo: should we have a underline of the important code here too? yes, but here is no
    val expected: String = "ErrorToken: Left Brace is missing! at FileReader: fileName: 'src/test/scala/parser/readFiles/filesToLexe/LeftBraceMissing.rise'; fileContent: {\n\\b:bool-> b )\n}; beginLocation: (column: 0 ; row: 12); endLocation: (column: 0 ; row: 12)\n\\b:bool-> b )"
    thrown.getMessage should equal(expected)
  }

  "RecognizeLexeme" should "work for RightBraceMissing.rise" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/RightBraceMissing.rise"
    val file: FileReader = new FileReader(fileName)
    val thrown = intercept[Exception] {
      new RecognizeLexeme(file)
    }
    val expected: String = "ErrorToken: Right Brace is missing! at FileReader: fileName: 'src/test/scala/parser/readFiles/filesToLexe/RightBraceMissing.rise'; fileContent: {\n\\b:bool->(   b\n}; beginLocation: (column: 0 ; row: 13); endLocation: (column: 0 ; row: 14)\n\\b:bool->(   b̲"
    thrown.getMessage should equal(expected)
  }

  "RecognizeLexeme" should "work for tooMuchRightBraces.rise" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/tooMuchRightBraces.rise"
    val file: FileReader = new FileReader(fileName)
    val thrown = intercept[Exception] {
      new RecognizeLexeme(file)
    } //Todo: should we have a underline of the important code here too? yes, but here is no
    val expected: String = "ErrorToken: Left Brace is missing! at FileReader: fileName: 'src/test/scala/parser/readFiles/filesToLexe/tooMuchRightBraces.rise'; fileContent: {\n\\b:bool->( b )      )\n}; beginLocation: (column: 0 ; row: 20); endLocation: (column: 0 ; row: 20)\n\\b:bool->( b )      )"
    thrown.getMessage should equal(expected)
  }

  "RecognizeLexeme" should "work for tooMuchLeftBraces.rise" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/tooMuchLeftBraces.rise"
    val file: FileReader = new FileReader(fileName)
    val thrown = intercept[Exception] {
      new RecognizeLexeme(file)
    }
    val expected: String = "ErrorToken: Right Brace is missing! at FileReader: fileName: 'src/test/scala/parser/readFiles/filesToLexe/tooMuchLeftBraces.rise'; fileContent: {\n\\b:bool->(( b )\n}; beginLocation: (column: 0 ; row: 15); endLocation: (column: 0 ; row: 15)\n\\b:bool->(( b )"
    thrown.getMessage should equal(expected)
  }

  "RecognizeLexeme" should "complexInOneLine" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/complexInOneLine.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match { //\x:I32->\y->-(x*y)+42%5
      case Right(Backslash(_)) :: Right(Identifier(IdentifierType("x"), _)) :: Right(Dots(_)) :: Right(Type(IntTyp(), _)) :: Right(Arrow(_)) :: Right(Backslash(_)) :: Right(Identifier(IdentifierType("y"), _)) :: Right(Arrow(_)) :: Right(UnOp(UnaryOpType.NEG, _)) :: Right(LBrace(_)) :: Right(Identifier(IdentifierType("x"), _)) :: Right(BinOp(BinOpType.MUL, _)) :: Right(Identifier(IdentifierType("y"), _)) :: Right(RBrace(_)) :: Right(BinOp(BinOpType.ADD, _)) :: Right(I32(42, _)) :: Right(BinOp(BinOpType.MOD, _)) :: Right(I32(5, _)) :: Nil => true
      case a => {
        val loc = Location(0, 0)
        val loc2 = Location(0, 1)
        val span = new Span(file, loc)
        val span2 = Span(file, loc, loc2)
        //        val l = Right(I32(5, span))::Right(BinOp(BinOpType.MOD, span)):: Right(I32(42, span))::Right(BinOp(BinOpType.ADD, span))::Right(RBrace(span)):: Right(Identifier(IdentifierType("y"), span)):: Right(BinOp(BinOpType.MUL, span))::Right(Identifier(IdentifierType("x"), span)):: Right(LBrace(span)):: Right(UnOp(UnaryOpType.NEG, span)):: Right(Arrow(span2)):: Right(Typ(IntTyp(), span)) :: Right(Dots(span)) :: Right(Identifier(IdentifierType("y"), span)) :: Right(Backslash(span)) ::Right(Arrow(span2)):: Right(Typ(IntTyp(), span)) :: Right(Dots(span)) :: Right(Identifier(IdentifierType("x"), span)) :: Right(Backslash(span)) :: Nil
        val l = Right(Backslash(span)) :: Right(Identifier(IdentifierType("x"), span)) :: Right(Dots(span)) :: Right(Type(IntTyp(), span)) :: Right(Arrow(span2)) :: Right(Backslash(span)) :: Right(Identifier(IdentifierType("y"), span)) :: Right(Arrow(span2)) :: Right(UnOp(UnaryOpType.NEG, span)) :: Right(LBrace(span)) :: Right(Identifier(IdentifierType("x"), span)) :: Right(BinOp(BinOpType.MUL, span)) :: Right(Identifier(IdentifierType("y"), span)) :: Right(RBrace(span)) :: Right(BinOp(BinOpType.ADD, span)) :: Right(I32(42, span)) :: Right(BinOp(BinOpType.MOD, span)) :: Right(I32(5, span)) :: Nil
        throw new Exception(a.toString() + "\n" + l.toString())
      }
    }
  }

  "RecognizeLexeme" should "complexInThreeLines" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/complexInThreeLines.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case Right(Backslash(_)) :: Right(Identifier(IdentifierType("x"), _)) :: Right(Dots(_)) :: Right(Type(IntTyp(), _)) :: Right(Arrow(_)) :: Right(Backslash(_)) :: Right(Identifier(IdentifierType("y"), _)) :: Right(Dots(_)) :: Right(Type(IntTyp(), _)) :: Right(Arrow(_)) :: Right(UnOp(UnaryOpType.NEG, _)) :: Right(LBrace(_)) :: Right(Identifier(IdentifierType("x"), _)) :: Right(BinOp(BinOpType.MUL, _)) :: Right(Identifier(IdentifierType("y"), _)) :: Right(RBrace(_)) :: Right(BinOp(BinOpType.ADD, _)) :: Right(I32(42, _)) :: Right(BinOp(BinOpType.MOD, _)) :: Right(I32(5, _)) :: Nil => true
      case a => {
        val loc = Location(0, 0)
        val loc2 = Location(0, 1)
        val span = new Span(file, loc)
        val span2 = Span(file, loc, loc2)
        val l = Right(Backslash(span)) :: Right(Identifier(IdentifierType("x"), span)) :: Right(Dots(span)) :: Right(Type(IntTyp(), span)) :: Right(Arrow(span2)) :: Right(Backslash(span)) :: Right(Identifier(IdentifierType("y"), span)) :: Right(Dots(span)) :: Right(Type(IntTyp(), span)) :: Right(Arrow(span2)) :: Right(UnOp(UnaryOpType.NEG, span)) :: Right(LBrace(span)) :: Right(Identifier(IdentifierType("x"), span)) :: Right(BinOp(BinOpType.MUL, span)) :: Right(Identifier(IdentifierType("y"), span)) :: Right(RBrace(span)) :: Right(BinOp(BinOpType.ADD, span)) :: Right(I32(42, span)) :: Right(BinOp(BinOpType.MOD, span)) :: Right(I32(5, span)) :: Nil
        throw new Exception(a.toString() + "\n" + l.toString())
      }
    }
  }

  "RecognizeLexeme" should "veryComplicated.rise" in {
    val fileName: String = "src/test/scala/parser/readFiles/filesToLexe/veryComplicated.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    lexer.tokens match {
      case Right(Backslash(_)) :: Right(Identifier(IdentifierType("Michael"), _)) :: Right(Dots(_)) :: Right(Type(BoolType(), _)) :: Right(Arrow(_)) :: Right(Backslash(_)) :: Right(Identifier(IdentifierType("Heinrich"), _)) :: Right(Dots(_)) :: Right(Type(BoolType(), _)) :: Right(Arrow(_)) :: Right(UnOp(UnaryOpType.NOT, _)) :: Right(LBrace(_)) :: Right(LBrace(_)) :: Right(Backslash(_)) :: Right(Identifier(IdentifierType("varX"), _)) :: Right(Dots(_)) :: Right(Type(IntTyp(), _)) :: Right(Arrow(_)) :: Right(Backslash(_)) :: Right(Identifier(IdentifierType("varY"), _)) :: Right(Dots(_)) :: Right(Type(FloatTyp(), _)) :: Right(Arrow(_)) :: Right(Identifier(IdentifierType("varX"), _)) :: Right(BinOp(BinOpType.MUL, _)) :: Right(Identifier(IdentifierType("varY"), _)) :: Right(BinOp(BinOpType.MUL, _)) :: Right(LBrace(_)) :: Right(I32(25, _)) :: Right(BinOp(BinOpType.SUB, _)) :: Right(F32(a, _)) :: Right(RBrace(_)) :: Right(BinOp(BinOpType.DIV, _)) :: Right(F32(b, _)) :: Right(RBrace(_)) :: Right(BinOp(BinOpType.MOD, _)) :: Right(I32(42, _)) :: Right(BinOp(BinOpType.EQ, _)) :: Right(I32(0, _)) :: Right(RBrace(_)) :: Nil => {
        a == 10.5 && b == 2.3 //I can't write 2.3 directly in the pattern match, because then it would be unequal
      }
      case a => {
        val l = "Right(Backslash(_)) :: Right(Identifier(IdentifierType(\"Michael\"), _)) ::Right(Dots(_)) ::  Right(Type(BoolType(), _)) :: Right(Arrow(_)):: Right(Backslash(_))::Right(Identifier(IdentifierType(\"Heinrich\"), _)) :: Right(Dots(_)) :: Right(Type(BoolType(), _)) :: Right(Arrow(_)):: Right(UnOp(UnaryOpType.NOT, _))::Right(LBrace(_))::Right(LBrace(_))::Right(Backslash(_))::Right(Identifier(IdentifierType(\"varX\"), _)) ::Right(Dots(_)) ::  Right(Type(IntTyp(), _)) :: Right(Arrow(_))::  Right(Backslash(_))::Right(Identifier(IdentifierType(\"varY\"), _)) ::Right(Dots(_)) ::Right(Type(FloatTyp(), _)) ::  Right(Arrow(_)):: Right(Identifier(IdentifierType(\"varX\"),_))::Right(BinOp(BinOpType.MUL, _))::Right(Identifier(IdentifierType(\"varY\"),_)):: Right(BinOp(BinOpType.MUL, _))::Right(LBrace(_))::Right(I32(25, _))::Right(BinOp(BinOpType.SUB, _))::Right(F32(a,_))::Right(RBrace(_))::Right(BinOp(BinOpType.DIV, _))::Right(F32(b,_))::Right(RBrace(_))::Right(BinOp(BinOpType.MOD, _))::Right(I32(42,_))::Right(BinOp(BinOpType.EQ, _))::Right(RBrace(_))::Right(I32(0,_)):: Nil"
        throw new Exception(a.toString() + "\n" + l.toString())
      }
    }
  }
}