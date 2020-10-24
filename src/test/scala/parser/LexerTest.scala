package parser

import OpType.{BinOpType, UnaryOpType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.equal
import org.scalatest.matchers.should.Matchers._

class LexerTest extends  AnyFlatSpec {
  val testFilePath = "src/test/scala/parser/readFiles/filesToLex/"

  "RecognizeLexeme" should "work for the Brace5" in {
    val fileName: String = testFilePath + "Brace5.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(IntTyp(), _) :: Arrow(_) :: Type(IntTyp(), _)::
        EndTypAnnotatedIdent(_) :: BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::Backslash(_)::Identifier("x",_)::Arrow(_)::LBrace(_)::
        Identifier("x",_)::RBrace(_) ::EndNamedExpr(_)
        :: Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for braces" in {
    val fileName: String = testFilePath + "braces.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(BoolType(), _) :: Arrow(_) :: Type(BoolType(), _)::
        EndTypAnnotatedIdent(_) :: BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::Backslash(_) :: Identifier("b", _) :: Arrow(_) ::
        LBrace(_) :: Identifier("b", _)
        :: RBrace(_)
        ::EndNamedExpr(_) :: Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for bracesWithNot" in {
    val fileName: String = testFilePath + "bracesWithNot.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(BoolType(), _) :: Arrow(_) :: Type(BoolType(), _)::
        EndTypAnnotatedIdent(_) :: BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::Backslash(_) :: Identifier("b", _) :: Arrow(_) ::
        LBrace(_) :: UnOp(UnaryOpType.NOT, _)::Identifier("b", _)
        :: RBrace(_)
        ::EndNamedExpr(_) :: Nil => true
      case a => fail(a.toString())
    }
  }


  "RecognizeLexeme" should "Complex1" in {
    val fileName: String = testFilePath + "Complex1.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(DoubleType(), _) :: Arrow(_) :: Type(DoubleType(), _)::
        Arrow(_) :: Type(DoubleType(), _)::Arrow(_) :: Type(DoubleType(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::Backslash(_) :: Identifier("x", _) :: Arrow(_) ::
        Backslash(_) :: Identifier("y", _) :: Arrow(_):: Backslash(_) ::
        Identifier("z", _) :: Arrow(_)::
        BinOp(BinOpType.MUL, _):: Identifier("x", _)::
        LBrace(_):: BinOp(BinOpType.ADD, _):: Identifier("y", _):: Identifier("z", _)::
        RBrace(_):: EndNamedExpr(_)::Nil =>
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "Complex2" in {
    val fileName: String = testFilePath + "Complex2.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(FloatTyp(), _) :: Arrow(_) :: Type(FloatTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) ::
        BinOp(BinOpType.MUL, _):: LBrace(_) :: BinOp(BinOpType.ADD, _):: Identifier("x", _):: Identifier("x", _)::
        RBrace(_):: Identifier("x", _):: EndNamedExpr(_)::Nil =>
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "Complex3" in {
    val fileName: String = testFilePath + "Complex3.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(ShortTyp(), _) :: Arrow(_) :: Type(ShortTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) ::
        BinOp(BinOpType.MUL, _):: LBrace(_) :: BinOp(BinOpType.ADD, _):: Identifier("x", _):: Identifier("x", _)::
        RBrace(_):: Identifier("x", _):: EndNamedExpr(_)::Nil =>
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "Complex4" in {
    val fileName: String = testFilePath + "Complex4.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(FloatTyp(), _) :: Arrow(_) :: Type(FloatTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) ::
        BinOp(BinOpType.DIV, _):: LBrace(_):: BinOp(BinOpType.MUL, _)::
        Identifier("x", _):: Identifier("x", _):: RBrace(_)::
        LBrace(_):: BinOp(BinOpType.MUL, _)::Identifier("x", _):: Identifier("x", _)::
        RBrace(_):: EndNamedExpr(_)::Nil =>
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "Complex5" in {
    val fileName: String = testFilePath + "Complex5.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(FloatTyp(), _) :: Arrow(_) :: Type(FloatTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) ::
        BinOp(BinOpType.DIV, _):: LBrace(_):: BinOp(BinOpType.MUL, _)::
        Identifier("x", _):: Identifier("x", _):: RBrace(_)::
        LBrace(_):: LBrace(_):: BinOp(BinOpType.MUL, _)::Identifier("x", _)::
        RBrace(_):: Identifier("x", _)::RBrace(_)::EndNamedExpr(_):: Nil =>
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "Complex6" in {
    val fileName: String = testFilePath + "Complex6.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(FloatTyp(), _) :: Arrow(_) :: Type(FloatTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) ::
        BinOp(BinOpType.DIV, _):: LBrace(_):: LBrace(_):: BinOp(BinOpType.MUL, _)::
        Identifier("x", _):: RBrace(_):: Identifier("x", _):: RBrace(_)::
        LBrace(_):: BinOp(BinOpType.MUL, _)::Identifier("x", _):: Identifier("x", _)::
        RBrace(_):: EndNamedExpr(_)::Nil =>
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "Complex7" in {
    val fileName: String = testFilePath + "Complex7.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(FloatTyp(), _) :: Arrow(_) :: Type(FloatTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) ::
        LBrace(_):: BinOp(BinOpType.DIV, _):: LBrace(_):: LBrace(_)::
        BinOp(BinOpType.MUL, _)::Identifier("x", _)::RBrace(_)::  Identifier("x", _)::
        RBrace(_):: LBrace(_):: LBrace(_):: BinOp(BinOpType.MUL, _)::Identifier("x", _)
        ::RBrace(_)::  Identifier("x", _)::RBrace(_)::RBrace(_)::EndNamedExpr(_):: Nil =>
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the ComplexIdentifier" in {
    val fileName: String = testFilePath + "ComplexIdentifier.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(FloatTyp(), _) :: Arrow(_) :: Type(FloatTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(span6) :: Identifier("Hans_Georg", _) ::Arrow(_) ::
        Identifier("Hans_Georg", _) :: EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "complexInOneLine" in {
    val fileName: String = testFilePath + "complexInOneLine.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(IntTyp(), _) :: Arrow(_) :: Type(IntTyp(), _)::
        Arrow(_) :: Type(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) :: Backslash(_) ::
        Identifier("y", _) :: Arrow(_) :: UnOp(UnaryOpType.NEG, _) :: LBrace(_)::
        BinOp(BinOpType.ADD, _) :: LBrace(_) :: BinOp(BinOpType.MUL, _) ::
        Identifier("x", _)  :: Identifier("y", _) :: RBrace(_) :: LBrace(_)::
        BinOp(BinOpType.MOD, _) :: I32(42, _) :: I32(5, _) :: RBrace(_):: RBrace(_)::
        EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "complexInThreeLines" in {
    val fileName: String = testFilePath + "complexInThreeLines.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(IntTyp(), _) :: Arrow(_) :: Type(IntTyp(), _)::
        Arrow(_) :: Type(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) :: Backslash(_) ::
        Identifier("y", _) :: Arrow(_) :: UnOp(UnaryOpType.NEG, _) ::
        BinOp(BinOpType.ADD, _) :: LBrace(_) :: BinOp(BinOpType.MUL, _) ::
        Identifier("x", _)  :: Identifier("y", _) :: RBrace(_) ::
        BinOp(BinOpType.MOD, _) :: I32(42, _) :: I32(5, _) ::
        EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the constant42" in {
    val fileName: String = testFilePath + "constant42.rise"
    val file: FileReader = FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(IntTyp(), _) :: Arrow(_) :: Type(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("c", _) :: Arrow(_) ::
        I32(42, _) :: EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the FunctionInBraces" in {
    val fileName: String = testFilePath + "FunctionInBraces.rise"
    val file: FileReader = FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(IntTyp(), _) :: Arrow(_) :: Type(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) ::
        LBrace(_)::Backslash(_):: Identifier("y", _):: Colon(_) ::Type(IntTyp(),_)::
        Arrow(_)::Identifier("y", _)::RBrace(_):: Identifier("x",_)
       :: EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the fx" in {
    val fileName: String = testFilePath + "fx.rise"
    val file: FileReader = FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(IntTyp(), _) :: Arrow(_) :: Type(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) ::
        Identifier("fkt",_) :: Identifier("x",_)
        :: EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the identity" in {
    val fileName: String = testFilePath + "identity.rise"
    val file: FileReader =  FileReader(fileName)
    val thrown = intercept[RuntimeException] {
      RecognizeLexeme(file)
    }
    thrown.getMessage should equal("You can't start with a NamedExpr")
  }

  "RecognizeLexeme" should "work for the identityWithI32" in {
    val fileName: String = testFilePath + "identityWithI32.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(IntTyp(), _) :: Arrow(_) :: Type(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) ::
        Identifier("x", _) :: EndNamedExpr(_):: Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "lessComplexInOneLine" in {
    val fileName: String = testFilePath + "lessComplexInOneLine.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(IntTyp(), _) :: Arrow(_) :: Type(IntTyp(), _)::
        Arrow(_) :: Type(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) :: Backslash(_) ::
        Identifier("y", _)  :: Arrow(_) :: UnOp(UnaryOpType.NEG, _) :: LBrace(_) ::
        BinOp(BinOpType.MUL, _) :: Identifier("x", _)  :: Identifier("y", _) ::
        RBrace(_) :: EndNamedExpr(_):: Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "lessComplexInOneLineWithDifferentType" in {
    val fileName: String = testFilePath + "lessComplexInOneLineWithDifferentType.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(IntTyp(), _) :: Arrow(_) :: Type(FloatTyp(), _)::
        Arrow(_) :: Type(FloatTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) :: Backslash(_) ::
        Identifier("y", _)  :: Arrow(_) :: UnOp(UnaryOpType.NEG, _) :: LBrace(_) ::
        BinOp(BinOpType.MUL, _) :: Identifier("x", _)  :: Identifier("y", _) ::
        RBrace(_) :: EndNamedExpr(_):: Nil => true      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "littleComplexLine" in {
    val fileName: String = testFilePath + "littleComplexLine.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(IntTyp(), _) :: Arrow(_) :: Type(IntTyp(), _)::
        Arrow(_) :: Type(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) :: Backslash(_) ::
        Identifier("y", _) ::  Arrow(_) ::  BinOp(BinOpType.ADD, _) ::
        LBrace(_) :: BinOp(BinOpType.MUL, _) :: Identifier("x", _)  ::
        Identifier("y", _) :: RBrace(_) :: I32(42, _) :: EndNamedExpr(_):: Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the longIdentity" in {
    val fileName: String = testFilePath + "longIdentity.rise"
    val file: FileReader = FileReader(fileName)
    val thrown = intercept[RuntimeException] {
      RecognizeLexeme(file)
    }
    thrown.getMessage should equal("You can't start with a NamedExpr")
  }

  "RecognizeLexeme" should "work for the longIdentityWithI32" in {
    val fileName: String = testFilePath + "longIdentityWithI32.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(IntTyp(), _) ::
        Arrow(_) :: Type(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("jens", _) :: Arrow(_) :: Identifier("jens", _)
        :: EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for minus" in {
    val fileName: String = testFilePath + "minus.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(IntTyp(), _) ::
        Arrow(_) :: Type(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Colon(_) :: Type(IntTyp(), _) ::
        Arrow(_) :: BinOp(BinOpType.SUB, _) :: Identifier("x", _)  :: I32(5, _) ::
        EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for negation" in {
    val fileName: String = testFilePath + "negation.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(IntTyp(), _) ::
        Arrow(_) :: Type(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("y", _) :: Arrow(_) :: UnOp(UnaryOpType.NEG, _)
        :: Identifier("y", _) :: EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for negationWithoutIdentifierAtBeginning" in {
    val fileName: String = testFilePath + "negationWithoutIdentifierAtBeginning.rise"
    val file: FileReader =  FileReader(fileName)
    val thrown = intercept[RuntimeException] {
      RecognizeLexeme(file)
    }
    thrown.getMessage should equal("Here should be an Identifier, but whitout an Identifier nothing new can be started")
  }

  "RecognizeLexeme" should "work for negationWithBool" in {
    val fileName: String = testFilePath + "negationWithBool.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(BoolType(), _) ::
        Arrow(_) :: Type(BoolType(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("b", _) :: Arrow(_) :: UnOp(UnaryOpType.NEG, _)
        :: Identifier("b", _) :: EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }

  "parser" should "not be able to parse 'noExpression.rise'" in {
    val fileName: String = testFilePath + "noExpression.rise"
    val file: FileReader = new FileReader(fileName)
    val thrown = intercept[RuntimeException] {
      RecognizeLexeme(file)
    }
    thrown.getMessage should equal("Here is at the Beginning a Identifier expected, but here is no Identifier!")
  }

  "RecognizeLexeme" should "work for noIdentityAndEqualSignAtBeginning.rise" in {
    val fileName: String = testFilePath + "noIdentityAndEqualSignAtBeginning.rise"
    val file: FileReader =  FileReader(fileName)
    val thrown = intercept[Exception] {
      RecognizeLexeme(file)
    } //Todo: should we have a underline of the important code here too? yes, but here is no
    val expected: String = "Here should be an '::' or '=', but whitout this nothing new can be started"
    thrown.getMessage should equal(expected)
  }

  "RecognizeLexeme" should "work for not" in {
    val fileName: String = testFilePath + "not.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(BoolType(), _) ::
        Arrow(_) :: Type(BoolType(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("b", _) :: Arrow(_) :: UnOp(UnaryOpType.NOT, _)
        :: Identifier("b", _) :: EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for plus" in {
    val fileName: String = testFilePath + "plus.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(IntTyp(), _) ::
        Arrow(_) :: Type(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _)  ::
        Arrow(_) :: BinOp(BinOpType.ADD, _) :: Identifier("x", _)  :: I32(5, _) ::
        EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for twoplus1extraDefintion" in {
    val fileName: String = testFilePath + "twoplus1extraDefintion.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(IntTyp(), _) ::
        Arrow(_) :: Type(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::

        BeginTypAnnotatedIdent(_):: Identifier("h", _)::
        DoubleColons(_) :: Type(IntTyp(), _) ::
        Arrow(_) :: LBrace(_) :: Type(IntTyp(), _):: Arrow(_) :: Type(IntTyp(), _) ::
        RBrace(_) :: Arrow(_) :: Type(IntTyp(), _):: EndTypAnnotatedIdent(_) ::

        BeginNamedExpr(_) :: Identifier("h", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _)  ::
        Arrow(_) :: Backslash(_) :: Identifier("fkt", _)  ::
        Arrow(_) :: Identifier("fkt", _) :: Identifier("x", _) ::
        EndNamedExpr(_)::

        BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _)  ::
        Arrow(_) :: BinOp(BinOpType.ADD, _) :: Identifier("x", _)  :: I32(5, _) ::
        EndNamedExpr(_)::

        BeginTypAnnotatedIdent(_):: Identifier("z", _)::
        DoubleColons(_) :: Type(IntTyp(), _) ::
        Arrow(_) :: Type(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for twoSimpleFunctionsInDifferentOrder" in {
    val fileName: String = testFilePath + "twoSimpleFunctionsInDifferentOrder.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(IntTyp(), _) ::
        Arrow(_) :: Type(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::

        BeginNamedExpr(_) :: Identifier("h", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _)  ::
        Arrow(_) :: Backslash(_) :: Identifier("fkt", _)  ::
        Arrow(_) :: Identifier("fkt", _) :: Identifier("x", _) ::
        EndNamedExpr(_)::

        BeginTypAnnotatedIdent(_):: Identifier("h", _)::
        DoubleColons(_) :: Type(IntTyp(), _) ::
        Arrow(_) :: LBrace(_) :: Type(IntTyp(), _):: Arrow(_) :: Type(IntTyp(), _) ::
        RBrace(_) :: Arrow(_) :: Type(IntTyp(), _):: EndTypAnnotatedIdent(_) ::

        BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _)  ::
        Arrow(_) :: BinOp(BinOpType.ADD, _) :: Identifier("x", _)  :: I32(5, _) ::
        EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }


  "RecognizeLexeme" should "work for twoSimpleFunctions" in {
    val fileName: String = testFilePath + "twoSimpleFunctions.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(IntTyp(), _) ::
        Arrow(_) :: Type(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::

        BeginTypAnnotatedIdent(_):: Identifier("h", _)::
        DoubleColons(_) :: Type(IntTyp(), _) ::
        Arrow(_) :: LBrace(_) :: LBrace(_) :: Type(IntTyp(), _):: Arrow(_) :: Type(IntTyp(), _) ::
        RBrace(_) :: Arrow(_) :: Type(IntTyp(), _):: RBrace(_):: EndTypAnnotatedIdent(_) ::

        BeginNamedExpr(_) :: Identifier("h", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _)  ::
        Arrow(_) :: LBrace(_)::Backslash(_) :: Identifier("fkt", _)  ::
        Arrow(_) :: LBrace(_):: Identifier("fkt", _) :: Identifier("x", _) ::
        RBrace(_)::RBrace(_)::
        EndNamedExpr(_)::

        BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("y", _)  ::
        Arrow(_) :: BinOp(BinOpType.ADD, _) :: Identifier("y", _)  :: I32(5, _) ::
        EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }


  "RecognizeLexeme" should "work for twoSimpleFunctionsButWithSameLocalVarName" in {
    val fileName: String = testFilePath + "twoSimpleFunctionsButWithSameLocalVarName.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(IntTyp(), _) ::
        Arrow(_) :: Type(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::

        BeginTypAnnotatedIdent(_):: Identifier("h", _)::
        DoubleColons(_) :: Type(IntTyp(), _) ::
        Arrow(_) :: LBrace(_) :: LBrace(_) :: Type(IntTyp(), _):: Arrow(_) :: Type(IntTyp(), _) ::
        RBrace(_) :: Arrow(_) :: Type(IntTyp(), _):: RBrace(_):: EndTypAnnotatedIdent(_) ::

        BeginNamedExpr(_) :: Identifier("h", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _)  ::
        Arrow(_) :: LBrace(_)::Backslash(_) :: Identifier("fkt", _)  ::
        Arrow(_) :: LBrace(_):: Identifier("fkt", _) :: Identifier("x", _) ::
        RBrace(_)::RBrace(_)::
        EndNamedExpr(_)::

        BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _)  ::
        Arrow(_) :: BinOp(BinOpType.ADD, _) :: Identifier("x", _)  :: I32(5, _) ::
        EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for TypWith-" in {
    val fileName: String = testFilePath + "TypWith-.rise"
    val file: FileReader =  FileReader(fileName)
    val thrown = intercept[Exception] {
      RecognizeLexeme(file)
    }
    val expected: String = "ErrorToken: It is an '->' expected. The Lexeme '--' is not an '->'! at FileReader: " +
      "fileName: 'src/test/scala/parser/readFiles/filesToLex/TypWith-.rise'; fileContent: {\n" +
      "f::I32->I32f=\\x-->x\n}; beginLocation: (column: 1 ; row: 4); endLocation: (column: 1 ; row: 5)\nf=\\x-̲->x"
    thrown.getMessage should equal(expected)
  }

  "RecognizeLexeme" should "veryComplicated.rise" in {
    val fileName: String = testFilePath + "veryComplicated.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Type(BoolType(), _)::
        Arrow(_) :: Type(BoolType(), _)::
        Arrow(_) :: Type(IntTyp(), _)::
        Arrow(_) :: Type(FloatTyp(), _)::
        Arrow(_) :: Type(FloatTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("Michael", _) :: Arrow(_) :: Backslash(_) ::
        Identifier("Heinrich", _) :: Arrow(_) :: UnOp(UnaryOpType.NOT, _) ::
        LBrace(_) :: BinOp(BinOpType.EQ, _) :: BinOp(BinOpType.MOD, _) ::
        LBrace(_) :: Backslash(_) :: Identifier("varX", _) ::
        Arrow(_) :: Backslash(_) :: Identifier("varY", _) ::
        Arrow(_) :: BinOp(BinOpType.MUL, _) :: Identifier("varX", _)  ::
        BinOp(BinOpType.MUL, _) :: Identifier("varY", _)  :: BinOp(BinOpType.DIV, _)
        :: LBrace(_) :: BinOp(BinOpType.SUB, _) :: I32(25, _) :: F32(a, _) :: RBrace(_)
        ::  F32(b, _) :: RBrace(_) :: I32(42, _)  :: I32(0, _) :: RBrace(_) ::
        EndNamedExpr(_)::Nil => {
        a == 10.5 && b == 2.3 //I can't write 2.3 directly in the pattern match, because then it would be unequal
      }
      case a => fail(a.toString())
    }
  }

}