package parser

import OpType.{BinOpType, UnaryOpType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.equal
import org.scalatest.matchers.should.Matchers._

class LexerTest extends  AnyFlatSpec {
  val testFilePath = "src/test/scala/parser/readFiles/filesToLex/"
  
  "RecognizeLexeme" should "work for the arrayType" in {
    val fileName: String = testFilePath + "arrayType.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(ArrayType(NatNumber(5), IntTyp()), _):: Arrow(_)::ScalarType(IntTyp(), _) ::
        Arrow(_) :: ScalarType(IntTyp(), _)::

        EndTypAnnotatedIdent(_) :: BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::Backslash(_)::Identifier("a",_)::Arrow(_)::
        Backslash(_)::Identifier("x",_)::Arrow(_)::
        Identifier("a",_)::EndNamedExpr(_)
        :: Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the Brace5" in {
    val fileName: String = testFilePath + "Brace5.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(IntTyp(), _) :: Arrow(_) :: ScalarType(IntTyp(), _)::
        EndTypAnnotatedIdent(_) :: BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::Backslash(_)::Identifier("x",_)::Arrow(_)::LParentheses(_)::
        Identifier("x",_)::RParentheses(_) ::EndNamedExpr(_)
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
        DoubleColons(_) :: ScalarType(BoolType(), _) :: Arrow(_) :: ScalarType(BoolType(), _)::
        EndTypAnnotatedIdent(_) :: BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::Backslash(_) :: Identifier("b", _) :: Arrow(_) ::
        LParentheses(_) :: Identifier("b", _)
        :: RParentheses(_)
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
        DoubleColons(_) :: ScalarType(BoolType(), _) :: Arrow(_) :: ScalarType(BoolType(), _)::
        EndTypAnnotatedIdent(_) :: BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::Backslash(_) :: Identifier("b", _) :: Arrow(_) ::
        LParentheses(_) :: UnOp(UnaryOpType.NOT, _)::Identifier("b", _)
        :: RParentheses(_)
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
        DoubleColons(_) :: ScalarType(DoubleType(), _) :: Arrow(_) :: ScalarType(DoubleType(), _)::
        Arrow(_) :: ScalarType(DoubleType(), _)::Arrow(_) :: ScalarType(DoubleType(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) ::

        Identifier("f", _) ::
        EqualsSign(_)::Backslash(_) :: Identifier("x", _) :: Arrow(_) ::
        Backslash(_) :: Identifier("y", _) :: Arrow(_):: Backslash(_) ::
        Identifier("z", _) :: Arrow(_)::
        BinOp(BinOpType.MUL, _):: Identifier("x", _)::
        LParentheses(_):: BinOp(BinOpType.ADD, _):: Identifier("y", _):: Identifier("z", _)::
        RParentheses(_):: EndNamedExpr(_)::Nil =>
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "Complex2" in {
    val fileName: String = testFilePath + "Complex2.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(FloatTyp(), _) :: Arrow(_) :: ScalarType(FloatTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) ::
        BinOp(BinOpType.MUL, _):: LParentheses(_) :: BinOp(BinOpType.ADD, _):: Identifier("x", _):: Identifier("x", _)::
        RParentheses(_):: Identifier("x", _):: EndNamedExpr(_)::Nil =>
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "Complex3" in {
    val fileName: String = testFilePath + "Complex3.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(ShortTyp(), _) :: Arrow(_) :: ScalarType(ShortTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) ::
        BinOp(BinOpType.MUL, _):: LParentheses(_) :: BinOp(BinOpType.ADD, _):: Identifier("x", _):: Identifier("x", _)::
        RParentheses(_):: Identifier("x", _):: EndNamedExpr(_)::Nil =>
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "Complex4" in {
    val fileName: String = testFilePath + "Complex4.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(FloatTyp(), _) :: Arrow(_) :: ScalarType(FloatTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) ::
        BinOp(BinOpType.DIV, _):: LParentheses(_):: BinOp(BinOpType.MUL, _)::
        Identifier("x", _):: Identifier("x", _):: RParentheses(_)::
        LParentheses(_):: BinOp(BinOpType.MUL, _)::Identifier("x", _):: Identifier("x", _)::
        RParentheses(_):: EndNamedExpr(_)::Nil =>
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "Complex5" in {
    val fileName: String = testFilePath + "Complex5.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(FloatTyp(), _) :: Arrow(_) :: ScalarType(FloatTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) ::
        BinOp(BinOpType.DIV, _):: LParentheses(_):: BinOp(BinOpType.MUL, _)::
        Identifier("x", _):: Identifier("x", _):: RParentheses(_)::
        LParentheses(_):: LParentheses(_):: BinOp(BinOpType.MUL, _)::Identifier("x", _)::
        RParentheses(_):: Identifier("x", _)::RParentheses(_)::EndNamedExpr(_):: Nil =>
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "Complex6" in {
    val fileName: String = testFilePath + "Complex6.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(FloatTyp(), _) :: Arrow(_) :: ScalarType(FloatTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) ::
        BinOp(BinOpType.DIV, _):: LParentheses(_):: LParentheses(_):: BinOp(BinOpType.MUL, _)::
        Identifier("x", _):: RParentheses(_):: Identifier("x", _):: RParentheses(_)::
        LParentheses(_):: BinOp(BinOpType.MUL, _)::Identifier("x", _):: Identifier("x", _)::
        RParentheses(_):: EndNamedExpr(_)::Nil =>
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "Complex7" in {
    val fileName: String = testFilePath + "Complex7.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(FloatTyp(), _) :: Arrow(_) :: ScalarType(FloatTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) ::
        LParentheses(_):: BinOp(BinOpType.DIV, _):: LParentheses(_):: LParentheses(_)::
        BinOp(BinOpType.MUL, _)::Identifier("x", _)::RParentheses(_)::  Identifier("x", _)::
        RParentheses(_):: LParentheses(_):: LParentheses(_):: BinOp(BinOpType.MUL, _)::Identifier("x", _)
        ::RParentheses(_)::  Identifier("x", _)::RParentheses(_)::RParentheses(_)::EndNamedExpr(_):: Nil =>
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the ComplexIdentifier" in {
    val fileName: String = testFilePath + "ComplexIdentifier.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(FloatTyp(), _) :: Arrow(_) :: ScalarType(FloatTyp(), _)::
        EndTypAnnotatedIdent(_) ::

        BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(span6) :: Identifier("hans_Georg", _) ::Arrow(_) ::
        Identifier("hans_Georg", _) :: EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "complexInOneLine" in {
    val fileName: String = testFilePath + "complexInOneLine.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(IntTyp(), _) :: Arrow(_) :: ScalarType(IntTyp(), _)::
        Arrow(_) :: ScalarType(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::

        BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) :: Backslash(_) ::
        Identifier("y", _) :: Arrow(_) :: UnOp(UnaryOpType.NEG, _) :: LParentheses(_)::
        BinOp(BinOpType.ADD, _) :: LParentheses(_) :: BinOp(BinOpType.MUL, _) ::
        Identifier("x", _)  :: Identifier("y", _) :: RParentheses(_) :: LParentheses(_)::
        BinOp(BinOpType.MOD, _) :: I32(42, _) :: I32(5, _) :: RParentheses(_):: RParentheses(_)::
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
        DoubleColons(_) :: ScalarType(IntTyp(), _) :: Arrow(_) :: ScalarType(IntTyp(), _)::
        Arrow(_) :: ScalarType(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::

        BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) :: Backslash(_) ::
        Identifier("y", _) :: Arrow(_) :: UnOp(UnaryOpType.NEG, _) :: LParentheses(_)::
        BinOp(BinOpType.ADD, _) :: LParentheses(_) :: BinOp(BinOpType.MUL, _) ::
        Identifier("x", _)  :: Identifier("y", _) :: RParentheses(_) :: LParentheses(_)::
        BinOp(BinOpType.MOD, _) :: I32(42, _) :: I32(5, _) :: RParentheses(_):: RParentheses(_)::
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
        DoubleColons(_) :: ScalarType(IntTyp(), _) :: Arrow(_) :: ScalarType(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) ::

        Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("c", _) :: Arrow(_) ::
        I32(42, _) :: EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the DepLambdaNat" in {
    val fileName: String = testFilePath + "DepLambdaNat.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: TypeIdentifier("N",_)::Colon(_):: Kind(Nat(), _) :: DepArrow(_) :: ScalarType(ArrayType(n, IntTyp()), _)::
        EndTypAnnotatedIdent(_) ::

        BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(span6) :: Identifier("a", _) ::Arrow(_) ::
        Identifier("a", _) :: EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the DepLambda" in {
    val fileName: String = testFilePath + "DepLambda.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Kind(AddrSpace(), _) :: DepArrow(_) :: ScalarType(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::

        BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(span6) :: TypeIdentifier("Addr", _) ::DepArrow(_) ::
        I32(5,_) :: EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the DepLambda2" in {
    val fileName: String = testFilePath + "DepLambda2.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: Kind(Data(), _) :: DepArrow(_) :: ScalarType(IntTyp(), _)::
        Arrow(_) :: ScalarType(IntTyp(), _):: Arrow(_) ::
        Kind(Nat(), _) :: DepArrow(_):: ScalarType(FloatTyp(), _)::
        EndTypAnnotatedIdent(_) ::

        BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: TypeIdentifier("D", _) ::DepArrow(_) :: Backslash(_) ::
        Identifier("x", _) :: Arrow(_) :: Backslash(_) :: Identifier("y", _) :: Arrow(_) ::
        Backslash(_) :: TypeIdentifier("N", _) :: DepArrow(_) :: BinOp(BinOpType.MUL, _) ::
        Identifier("x", _) :: Identifier("y", _)
        :: EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the DepLambda3" in {
    val fileName: String = testFilePath + "DepLambda3.rise"
    val file: FileReader =  FileReader(fileName)
    val thrown = intercept[Exception] {
      RecognizeLexeme(file)
    }
    thrown.getMessage should equal("ErrorToken: It is an '=>' expected. The Lexeme '->' is not an '=>'! at FileReader: fileName: 'src/test/scala/parser/readFiles/filesToLex/DepLambda3.rise'; fileContent: {\nf::AddrSpaceK->I32f=\\Addr=>5\n}; beginLocation: (column: 0 ; row: 13); endLocation: (column: 0 ; row: 14)\nf::AddrSpaceK-̲>I32")
  }

  "RecognizeLexeme" should "work for the DepLambda4" in {
    val fileName: String = testFilePath + "DepLambda4.rise"
    val file: FileReader =  FileReader(fileName)
    val thrown = intercept[Exception] {
      RecognizeLexeme(file)
    }
    thrown.getMessage should equal("ErrorToken: It is an '=>' expected. The Lexeme '->' is not an '=>'! at FileReader: fileName: 'src/test/scala/parser/readFiles/filesToLex/DepLambda4.rise'; fileContent: {\nf::AddrSpaceK=>I32f=\\Addr->5\n}; beginLocation: (column: 1 ; row: 7); endLocation: (column: 1 ; row: 7)\nf=\\Addr->5")
  }

  "RecognizeLexeme" should "work for the DepLambda5" in {
    val fileName: String = testFilePath + "DepLambda5.rise"
    val file: FileReader =  FileReader(fileName)
    val thrown = intercept[Exception] {
      RecognizeLexeme(file)
    }
    thrown.getMessage should equal("ErrorToken: the given length is less than 2 for =>! at FileReader: fileName: 'src/test/scala/parser/readFiles/filesToLex/DepLambda5.rise'; fileContent: {\nf::AddrSpaceKf=A\n}; beginLocation: (column: 0 ; row: 13); endLocation: (column: 0 ; row: 13)\nf::AddrSpaceK")
  }

  "RecognizeLexeme" should "work for the FunctionInBraces" in {
    val fileName: String = testFilePath + "FunctionInBraces.rise"
    val file: FileReader = FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(IntTyp(), _) :: Arrow(_) :: ScalarType(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) ::
        LParentheses(_)::Backslash(_):: Identifier("y", _):: Colon(_) ::ScalarType(IntTyp(),_)::
        Arrow(_)::Identifier("y", _)::RParentheses(_):: Identifier("x",_)
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
        DoubleColons(_) :: ScalarType(IntTyp(), _) :: Arrow(_) :: ScalarType(IntTyp(), _)::
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
        DoubleColons(_) :: ScalarType(IntTyp(), _) :: Arrow(_) :: ScalarType(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) ::
        Identifier("x", _) :: EndNamedExpr(_):: Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the Idx" in {
    val fileName: String = testFilePath + "Idx.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(IndexType(NatNumber(2)), _)::
        Arrow(_)::ScalarType(IntTyp(), _) ::

        EndTypAnnotatedIdent(_) :: BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::Backslash(_)::Identifier("t",_)::Arrow(_)::
        Identifier("t",_)::EndNamedExpr(_)
        :: Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the Idx2" in {
    val fileName: String = testFilePath + "Idx2.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(IndexType(NatNumber(42)), _)::
        Arrow(_)::ScalarType(IntTyp(), _) ::

        EndTypAnnotatedIdent(_) :: BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::Backslash(_)::Identifier("t",_)::Arrow(_)::
        Identifier("t",_)::EndNamedExpr(_)
        :: Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "lessComplexInOneLine" in {
    val fileName: String = testFilePath + "lessComplexInOneLine.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(IntTyp(), _) :: Arrow(_) :: ScalarType(IntTyp(), _)::
        Arrow(_) :: ScalarType(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) :: Backslash(_) ::
        Identifier("y", _)  :: Arrow(_) :: UnOp(UnaryOpType.NEG, _) :: LParentheses(_) ::
        BinOp(BinOpType.MUL, _) :: Identifier("x", _)  :: Identifier("y", _) ::
        RParentheses(_) :: EndNamedExpr(_):: Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "lessComplexInOneLineWithDifferentType" in {
    val fileName: String = testFilePath + "lessComplexInOneLineWithDifferentType.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(IntTyp(), _) :: Arrow(_) :: ScalarType(FloatTyp(), _)::
        Arrow(_) :: ScalarType(FloatTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) :: Backslash(_) ::
        Identifier("y", _)  :: Arrow(_) :: UnOp(UnaryOpType.NEG, _) :: LParentheses(_) ::
        BinOp(BinOpType.MUL, _) :: Identifier("x", _)  :: Identifier("y", _) ::
        RParentheses(_) :: EndNamedExpr(_):: Nil => true      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "littleComplexLine" in {
    val fileName: String = testFilePath + "littleComplexLine.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(IntTyp(), _) :: Arrow(_) :: ScalarType(IntTyp(), _)::
        Arrow(_) :: ScalarType(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Arrow(_) :: Backslash(_) ::
        Identifier("y", _) ::  Arrow(_) ::  BinOp(BinOpType.ADD, _) ::
        LParentheses(_) :: BinOp(BinOpType.MUL, _) :: Identifier("x", _)  ::
        Identifier("y", _) :: RParentheses(_) :: I32(42, _) :: EndNamedExpr(_):: Nil => true
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
        DoubleColons(_) :: ScalarType(IntTyp(), _) ::
        Arrow(_) :: ScalarType(IntTyp(), _)::
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
        DoubleColons(_) :: ScalarType(IntTyp(), _) ::
        Arrow(_) :: ScalarType(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _) :: Colon(_) :: ScalarType(IntTyp(), _) ::
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
        DoubleColons(_) :: ScalarType(IntTyp(), _) ::
        Arrow(_) :: ScalarType(IntTyp(), _)::
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
        DoubleColons(_) :: ScalarType(BoolType(), _) ::
        Arrow(_) :: ScalarType(BoolType(), _)::
        EndTypAnnotatedIdent(_) ::

        BeginNamedExpr(_) :: Identifier("f", _) ::
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
        DoubleColons(_) :: ScalarType(BoolType(), _) ::
        Arrow(_) :: ScalarType(BoolType(), _)::
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
        DoubleColons(_) :: ScalarType(IntTyp(), _) ::
        Arrow(_) :: ScalarType(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _)  ::
        Arrow(_) :: BinOp(BinOpType.ADD, _) :: Identifier("x", _)  :: I32(5, _) ::
        EndNamedExpr(_)::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the TupleType" in {
    val fileName: String = testFilePath + "TupleType.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(TupleType(IntTyp(),FloatTyp()), _)::
        Arrow(_)::ScalarType(IntTyp(), _) ::

        EndTypAnnotatedIdent(_) :: BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::Backslash(_)::Identifier("t",_)::Arrow(_)::
        Identifier("t",_)::EndNamedExpr(_)
        :: Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the TupleType2" in {
    val fileName: String = testFilePath + "TupleType2.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(TupleType(IntTyp(),FloatTyp()), _)::
        Arrow(_)::ScalarType(IntTyp(), _) ::

        EndTypAnnotatedIdent(_) :: BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::Backslash(_)::Identifier("t",_)::Arrow(_)::
        Identifier("t",_)::EndNamedExpr(_)
        :: Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the TupleType3" in {
    val fileName: String = testFilePath + "TupleType3.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(TupleType(IntTyp(), ArrayType(NatNumber(2),IntTyp())), _)::
        Arrow(_)::ScalarType(IntTyp(), _) ::

        EndTypAnnotatedIdent(_) :: BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::Backslash(_)::Identifier("t",_)::Arrow(_)::
        Identifier("t",_)::EndNamedExpr(_)
        :: Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for the TupleType4" in {
    val fileName: String = testFilePath + "TupleType4.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(TupleType(TupleType(IntTyp(),
          ArrayType(NatNumber(5), ArrayType(NatNumber(4), ArrayType(NatNumber(3),
            ArrayType(NatNumber(2), IntTyp()))))),
      ArrayType(NatNumber(2), TupleType(IntTyp(), IntTyp()))), _)::
        Arrow(_)::ScalarType(IntTyp(), _) ::

        EndTypAnnotatedIdent(_) :: BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::Backslash(_)::Identifier("t",_)::Arrow(_)::
        Identifier("t",_)::EndNamedExpr(_)
        :: Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for twoplus1extraDefintion" in {
    val fileName: String = testFilePath + "twoplus1extraDefintion.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(IntTyp(), _) ::
        Arrow(_) :: ScalarType(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::

        BeginTypAnnotatedIdent(_):: Identifier("h", _)::
        DoubleColons(_) :: ScalarType(IntTyp(), _) ::
        Arrow(_) :: LParentheses(_) :: ScalarType(IntTyp(), _):: Arrow(_) :: ScalarType(IntTyp(), _) ::
        RParentheses(_) :: Arrow(_) :: ScalarType(IntTyp(), _):: EndTypAnnotatedIdent(_) ::

        BeginNamedExpr(_) :: Identifier("h", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _)  ::
        Arrow(_) :: Backslash(_) :: Identifier("fkt", _)  ::
        Arrow(_) :: Identifier("fkt", _) :: Identifier("x", _) ::
        EndNamedExpr(_)::

        BeginNamedExpr(_) :: Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("y", _)  ::
        Arrow(_) :: BinOp(BinOpType.ADD, _) :: Identifier("y", _)  :: I32(5, _) ::
        EndNamedExpr(_)::

        BeginTypAnnotatedIdent(_):: Identifier("z", _)::
        DoubleColons(_) :: ScalarType(IntTyp(), _) ::
        Arrow(_) :: ScalarType(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::Nil => true
      case a => fail(a.toString())
    }
  }

  "RecognizeLexeme" should "work for twoplus1extraDefintionButSameNameInLocalVariable" in {
    val fileName: String = testFilePath + "twoplus1extraDefintionButSameNameInLocalVariable.rise"
    val file: FileReader =  FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    lexer.tokens match {
      case BeginTypAnnotatedIdent(_):: Identifier("f", _)::
        DoubleColons(_) :: ScalarType(IntTyp(), _) ::
        Arrow(_) :: ScalarType(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::

        BeginTypAnnotatedIdent(_):: Identifier("h", _)::
        DoubleColons(_) :: ScalarType(IntTyp(), _) ::
        Arrow(_) :: LParentheses(_) :: ScalarType(IntTyp(), _):: Arrow(_) :: ScalarType(IntTyp(), _) ::
        RParentheses(_) :: Arrow(_) :: ScalarType(IntTyp(), _):: EndTypAnnotatedIdent(_) ::

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
        DoubleColons(_) :: ScalarType(IntTyp(), _) ::
        Arrow(_) :: ScalarType(IntTyp(), _)::
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
        DoubleColons(_) :: ScalarType(IntTyp(), _) ::
        Arrow(_) :: ScalarType(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::

        BeginNamedExpr(_) :: Identifier("h", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _)  ::
        Arrow(_) :: Backslash(_) :: Identifier("fkt", _)  ::
        Arrow(_) :: Identifier("fkt", _) :: Identifier("x", _) ::
        EndNamedExpr(_)::

        BeginTypAnnotatedIdent(_):: Identifier("h", _)::
        DoubleColons(_) :: ScalarType(IntTyp(), _) ::
        Arrow(_) :: LParentheses(_) :: ScalarType(IntTyp(), _):: Arrow(_) :: ScalarType(IntTyp(), _) ::
        RParentheses(_) :: Arrow(_) :: ScalarType(IntTyp(), _):: EndTypAnnotatedIdent(_) ::

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
        DoubleColons(_) :: ScalarType(IntTyp(), _) ::
        Arrow(_) :: ScalarType(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::

        BeginTypAnnotatedIdent(_):: Identifier("h", _)::
        DoubleColons(_) :: ScalarType(IntTyp(), _) ::
        Arrow(_) :: LParentheses(_) :: LParentheses(_) :: ScalarType(IntTyp(), _):: Arrow(_) :: ScalarType(IntTyp(), _) ::
        RParentheses(_) :: Arrow(_) :: ScalarType(IntTyp(), _):: RParentheses(_):: EndTypAnnotatedIdent(_) ::

        BeginNamedExpr(_) :: Identifier("h", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _)  ::
        Arrow(_) :: LParentheses(_)::Backslash(_) :: Identifier("fkt", _)  ::
        Arrow(_) :: LParentheses(_):: Identifier("fkt", _) :: Identifier("x", _) ::
        RParentheses(_)::RParentheses(_)::
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
        DoubleColons(_) :: ScalarType(IntTyp(), _) ::
        Arrow(_) :: ScalarType(IntTyp(), _)::
        EndTypAnnotatedIdent(_) ::

        BeginTypAnnotatedIdent(_):: Identifier("h", _)::
        DoubleColons(_) :: ScalarType(IntTyp(), _) ::
        Arrow(_) :: LParentheses(_) :: LParentheses(_) :: ScalarType(IntTyp(), _):: Arrow(_) :: ScalarType(IntTyp(), _) ::
        RParentheses(_) :: Arrow(_) :: ScalarType(IntTyp(), _):: RParentheses(_):: EndTypAnnotatedIdent(_) ::

        BeginNamedExpr(_) :: Identifier("h", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("x", _)  ::
        Arrow(_) :: LParentheses(_)::Backslash(_) :: Identifier("fkt", _)  ::
        Arrow(_) :: LParentheses(_):: Identifier("fkt", _) :: Identifier("x", _) ::
        RParentheses(_)::RParentheses(_)::
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
        DoubleColons(_) :: ScalarType(BoolType(), _)::
        Arrow(_) :: ScalarType(BoolType(), _)::
        Arrow(_) :: ScalarType(IntTyp(), _)::
        Arrow(_) :: ScalarType(FloatTyp(), _)::
        Arrow(_) :: ScalarType(FloatTyp(), _)::
        EndTypAnnotatedIdent(_) ::BeginNamedExpr(_) ::

        Identifier("f", _) ::
        EqualsSign(_)::
        Backslash(_) :: Identifier("michael", _) :: Arrow(_) :: Backslash(_) ::
        Identifier("heinrich", _) :: Arrow(_) :: UnOp(UnaryOpType.NOT, _) ::
        LParentheses(_) :: BinOp(BinOpType.EQ, _) :: LParentheses(_)::BinOp(BinOpType.MOD, _) ::
        LParentheses(_) :: Backslash(_) :: Identifier("varX", _) ::
        Arrow(_) :: Backslash(_) :: Identifier("varY", _) ::
        Arrow(_) :: BinOp(BinOpType.MUL, _) :: Identifier("varX", _)  :: LParentheses(_)::
        BinOp(BinOpType.MUL, _) :: Identifier("varY", _)  :: LParentheses(_) :: BinOp(BinOpType.DIV, _)
        :: LParentheses(_) :: BinOp(BinOpType.SUB, _) :: I32(25, _) :: F32(a, _) :: RParentheses(_)
        ::  F32(b, _) :: RParentheses(_) :: RParentheses(_):: RParentheses(_):: I32(42, _) :: RParentheses(_) :: I32(0, _) :: RParentheses(_) ::
        EndNamedExpr(_)::

        BeginTypAnnotatedIdent(_):: Identifier("specialFunctionOfChaos", _)::
        DoubleColons(_) :: ScalarType(BoolType(), _)::
        Arrow(_) :: ScalarType(BoolType(), _)::
        Arrow(_) :: ScalarType(IntTyp(), _)::
        Arrow(_) :: ScalarType(FloatTyp(), _)::
        Arrow(_) :: ScalarType(FloatTyp(), _)::
        EndTypAnnotatedIdent(_) ::

        Nil => {
        a == 10.5 && b == 2.3 //I can't write 2.3 directly in the pattern match, because then it would be unequal
      }
      case a => fail(a.toString())
    }
  }

}