package parser
import parser.lexer.ArrayType
import parser.lexer.ArrayType
import parser.lexer.{ArrayType, BoolType, DataK, DoubleType, FileReader, FloatTyp, FunctionTyp, GenericsTyp, Identifier, IdentifierType, IndexType, Location, Nat, Span, TupleType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class toStringTest extends  AnyFlatSpec{

  val beginLocation: Location = new Location( 13, 10)
  val endLocation: Location = new Location( 13, 14)
  val fileName:String = "src/test/scala/parser/readFiles/aRISEFile.rise"
  val file:FileReader = new FileReader(fileName)
  val location: Span = new Span(file, beginLocation, endLocation)


  "toString" should " work in LambdaType" in {
    val l:FunctionTyp = new FunctionTyp(new FloatTyp(), new DoubleType())
    l.toString should equal("<Float> -> <Double>")
  }

  "toString" should " work in GenericExpression" in {
    val identifier:IdentifierType = IdentifierType("PeterVogel_123")
    val l:GenericsTyp = new GenericsTyp(new Identifier(identifier, location), new DataK(), new DoubleType())
    l.toString should equal("( " + identifier.toString() + " : <dataKind> ) -> <Double>")
  }

  "toString" should " work in ToupleTypes" in {
    val identifier:IdentifierType = IdentifierType("PeterVogel_123")
    val l:GenericsTyp = new GenericsTyp(new Identifier(identifier, location), new DataK(), new DoubleType())
    val t:TupleType = new TupleType(l, new FloatTyp())
    t.toString should equal("( ( " + identifier.toString() + " : <dataKind> ) -> <Double> , <Float> )")
  }

  "toString" should " work in ArrayType" in {
    val a:ArrayType = new ArrayType(new Nat(5), new DoubleType())
    a.toString should equal("<5:nat>.<Double>")
  }

  "toString" should " work in indexType" in {
    val i:IndexType = new IndexType(new Nat(32))
    i.toString should equal("idx[<32:nat>]")
  }

  //_____________________________________________________________________________________

  "toString" should " work in boolType" in {
    val b:BoolType = new BoolType()
    b.toString should equal("<Bool>")
  }

  //Todo: Test toString of Span, FileReader and LexState-Case-Classes

}
