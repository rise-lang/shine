package parser
import org.scalatest.flatspec.AnyFlatSpec

//import parser.parse.ParseError
//import org.scalatest.matchers.should.Matchers.equal
import org.scalatest.matchers.should.Matchers._
import rise.{core => r}
import rise.core.{types => rt, primitives => rp}
//import rise.core.{semantics => rS}
//import rise.core.{primitives => rp}


class parserMainTest extends  AnyFlatSpec {
  val testFilePath = "src/test/scala/parser/readFiles/filesToLex/"
  //HashMap<r.Identifier, Option[r.Expr]> ist das oberste
  type MapFkt = parse.MapFkt

  "parser" should "be able to parse 'arrayType.rise'" in {
    val fileName: String = testFilePath + "arrayType.rise"
    val riseExprByIdent = parseFile(fileName)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
      case Left(lambda) => lambda.toExpr
      case Right(types) => fail("no definition is in map: " + types)
    }


    ex match {
      case r.Lambda(r.Identifier("a"), r.Lambda(r.Identifier("x"), r.App(r.App(rp.concat(_),
      r.Identifier("a")),r.Identifier("x")))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }

    ex.t match {
      case rt.FunType(rt.ArrayType(n, rt.i32), rt.FunType(rt.ArrayType(n2, rt.i32), rt.ArrayType(n3, rt.i32)))
        if n.eval.equals(5) &&n2.eval.equals(2)&&n3.eval.equals(7) => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }

    ex.span match {
      case None => fail("The Span should not be None")
      case Some(Span(file, begin, end)) => {
        file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/arrayType.rise")
        begin.row should equal(2)
        end.row should equal(22)
        begin.column should equal(1)
        end.column should equal(1)
      }
    }
  }

}