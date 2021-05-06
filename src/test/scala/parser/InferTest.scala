package parser

import apps.nbody.{runKernel, runOriginalKernel}
import org.scalatest.flatspec.AnyFlatSpec
import parser.parse.{HMExpr, HMNat, HMType}
import rise.core
import rise.core.{DepApp, DepLambda, Lambda, Literal, Primitive}
import shine.OpenCL.{GlobalSize, LocalSize}
import util.gen

//import parser.parse.ParseError
//import org.scalatest.matchers.should.Matchers.equal
import org.scalatest.matchers.should.Matchers._
import rise.core.{types => rt}
import rise.core.{semantics => rS}
import rise.core.{primitives => rp}
import rise.{core => r, openCL => o}
import o.{primitives => op, TypedDSL => dsl}


class InferTest extends  test_util.TestsWithExecutor {

  val testFilePath = "src/test/scala/parser/readFiles/inferTests/"
  type MapFkt = parse.MapFkt

  test("parser should be able to parse 'FctNameDoesNotExits.rise'") {
    val fileName: String = testFilePath + "FctNameDoesNotExits.rise"
    val riseExprByIdent = parseFile(fileName)
  }
  test("parser should be able to parse 'FctReturnsWrongType.rise'") {
    val fileName: String = testFilePath + "FctReturnsWrongType.rise"
    val riseExprByIdent = parseFile(fileName)
  }
  test("parser should be able to parse 'FctTakes1MoreArgument.rise'") {
    val fileName: String = testFilePath + "FctTakes1MoreArgument.rise"
    val riseExprByIdent = parseFile(fileName)
  }
  test("parser should be able to parse 'FctTakesAnotherFunctionButOnlyGetsSimpleType.rise'") {
    val fileName: String = testFilePath + "FctTakesAnotherFunctionButOnlyGetsSimpleType.rise"
    val riseExprByIdent = parseFile(fileName)
  }
  test("parser should be able to parse 'FctTakesDifferentType.rise'") {
    val fileName: String = testFilePath + "FctTakesDifferentType.rise"
    val riseExprByIdent = parseFile(fileName)
  }
  test("parser should be able to parse 'FctTakesNoArguments.rise'") {
    val fileName: String = testFilePath + "FctTakesNoArguments.rise"
    val riseExprByIdent = parseFile(fileName)
  }

}
