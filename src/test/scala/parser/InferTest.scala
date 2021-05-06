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

  test("parser should be able to parse 'AddWithTwoDifferentScalarTypes.rise'") {
    val fileName: String = testFilePath + "AddWithTwoDifferentScalarTypes.rise"
    val riseExprByIdent = parseFile(fileName)
  }
  test("parser should be able to parse 'AddWithTwoDifferentScalarTypes2.rise'") {
    val fileName: String = testFilePath + "AddWithTwoDifferentScalarTypes2.rise"
    val riseExprByIdent = parseFile(fileName)
  }
  test("parser should be able to parse 'AddWithTwoDifferentScalarTypes3.rise'") {
    val fileName: String = testFilePath + "AddWithTwoDifferentScalarTypes3.rise"
    val riseExprByIdent = parseFile(fileName)
  }
  test("parser should be able to parse 'AddWithTwoDifferentScalarTypes4.rise'") {
    val fileName: String = testFilePath + "AddWithTwoDifferentScalarTypes4.rise"
    val riseExprByIdent = parseFile(fileName)
  }
  test("parser should be able to parse 'AddWithTwoDifferentScalarTypes5.rise'") {
    val fileName: String = testFilePath + "AddWithTwoDifferentScalarTypes5.rise"
    val riseExprByIdent = parseFile(fileName)
  }
  test("parser should be able to parse 'AddWithTwoDifferentScalarTypes6.rise'") {
    val fileName: String = testFilePath + "AddWithTwoDifferentScalarTypes6.rise"
    val riseExprByIdent = parseFile(fileName)
  }
  test("parser should be able to parse 'AddWithTwoDifferentScalarTypes7.rise'") {
    val fileName: String = testFilePath + "AddWithTwoDifferentScalarTypes7.rise"
    val riseExprByIdent = parseFile(fileName)
  }
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
  test("parser should be able to parse 'FctTakesAnotherFunctionAndF32ButYouGiveWrongFunction.rise'") {
    val fileName: String = testFilePath + "FctTakesAnotherFunctionAndF32ButYouGiveWrongFunction.rise"
    val riseExprByIdent = parseFile(fileName)
  }
  test("parser should be able to parse 'FctTakesAnotherFunctionButOnlyGetsSimpleType.rise'") {
    val fileName: String = testFilePath + "FctTakesAnotherFunctionButOnlyGetsSimpleType.rise"
    val riseExprByIdent = parseFile(fileName)
  }
  test("parser should be able to parse 'FctTakesAnotherFunctionButYouGiveWrongFctType.rise'") {
    val fileName: String = testFilePath + "FctTakesAnotherFunctionButYouGiveWrongFctType.rise"
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
  test("parser should be able to parse 'TwoLambdasX.rise'") {
    val fileName: String = testFilePath + "TwoLambdasX.rise"
    val riseExprByIdent = parseFile(fileName)
    val ex_f = riseExprByIdent.get("f").get
    println("result:\n" +ex_f)
  }
  test("parser should be able to parse 'UseWrongTypeInAdd.rise'") {
    val fileName: String = testFilePath + "UseWrongTypeInAdd.rise"
    val riseExprByIdent = parseFile(fileName)
  }

  test("parser should be able to parse 'UseWrongTypeInAdd2.rise'") {
    val fileName: String = testFilePath + "UseWrongTypeInAdd2.rise"
    val riseExprByIdent = parseFile(fileName)
  }

  test("parser should be able to parse 'UseWrongTypeInAdd3.rise'") {
    val fileName: String = testFilePath + "UseWrongTypeInAdd3.rise"
    val riseExprByIdent = parseFile(fileName)
  }

  test("parser should be able to parse 'UseWrongTypeInAdd4.rise'") {
    val fileName: String = testFilePath + "UseWrongTypeInAdd4.rise"
    val riseExprByIdent = parseFile(fileName)
  }

  test("parser should be able to parse 'UseWrongTypeInAdd5.rise'") {
    val fileName: String = testFilePath + "UseWrongTypeInAdd5.rise"
    val riseExprByIdent = parseFile(fileName)
  }

  test("parser should be able to parse 'UseWrongTypeInPrimitiveJoin.rise'") {
    val fileName: String = testFilePath + "UseWrongTypeInPrimitiveJoin.rise"
    val riseExprByIdent = parseFile(fileName)
  }

  test("parser should be able to parse 'UseWrongTypeInPrimitiveJoin2.rise'") {
    val fileName: String = testFilePath + "UseWrongTypeInPrimitiveJoin2.rise"
    val riseExprByIdent = parseFile(fileName)
  }
  test("parser should be able to parse 'UseWrongTypeInPrimitiveJoin3.rise'") {
    val fileName: String = testFilePath + "UseWrongTypeInPrimitiveJoin3.rise"
    val riseExprByIdent = parseFile(fileName)
  }
}
