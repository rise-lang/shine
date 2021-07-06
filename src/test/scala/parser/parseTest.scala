package parser
import apps.convolution.{blurXTiled2D, blurXTiled2DSizes}
import apps.nbody.{runKernel, runOriginalKernel}
import org.scalatest.flatspec.AnyFlatSpec
import parser.ErrorMessage.ParserException
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



class parseTest extends  test_util.TestsWithExecutor {
  parser.ErrorMessage.debug.isOn = true
  val testFilePath = "src/test/scala/parser/readFiles/filesToLex/"
  val errorFilePath = "src/test/scala/parser/readFiles/filesToError/"
  //HashMap<r.Identifier, Option[r.Expr]> ist das oberste
  type MapFkt = parse.MapFkt

  test("parser should be able to parse 'addMatrix.rise'"){
    val fileName: String = testFilePath + "addMatrix.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "matrixaddition"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))
    println(lexer.tokens)
    println(gen.OpenCLKernel(ex))
  }
  test("parser should be able to parse 'addMatrixDifferentOrder.rise'"){
    val fileName: String = testFilePath + "addMatrixDifferentOrder.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "matrixaddition"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    println(gen.OpenCLKernel(ex))
  }

  test("parser should be able to parse 'arrayType.rise'"){
    val fileName: String = testFilePath + "arrayType.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))


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
      case Some(Span(file, Range(begin, end))) => {
        file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/arrayType.rise")
        begin.row should equal(2)
        end.row should equal(20)
        begin.column should equal(1)
        end.column should equal(1)
      }
    }
  }

  //Todo: It should be possible to parse "    arrayTypeOnlyTypAnn.rise\" and not only "arrayTypeOnlyTypAnn.rise"

  //Todo: not ignore; change it to an ErrorTest or change this test a little bit
  ignore("parser should be able to parse 'arrayTypeOnlyTypAnn.rise'"){
    val fileName: String = testFilePath + "arrayTypeOnlyTypAnn.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val exT = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

//    exT match {
//      case rt.FunType(rt.ArrayType(n, rt.i32), rt.FunType(rt.i32, rt.i32)) if n.eval.equals(5) => true
//      case a => fail("it was a different definition expected, but we see: " + a)
//    }
//
//    val functionName2: String = "h"
//    val ex: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))
//
//    ex.t match {
//      case rt.FunType(rt.f32, rt.f32) => true
//      case t => fail("The Type '" + t + "' is not the expected type.")
//    }
//
//    ex match {
//      case r.Lambda(r.Identifier("x"), spAddComp @ r.App(r.App(rp.add(spAdd), idX @ r.Identifier("x")), r.Literal(rS.FloatData(5), sp5))) =>
//      {
//        spAdd match {
//          case None => fail("The Span should not be None")
//          case Some(Span(file, Range(begin, end))) => {
//            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/arrayTypeOnlyTypAnn.rise")
//            begin.row should equal(6)
//            end.row should equal(7)
//            begin.column should equal(2)
//            end.column should equal(2)
//          }
//        }
//        idX.span match {
//          case None => fail("The Span should not be None")
//          case Some(Span(file, Range(begin, end))) => {
//            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/arrayTypeOnlyTypAnn.rise")
//            begin.row should equal(7)
//            end.row should equal(8)
//            begin.column should equal(2)
//            end.column should equal(2)
//          }
//        }
//        sp5 match {
//          case None => fail("The Span should not be None")
//          case Some(Span(file, Range(begin, end))) => {
//            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/arrayTypeOnlyTypAnn.rise")
//            begin.row should equal(9)
//            end.row should equal(13)
//            begin.column should equal(2)
//            end.column should equal(2)
//          }
//        }
//        spAddComp.span match {
//          case None => fail("The Span should not be None")
//          case Some(Span(file, Range(begin, end))) => {
//            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/arrayTypeOnlyTypAnn.rise")
//            begin.row should equal(6)
//            end.row should equal(13)
//            begin.column should equal(2)
//            end.column should equal(2)
//          }
//        }
//      }
//      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
//      case a => fail("not a lambda: " + a)
//    }
//
//    ex.span match {
//      case None => fail("The Span should not be None")
//      case Some(Span(file, Range(begin, end))) => {
//        file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/arrayTypeOnlyTypAnn.rise")
//        begin.row should equal(2)
//        end.row should equal(13)
//        begin.column should equal(2)
//        end.column should equal(2)
//      }
//    }
//  }
//
//
//  test("parser should be able to parse 'Brace5.rise'"){
//    val fileName: String = testFilePath + "Brace5.rise"
//    val file: FileReader = new FileReader(fileName)
//    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
//    val riseExprByIdent = parse(lexer.tokens)
//
//    val functionName: String = "f"
//    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))
//
//    ex match {
//      case r.Lambda(r.Identifier("x"), r.Identifier("x")) => true
//      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
//      case a => fail("not a lambda: " + a)
//    }
  }

  test("parser should be able to parse 'braces.rise'"){
    val fileName: String = testFilePath + "braces.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("b"), r.Identifier("b")) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'bracesWithNot.rise'"){
    val fileName: String = testFilePath + "bracesWithNot.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("b"), r.App(rp.not(_), r.Identifier("b"))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'convolution.rise'"){
    val fileName: String = testFilePath + "convolution.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName2: String = "nbody"
    val ex_g: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))

    val N = 512
    val tileX = 256
    val tileY = 1
    val random = new scala.util.Random()
    val matrix = Array.fill(N)(random.nextFloat() * 10.0f)
    val weights = Array.fill(17)(random.nextFloat())

    val (lsX, gsX) = blurXTiled2DSizes(N)
    val kernelX = gen.OpenCLKernel(blurXTiled2D(N))
    val kernelParser = gen.OpenCLKernel(ex_g)
    test_util.runsWithSameResult(Seq(
      ("parser NVIDIA", runKernel(kernelParser, lsX, gsX, matrix, weights)),
      ("dpia X", runKernel(kernelX, lsX, gsX, matrix, weights))
    ))
  }

  test("parser should be able to parse 'composition.rise'"){
    val fileName: String = testFilePath + "composition.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName2: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))

    ex_f match {
      case r.Lambda(r.Identifier("x"), r.App(r.App(rp.add(_), r.Identifier("x")),
      r.App(r.Lambda(e1, r.App(r.App(rp.add(_), r.Identifier("x")),r.App(r.App(rp.sub(_), r.Identifier("x")), e2:r.Identifier))), r.Identifier("x"))))
        if e1.name.equals(e2.name)
      => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'composition2.rise'"){
    val fileName: String = testFilePath + "composition2.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName2: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))

    ex_f match {
      case r.Lambda(r.Identifier("x"), r.App(r.Lambda(e1, r.App(rp.not(_), r.App(rp.not(_), e2:r.Identifier))), r.Identifier("x")))
        if e1.name.equals(e2.name)
      => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'composition3.rise'"){
    val fileName: String = testFilePath + "composition3.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName2: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))

    ex_f match {
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("y"),r.App(r.Lambda(e1, r.App(r.App(rp.equal(_),
      r.Identifier("y")), r.App(rp.not(_), e2:r.Identifier))), r.Identifier("x"))))
        if e1.name.equals(e2.name)
      => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'composition4.rise'"){
    val fileName: String = testFilePath + "composition4.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName2: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))

    ex_f match {
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("y"),r.App(r.Lambda(e1,
      r.App(r.App(rp.equal(_), r.Identifier("y")), r.App(rp.not(_),
      r.App(rp.not(_), e2:r.Identifier)))), r.Identifier("x"))))
        if e1.name.equals(e2.name)
      => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'Complex1.rise'"){
    val fileName: String = testFilePath + "Complex1.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("y"), r.Lambda(r.Identifier("z"), r.App(r.App(rp.mul(_), r.Identifier("x")), r.App(r.App(rp.add(_), r.Identifier("y")), r.Identifier("z")))))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'Complex2.rise'"){
    val fileName: String = testFilePath + "Complex2.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case lambda1@ r.Lambda(x@r.Identifier("x"), app1@ r.App( app2@r.App(rp.mul(spanMul),
      app3@r.App(app4@r.App(rp.add(spanAdd),
      x2@r.Identifier("x")), x3@r.Identifier("x"))), x4@r.Identifier("x"))) => {
        x4.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex2.rise")
            begin.row should equal(16)
            end.row should equal(17)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }
        x3.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex2.rise")
            begin.row should equal(13)
            end.row should equal(14)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }
        x2.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex2.rise")
            begin.row should equal(11)
            end.row should equal(12)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }
        x.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex2.rise")
            begin.row should equal(2)
            end.row should equal(3)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }
        spanAdd match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex2.rise")
            begin.row should equal(9)
            end.row should equal(10)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }
        spanMul match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex2.rise")
            begin.row should equal(6)
            end.row should equal(7)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }
        app4.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex2.rise")
            begin.row should equal(9)
            end.row should equal(12)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }
        app3.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex2.rise")
            begin.row should equal(9)
            end.row should equal(14)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }
        app2.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex2.rise")
            begin.row should equal(6)
            end.row should equal(14)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }
        app1.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex2.rise")
            begin.row should equal(6)
            end.row should equal(17)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }
      }
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'Complex3.rise'"){
    val fileName: String = testFilePath + "Complex3.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(x@r.Identifier("x"), app1@r.App(app2@r.App(rp.mul(spanMul), app3@r.App(
      r.App(rp.add(_),
      r.Identifier("x")), r.Identifier("x"))), x1@r.Identifier("x"))) => {
        app1.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex3.rise")
            begin.row should equal(7)
            end.row should equal(18)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }

        app2.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex3.rise")
            begin.row should equal(7)
            end.row should equal(15)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }

        app3.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex3.rise")
            begin.row should equal(10)
            end.row should equal(15)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }

        spanMul match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex3.rise")
            begin.row should equal(7)
            end.row should equal(8)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }

        x.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex3.rise")
            begin.row should equal(2)
            end.row should equal(3)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }

        x1.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex3.rise")
            begin.row should equal(17)
            end.row should equal(18)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }
      }
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'Complex4.rise'"){
    val fileName: String = testFilePath + "Complex4.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("x"), app1@r.App(app2@r.App(rp.div(spanDiv), app3@r.App(r.App(rp.mul(_),
      r.Identifier("x")), r.Identifier("x"))), app4@r.App(r.App(rp.mul(_),
      r.Identifier("x")), r.Identifier("x")))) => {
        spanDiv match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex4.rise")
            begin.row should equal(6)
            end.row should equal(7)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }
        app1.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex4.rise")
            begin.row should equal(6)
            end.row should equal(22)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }
        app2.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex4.rise")
            begin.row should equal(6)
            end.row should equal(14)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }
        app3.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex4.rise")
            begin.row should equal(9)
            end.row should equal(14)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }
        app4.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/Complex4.rise")
            begin.row should equal(17)
            end.row should equal(22)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }
      }
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'Complex5.rise'"){
    val fileName: String = testFilePath + "Complex5.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("x"), r.App(r.App(rp.div(_), r.App(r.App(rp.mul(_),
      r.Identifier("x")), r.Identifier("x"))), r.App(r.App(rp.mul(_),
      r.Identifier("x")), r.Identifier("x")))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'Complex6.rise'"){
    val fileName: String = testFilePath + "Complex6.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("x"), r.App(r.App(rp.div(_), r.App(r.App(rp.mul(_),
      r.Identifier("x")), r.Identifier("x"))), r.App(r.App(rp.mul(_),
      r.Identifier("x")), r.Identifier("x")))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'Complex7.rise'"){
    val fileName: String = testFilePath + "Complex7.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("x"), r.App(r.App(rp.div(_), r.App(r.App(rp.mul(_),
      r.Identifier("x")), r.Identifier("x"))), r.App(r.App(rp.mul(_),
      r.Identifier("x")), r.Identifier("x")))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'ComplexIdentifier.rise'"){
    val fileName: String = testFilePath + "ComplexIdentifier.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("hans_Georg"), r.Identifier("hans_Georg")) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'complexInOneLine.rise'"){
    val fileName: String = testFilePath + "complexInOneLine.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("y"), r.App(rp.neg(_),
      r.App(r.App(rp.add(_), r.App(r.App(rp.mul(_), r.Identifier("x")), r.Identifier("y"))),
      r.App(r.App(rp.mod(_), r.Literal(rS.FloatData(42), _)), r.Literal(rS.FloatData(5), _)))))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e + ": expected: " +
        r.Lambda(r.Identifier("x")(rt.TypePlaceholder), r.Lambda(r.Identifier("y")(rt.TypePlaceholder), r.App(rp.neg.primitive,
          r.App(r.App(rp.add.primitive, r.App(r.App(rp.mul.primitive, r.Identifier("x")(rt.TypePlaceholder))(rt.TypePlaceholder), r.Identifier("y")(rt.TypePlaceholder))(rt.TypePlaceholder))(rt.TypePlaceholder),
            r.App(r.App(rp.mod.primitive, r.Literal(rS.IntData(42)))(rt.TypePlaceholder), r.Literal(rS.IntData(5)))(rt.TypePlaceholder))(rt.TypePlaceholder))(rt.TypePlaceholder))(rt.TypePlaceholder))(rt.TypePlaceholder))
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'complexInThreeLines.rise'"){
    val fileName: String = testFilePath + "complexInThreeLines.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("y"), r.App(rp.neg(_),
      r.App(r.App(rp.add(_), r.App(r.App(rp.mul(_), r.Identifier("x")), r.Identifier("y"))),
      r.App(r.App(rp.mod(_), r.Literal(rS.FloatData(42), _)), r.Literal(rS.FloatData(5), _)))))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e + ": expected: " +
        r.Lambda(r.Identifier("x")(rt.TypePlaceholder), r.Lambda(r.Identifier("y")(rt.TypePlaceholder), r.App(rp.neg.primitive,
          r.App(r.App(rp.add.primitive, r.App(r.App(rp.mul.primitive, r.Identifier("x")(rt.TypePlaceholder))(rt.TypePlaceholder), r.Identifier("y")(rt.TypePlaceholder))(rt.TypePlaceholder))(rt.TypePlaceholder),
            r.App(r.App(rp.mod.primitive, r.Literal(rS.IntData(42)))(rt.TypePlaceholder), r.Literal(rS.IntData(5)))(rt.TypePlaceholder))(rt.TypePlaceholder))(rt.TypePlaceholder))(rt.TypePlaceholder))(rt.TypePlaceholder))
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'constant42.rise'"){
    val fileName: String = testFilePath + "constant42.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("c"), r.Literal(rS.FloatData(42), _)) => true
      case a => fail("not a lambda: " + a)
    }
    assert(r.IsClosedForm(ex))
  }

  //Todo: Should be fixed in future work (Error by calling a Function, which is declared but not implemented)
  //<Variable is only declared but has no definition: g in (2,10-11)>
  ignore("parser should be able to parse 'DepLambda.rise'"){
    val fileName: String = testFilePath + "DepLambda.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    val functionName2: String = "g"
    val ex_g: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))

    ex_f.t match {
      case dep@rt.DepFunType(n,
      arr@rt.ArrayType(n1: rt.NatIdentifier, rt.i32))
        if n.name.equals("N") && n1.name.equals("N") => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }

    ex_g.t match {
      case rt.DepFunType(n, rt.DepFunType(d,
      rt.ArrayType(n1: rt.NatIdentifier, d1: rt.DataTypeIdentifier)))
        if n.name.equals("N1") && n1.name.equals(n.name)
          && d.name.equals("D") && d1.name.equals(d.name) => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }

    ex_f match {
      case dep@r.DepLambda(n: rt.NatIdentifier, depapp1@r.DepApp(depapp2@r.DepApp(r.Identifier("g"),
      n1:rt.NatIdentifier), i:rt.i32.type ))
        if n.name.equals("N") && n1.name.equals("N")
      => {
        depapp2.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/DepLambda.rise")
            begin.row should equal(10)
            end.row should equal(13)
            begin.column should equal(2)
            end.column should equal(2)
          }
        }
        depapp1.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/DepLambda.rise")
            begin.row should equal(10)
            end.row should equal(17)
            begin.column should equal(2)
            end.column should equal(2)
          }
        }
        dep.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/DepLambda.rise")
            begin.row should equal(2)
            end.row should equal(17)
            begin.column should equal(2)
            end.column should equal(2)
          }
        }
      }
      case r.DepLambda(n, e) => fail("Not correct deplambda: "
        +n.toString()+ " , " + e.toString())
      case a => fail("Not a DepLambda: " + a)
    }

    ex_f.span match {
      case None => fail("The Span should not be None")
      case Some(Span(file, Range(begin, end))) => {
        file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/DepLambda.rise")
        begin.row should equal(2)
        end.row should equal(17)
        begin.column should equal(2)
        end.column should equal(2)
      }
    }


    ex_g match {
      case dep@r.DepLambda(n: rt.NatIdentifier, dep1@r.DepLambda(d:rt.DataTypeIdentifier,
      r.App(r.DepApp(rp.generate(spanGenerate), n1:rt.NatIdentifier),
      lam@r.Lambda(r.Identifier("i"), app1@r.App(
      dep2@r.DepApp(rp.cast(_),d2:rt.DataTypeIdentifier), r.Identifier("i")))
      )))
        if n.name.equals("N1")
          && d.name.equals("D") && n1.name.equals(n.name)&&
          d2.name.equals(d.name)
      => {
        dep.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/DepLambda.rise")
            begin.row should equal(3)
            end.row should equal(47)
            begin.column should equal(3)
            end.column should equal(3)
          }
        }
        dep1.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/DepLambda.rise")
            begin.row should equal(12)
            end.row should equal(47)
            begin.column should equal(3)
            end.column should equal(3)
          }
        }
        spanGenerate match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/DepLambda.rise")
            begin.row should equal(21)
            end.row should equal(29)
            begin.column should equal(3)
            end.column should equal(3)
          }
        }
        lam.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/DepLambda.rise")
            begin.row should equal(34)
            end.row should equal(46)
            begin.column should equal(3)
            end.column should equal(3)
          }
        }
      }
      case r.DepLambda(n, e) => fail("Not correct deplambda: "
        +n.toString()+ " , " + e.toString())
      case a => fail("Not a DepLambda: " + a)
    }

    assert(r.IsClosedForm(ex_f))
    assert(r.IsClosedForm(ex_g))
  }

  //Todo: Should be fixed in future work (Error by Creating a Function which is not covered by the match in Solution yet)
  //<scala.MatchError: (D1: data -> (_t279 -> f32)) ((f32 -> i32)) ~ _t281 (of class rise.core.types.DepConstraint)>
  ignore("parser should be able to parse 'DepLambdaFunctionType.rise'"){
    val fileName: String = testFilePath + "DepLambdaFunctionType.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    //    val functionName: String = "f"
    //    val ex_f: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!")) match {
    //      case Left(lambda) => lambda.toExpr
    //      case Right(types) => fail("no definition is in map: " + types)
    //    }

    val functionName2: String = "g"
    val ex_g: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))

    val functionName3: String = "u"
    val ex_u: r.Expr = riseExprByIdent.get(functionName3).getOrElse(fail("The function '" + functionName3 + "' does not exist!!!"))

    //    ex_f.t match {
    //      case rt.DepFunType(n1:rt.NatIdentifier,rt.DepFunType(d1:rt.DataTypeIdentifier,
    //      rt.FunType(rt.ArrayType(n2: rt.NatIdentifier, rt.ArrayType(n3: rt.NatIdentifier, d2:rt.DataTypeIdentifier)),
    //      rt.FunType(rt.FunType(rt.ArrayType(n4: rt.NatIdentifier,
    //      rt.ArrayType(n5: rt.NatIdentifier, d3:rt.DataTypeIdentifier)),
    //      rt.ArrayType(n6: rt.NatIdentifier, rt.ArrayType(n7: rt.NatIdentifier, d4:rt.DataTypeIdentifier)))
    //      , rt.ArrayType(n8: rt.NatIdentifier, rt.ArrayType(n9: rt.NatIdentifier, d5:rt.DataTypeIdentifier))))
    //      ))
    //        if n1.name.equals("N") && n2.name.equals(n1.name) && n3.name.equals(n1.name) && n4.name.equals(n1.name)
    //          && n5.name.equals(n1.name) && n6.name.equals(n1.name) && n7.name.equals(n1.name)
    //          && n8.name.equals(n1.name) && n9.name.equals(n1.name)
    //          && d1.name.equals("D") && d2.name.equals(d1.name) && d3.name.equals(d1.name)
    //          && d4.name.equals(d1.name) && d5.name.equals(d1.name)
    //      => true
    //      case t => fail("The Type '" + t + "' is not the expected type.")
    //    }

    ex_u.t match {
      case rt.DepFunType(d:rt.DataTypeIdentifier, rt.FunType(d1:rt.DataTypeIdentifier, rt.f32))
        if d.name.equals("D1") && d1.name.equals("D1")=> true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }

    ex_g.t match {
      case rt.FunType(rt.f32, rt.f32) => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }

    ex_u match {
      case r.DepLambda(d:rt.DataTypeIdentifier,
      r.Lambda(r.Identifier("fkt"), r.Literal(rS.FloatData(1), _)))
        if d.name.equals("D1")
      => true
      case r.DepLambda(n, e) => fail("Not correct deplambda: "
        +n.toString()+ " , " + e.toString())
      case a => fail("Not a DepLambda: " + a)
    }

    ex_g match {
      case r.Lambda(r.Identifier("x"), r.DepApp(r.Identifier("u"), rt.FunType(rt.f32, rt.f32)))=> true
      case a => fail("Lambda not correct: " + a)
    }

    assert(r.IsClosedForm(ex_u))
    assert(r.IsClosedForm(ex_g))
  }

  test("parser should be able to parse 'DepLambda2.rise'"){
    val fileName: String = testFilePath + "DepLambda2.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName2: String = "g"
    val ex_g: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))

    ex_g.t match {
      case rt.DepFunType(n, rt.DepFunType(d,
      rt.ArrayType(n2: rt.NatIdentifier,rt.f32)))
        if n2.name.equals(n.name) => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }

    ex_g match {
      case dep@r.DepLambda(n: rt.NatIdentifier, dep2@r.DepLambda(d:rt.DataTypeIdentifier,
      r.App(rp.generate(_),
      lam@r.Lambda(r.Identifier("x"), r.Literal(rS.FloatData(0), _))))) => {
        dep.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/DepLambda2.rise")
            begin.row should equal(3)
            end.row should equal(38)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }
        dep2.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/DepLambda2.rise")
            begin.row should equal(10)
            end.row should equal(38)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }
        lam.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/DepLambda2.rise")
            begin.row should equal(28)
            end.row should equal(37)
            begin.column should equal(1)
            end.column should equal(1)
          }
        }
      }
      case r.DepLambda(n, e) => {
        fail("Not correct deplambda: "
          +n.toString()+ " , " + e.toString())
      }
      case a => fail("Not a DepLambda: " + a)
    }
  }

  test("parser should be able to parse 'DepLambdaNat.rise'"){
    val fileName: String = testFilePath + "DepLambdaNat.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex.t match {
      case rt.DepFunType(nat, rt.FunType(rt.ArrayType(n:rt.NatIdentifier,rt.i32),
      rt.ArrayType(n1:rt.NatIdentifier,rt.i32)))
        if n.name.equals(nat.name)
          &&n1.name.equals(nat.name)=> true
      case t => fail("The Type '"+t+"' is not the expected type.")
    }

    ex match {
      case r.DepLambda(nat, r.Lambda(r.Identifier("arr"), r.Identifier("arr"))) => true
      case r.DepLambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'dotProduct.rise'"){
    val fileName: String = testFilePath + "dotProduct.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex_f.t match {
      case rt.DepFunType(n,
      rt.FunType(rt.ArrayType(n1:rt.NatIdentifier, rt.f32),
      rt.FunType(rt.ArrayType(n2:rt.NatIdentifier, rt.f32),
      rt.f32)))
        if n1.name.equals(n.name) && n2.name.equals(n.name) => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }

    ex_f match {
      //Todo: How can I give rt.i32 to DepApp as second argument or how to do it else to give rt.i32 as an argument to an fkt
      case r.DepLambda(n,
      r.Lambda(r.Identifier("vec1"),r.Lambda(r.Identifier("vec2"),
      r.App(r.App(r.App(rp.reduceSeq(_), r.Lambda(r.Identifier("acc"), r.Lambda(
      r.Identifier("arg"), r.App(r.App(rp.add(_) , r.Identifier("acc")),r.Identifier("arg"))
      ))),r.Literal(rS.FloatData(0),_)),
      r.App(rp.toMem(_),r.App(r.App(rp.mapSeq(_), r.Lambda(r.Identifier("x"),
      r.App(r.App(rp.mul(_), r.App(rp.fst(_), r.Identifier("x"))),
      r.App(rp.snd(_), r.Identifier("x")) )
      )), r.App(r.App(rp.zip(_), r.Identifier("vec1")), r.Identifier("vec2"))))
      )))) => true
      case r.DepLambda(n, e) => fail("Not correct deplambda: "
        +n.toString()+ " , " + e.toString())
      case a => fail("Not a DepLambda: " + a)
    }
  }

  //Todo: Should be fixed in future work (Error: We have an Kind Identifier and we need an
  // neutral element like the 0 in I32 or 0.0 in F32,
  // but we have no universal neutral element, so I use instead 0.
  // 0 is I32 but Kind Identifier could be F32 or Boolean or anything else, because of that
  // this test does not work. If we could have a function or something, which could create an in general
  // an neutral element. This problem could be fixed. But now please don't delete this test yet.
  // Maybe later we could use it.)
  //<rise.core.types.int$ cannot be cast to rise.core.types.Kind$Identifier>
  ignore("parser should be able to parse 'dotProductDep.rise'") {
    val fileName: String = testFilePath + "dotProductDep.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex_f.t match {
      case rt.DepFunType(n,rt.DepFunType(d,
      rt.FunType(rt.ArrayType(n1:rt.NatIdentifier, d1:rt.DataTypeIdentifier),
      rt.FunType(rt.ArrayType(n2:rt.NatIdentifier, d2:rt.DataTypeIdentifier),
      d3:rt.DataTypeIdentifier))))
        if n.name.equals("N") && n1.name.equals(n.name) && n2.name.equals(n.name)
          && d.name.equals("D") && d1.name.equals(d.name)
          && d2.name.equals(d.name) && d3.name.equals(d.name)=> true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }

    ex_f match {
      //Todo: How can I give rt.i32 to DepApp as second argument or how to do it else to give rt.i32 as an argument to an fkt
      case r.DepLambda(n,r.DepLambda(d,
      r.Lambda(r.Identifier("vec1"),r.Lambda(r.Identifier("vec2"),
      r.App(r.App(r.App(rp.reduceSeq(_), r.Lambda(r.Identifier("acc"), r.Lambda(
      r.Identifier("arg"), r.App(r.App(rp.add(_) , r.Identifier("acc")),r.Identifier("arg"))
      ))),r.Literal(rS.IntData(0),_)),
      r.App(rp.toMem(_),r.App(r.App(rp.mapSeq(_), r.Lambda(r.Identifier("x"),
      r.App(r.App(rp.mul(_), r.App(rp.fst(_), r.Identifier("x"))),
      r.App(rp.snd(_), r.Identifier("x")) )
      )), r.App(r.App(rp.zip(_), r.Identifier("vec1")), r.Identifier("vec2"))))
      ))))) => true
      case r.DepLambda(n, e) => fail("Not correct deplambda: "
        +n.toString()+ " , " + e.toString())
      case a => fail("Not a DepLambda: " + a)
    }
  }


  test("parser should be able to parse 'dotProductEasy.rise'"){
    val fileName: String = testFilePath + "dotProductEasy.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    //    ex_f.t match {
    //      case rt.FunType(rt.ArrayType(n, rt.i32),
    //      rt.FunType(rt.ArrayType(n1, rt.i32),
    //      rt.i32))  if n.eval.equals(4) && n1.eval.equals(n.eval)=> true
    //      case rt.FunType(rt.ArrayType(n, rt.i32),
    //      rt.FunType(rt.ArrayType(n1, rt.i32),
    //      rt.i32))=> fail("alomst right, but n should be 4, but it ist: "+ n + " , " + n1)
    //      case t => fail("The Type '" + t + "' is not the expected type.")
    //    }

    //    ex_f match {
    //      //Todo: How can I give rt.i32 to DepApp as second argument or how to do it else to give rt.i32 as an argument to an fkt
    //      case r.Lambda(r.Identifier("vec1"),r.Lambda(r.Identifier("vec2"),
    //      r.App(r.App(rp.reduceSeq(_), r.Lambda(r.Identifier("acc"), r.Lambda(
    //      r.Identifier("arg"), r.App(r.App(rp.add(_) , r.Identifier("acc")),r.Identifier("arg"))
    //      ))),
    //      r.App(r.App(rp.mapSeq(_), r.Lambda(r.Identifier("x"),
    //      r.App(r.App(rp.mul(_), r.App(rp.fst(_), r.Identifier("x"))),
    //      r.App(rp.snd(_), r.Identifier("x")) )
    //      )), r.App(r.App(rp.zip(_), r.Identifier("vec1")), r.Identifier("vec2")))
    //      )
    //      )) => true
    //      case r.DepLambda(n, e) => fail("Not correct deplambda: "
    //        +n.toString()+ " , " + e.toString())
    //      case a => fail("Not a DepLambda: " + a)
    //    }

    val infer_ex_f = r.DSL.ToBeTyped(ex_f).toExpr
    assert(r.IsClosedForm(infer_ex_f)) //It has to be closed form so that gen.Cprogram runs

    val code = gen.CProgram(infer_ex_f, "dot").code
    println(code)
  }

  test("parser should be able to parse 'FunctionInBraces.rise'"){
    val fileName: String = testFilePath + "FunctionInBraces.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("x"), r.App(r.Lambda(r.Identifier("y"), r.Identifier("y")), r.Identifier("x"))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should not be able to parse 'IdentityWithI32.rise'"){
    val fileName: String = testFilePath + "identityWithI32.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("x"), r.Identifier("x")) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'Idx.rise'"){
    val fileName: String = testFilePath + "Idx.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex.t match {
      case rt.FunType(rt.IndexType(n), rt.f32) if n.eval.equals(2)=> true
      case t => fail("The Type '"+t+"' is not the expected type.")
    }

    ex match {
      case r.Lambda(r.Identifier("t"), r.Literal(rS.FloatData(0), _)) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
    assert(r.IsClosedForm(ex))
  }

  test("parser should be able to parse 'Idx2.rise'"){
    val fileName: String = testFilePath + "Idx2.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex.t match {
      case rt.FunType(rt.IndexType(n), rt.f32) if n.eval.equals(42)=> true
      case t => fail("The Type '"+t+"' is not the expected type.")
    }

    ex match {
      case r.Lambda(r.Identifier("t"), r.Literal(rS.FloatData(0), _)) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
    assert(r.IsClosedForm(ex))
  }

  test("parser should be able to parse 'lessComplexInOneLine.rise'"){
    val fileName: String = testFilePath + "lessComplexInOneLine.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("y"), r.App(rp.neg(_), r.App(r.App(rp.mul(_), r.Identifier("x")), r.Identifier("y"))))) => true
      case r.Lambda(x, e) => {
        println("not correct Identifier or not correct expression: " + x + " , " + e)
        fail("not correct Identifier or not correct expression: " + x + " , " + e)
      }
      case a => fail("not a lambda: " + a)
    }
    gen.CProgram(ex,"lessComplexInOneLine")
    assert(r.IsClosedForm(ex))
  }



  test("parser should be able to parse 'littleComplexLine.rise'"){
    val fileName: String = testFilePath + "littleComplexLine.rise"
    val file: FileReader = FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(x@r.Identifier("x"),
      r.Lambda(y@r.Identifier("y"),
      r.App(r.App(rp.add(_),
      r.App(r.App(rp.mul(_), r.Identifier("x")), r.Identifier("y"))),
      r.Literal(rS.FloatData(42), _))
      )) if x.t == rt.f32 && y.t == rt.f32 => true
      case r.Lambda(x@r.Identifier("x"),
      r.Lambda(y@r.Identifier("y"),
      r.App(r.App(rp.add(_),
      r.App(r.App(rp.mul(_), r.Identifier("x")), r.Identifier("y"))),
      r.Literal(rS.FloatData(42), _))
      )) => fail("almost correct, but Types don't match!  " + x.t + " != rt.i32 oder/und " + y.t + " != rt.i32")
      case r.Lambda(x, r.Lambda(y, e)) => fail("not correct Identifier or not correct expression: " + "Lambda(" + x + ",Lambda," + y + " , " + e + " , x.t= " + x.t + " , y.t= " + y.t)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'longIdentityWithI32.rise'"){
    val fileName: String = testFilePath + "longIdentityWithI32.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("jens"), r.Identifier("jens")) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'makePair.rise'"){
    val fileName: String = testFilePath + "makePair.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex.t match {
      case rt.PairType(rt.f32,rt.f32) => true
      case t => fail("The Type '"+t+"' is not the expected type.")
    }

    ex match {
      case
        r.App(r.App(rp.makePair(_), r.Literal(rS.FloatData(0), _)), r.Literal(rS.FloatData(0), _))
      => true
      case e => fail("not correct expression: " + e)
    }
  }

  test("parser should be able to parse 'matrixMult.rise'"){
    val fileName: String = testFilePath + "matrixMult.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent= parse(lexer.tokens)

    val functionName: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex_f.t match {
      case rt.DepFunType(n,rt.DepFunType(m,
      rt.FunType(rt.ArrayType(n1:rt.NatIdentifier,rt.ArrayType(m1:rt.NatIdentifier, rt.f32)),
      rt.FunType(rt.ArrayType(m2:rt.NatIdentifier, rt.ArrayType(n2:rt.NatIdentifier,rt.f32)),
      rt.ArrayType(n3:rt.NatIdentifier, rt.ArrayType(n4:rt.NatIdentifier,rt.f32))
      ))))        if n1.name.equals(n.name) && n2.name.equals(n.name) &&n3.name.equals(n.name) && n4.name.equals(n.name)
        && m1.name.equals(m.name) && m2.name.equals(m.name)
      => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }

    ex_f match {
      //Todo: How can I give rt.i32 to DepApp as second argument or how to do it else to give rt.i32 as an argument to an fkt
      case r.DepLambda(n, r.DepLambda(m,
      r.Lambda(r.Identifier("mat1"),r.Lambda(r.Identifier("mat2"),
      r.App(r.App(rp.mapSeq(_),
      r.Lambda(r.Identifier("vec1"),
      r.App(r.App(rp.mapSeq(_),

      r.Lambda(r.Identifier("vec2"),
      r.App(r.App(r.App(rp.reduceSeq(_), r.Lambda(
      r.Identifier("acc"),r.Lambda(
      r.Identifier("arg"), r.App(r.App(rp.add(_) , r.Identifier("acc")),r.Identifier("arg"))
      ))), r.Literal(rS.FloatData(0),_)),
      r.App(rp.toMem(_),r.App(r.App(rp.mapSeq(_), r.Lambda(r.Identifier("x"),
      r.App(r.App(rp.mul(_), r.App(rp.fst(_), r.Identifier("x"))),
      r.App(rp.snd(_), r.Identifier("x")) )
      )), r.App(r.App(rp.zip(_), r.Identifier("vec1")), r.Identifier("vec2")))))
      )),

      r.App(rp.transpose(_),r.Identifier("mat2"))
      ))),

      r.Identifier("mat1")
      ))
      )))=> true
      case r.DepLambda(n, e) => fail("Not correct deplambda: "
        +n.toString()+ " , " + e.toString())
      case a => fail("Not a DepLambda: " + a)
    }

    //    println("infer: " +rt.infer(ex_f))
    //Todo: What is this uniquenNames and infer, what I saw in https://github.com/rise-lang/rise/blob/feature/parallel-reduce/src/test/scala/rise/core/uniqueNamesCheck.scala
    //assert(r.uniqueNames.check(rt.infer(ex_f))) //Todo:This still fails
  }

  test("parser should be able to parse 'matMulOpenCL.rise'"){
    val fileName: String = testFilePath + "matMulOpenCL.rise"
    val file: FileReader = FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "matMul"
    val ex_f: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))
    ex_f.t match {
      case rt.DepFunType(r, rt.DepFunType(m, rt.DepFunType(n, rt.FunType(
      rt.ArrayType(n1:rt.NatIdentifier, rt.ArrayType(m1:rt.NatIdentifier, rt.f32)),
      rt.FunType(rt.ArrayType(m2:rt.NatIdentifier, rt.ArrayType(r1:rt.NatIdentifier, rt.f32)),
      rt.ArrayType(n2:rt.NatIdentifier, rt.ArrayType(r2:rt.NatIdentifier, rt.f32)))
      ))))
        if n1.name.equals(n.name) && n2.name.equals(n.name) &&m1.name.equals(m.name)&&m2.name.equals(m.name)
          &&r1.name.equals(r.name)&&r2.name.equals(r.name)
      => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }

    ex_f match {
      case dep@r.DepLambda(r1, dep2@r.DepLambda(m, dep3@r.DepLambda(n, lam@r.Lambda(r.Identifier("matrixA"),

      lam2@r.Lambda(r.Identifier("matrixB"), r.App(r.App(op.mapGlobal((0, _)),

      lam3@r.Lambda(r.Identifier("rowA"),
      r.App(r.App(op.mapLocal(1,_),

      lam4@r.Lambda(r.Identifier("columnB"),
      r.App(r.App(r.App(r.DepApp(op.oclReduceSeq(_), l:rt.AddressSpace.Local.type ),
      lam5@r.Lambda(r.Identifier("acc"), lam6@r.Lambda(r.Identifier("arg"),
      r.App(r.App(rp.add(_), r.Identifier("acc")), r.Identifier("arg"))))),r.Literal(rS.FloatData(0),_)),
      r.App(r.DepApp(op.oclToMem(_), l1:rt.AddressSpace.Local.type),

      r.App(r.App(rp.mapSeq(_), lam7@r.Lambda(r.Identifier("x"), appMulFstSnd@r.App(appMulFst@r.App(rp.mul(spanMul), appFst@r.App(rp.fst(spanFst), r.Identifier("x"))),
      r.App(rp.snd(spanSnd), r.Identifier("x"))))),

      r.App(r.App(rp.zip(_), r.Identifier("rowA")), r.Identifier("columnB"))

      )

      )

      )
      )
      ), appTransPose@r.App(rp.transpose(spanTranspose), r.Identifier("matrixB"))))

      ),
      r.Identifier("matrixA")
      ))

      )))) =>
      {
        dep.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/matMulOpenCL.rise")
            begin.row should equal(7)
            end.row should equal(9)
            begin.column should equal(9)
            end.column should equal(22)
          }
        }
        dep2.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/matMulOpenCL.rise")
            begin.row should equal(14)
            end.row should equal(9)
            begin.column should equal(9)
            end.column should equal(22)
          }
        }
        dep3.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/matMulOpenCL.rise")
            begin.row should equal(21)
            end.row should equal(9)
            begin.column should equal(9)
            end.column should equal(22)
          }
        }
        lam.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/matMulOpenCL.rise")
            begin.row should equal(2)
            end.row should equal(9)
            begin.column should equal(10)
            end.column should equal(22)
          }
        }
        lam2.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/matMulOpenCL.rise")
            begin.row should equal(11)
            end.row should equal(9)
            begin.column should equal(10)
            end.column should equal(22)
          }
        }
        lam3.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/matMulOpenCL.rise")
            begin.row should equal(4)
            end.row should equal(25)
            begin.column should equal(13)
            end.column should equal(20)
          }
        }
        lam4.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/matMulOpenCL.rise")
            begin.row should equal(8)
            end.row should equal(10)
            begin.column should equal(15)
            end.column should equal(18)
          }
        }
        lam5.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/matMulOpenCL.rise")
            begin.row should equal(38)
            end.row should equal(66)
            begin.column should equal(15)
            end.column should equal(15)
          }
        }
        lam6.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/matMulOpenCL.rise")
            begin.row should equal(47)
            end.row should equal(66)
            begin.column should equal(15)
            end.column should equal(15)
          }
        }
        lam7.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/matMulOpenCL.rise")
            begin.row should equal(17)
            end.row should equal(47)
            begin.column should equal(17)
            end.column should equal(17)
          }
        }
        spanFst match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/matMulOpenCL.rise")
            begin.row should equal(33)
            end.row should equal(36)
            begin.column should equal(17)
            end.column should equal(17)
          }
        }
        spanMul match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/matMulOpenCL.rise")
            begin.row should equal(30)
            end.row should equal(31)
            begin.column should equal(17)
            end.column should equal(17)
          }
        }
        spanSnd match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/matMulOpenCL.rise")
            begin.row should equal(41)
            end.row should equal(44)
            begin.column should equal(17)
            end.column should equal(17)
          }
        }
        spanTranspose match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/matMulOpenCL.rise")
            begin.row should equal(7)
            end.row should equal(16)
            begin.column should equal(20)
            end.column should equal(20)
          }
        }
        appMulFst.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/matMulOpenCL.rise")
            begin.row should equal(30)
            end.row should equal(38)
            begin.column should equal(17)
            end.column should equal(17)
          }
        }
        appFst.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/matMulOpenCL.rise")
            begin.row should equal(33)
            end.row should equal(38)
            begin.column should equal(17)
            end.column should equal(17)
          }
        }
        appMulFstSnd.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/matMulOpenCL.rise")
            begin.row should equal(30)
            end.row should equal(46)
            begin.column should equal(17)
            end.column should equal(17)
          }
        }
        appTransPose.span match {
          case None => fail("The Span should not be None")
          case Some(Span(file, Range(begin, end))) => {
            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/matMulOpenCL.rise")
            begin.row should equal(7)
            end.row should equal(24)
            begin.column should equal(20)
            end.column should equal(20)
          }
        }
      }
      case r.DepLambda(n, e) => fail("Not correct deplambda: "
        +n.toString()+ " , " + e.toString())
      case a => fail("Not a DepLambda: " + a)
    }
    ex_f.span match {
      case None => fail("The Span should not be None")
      case Some(Span(file, Range(begin, end))) => {
        file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/matMulOpenCL.rise")
        begin.row should equal(7)
        end.row should equal(9)
        begin.column should equal(9)
        end.column should equal(22)
      }
    }
  }

  test("parser should be able to parse 'matrixMultWithComments.rise'"){
    val fileName: String = testFilePath + "matrixMultWithComments.rise"
    val file: FileReader = FileReader(fileName)
    val lexer: RecognizeLexeme = RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex_f.t match {
      case rt.DepFunType(n,rt.DepFunType(m,
      rt.FunType(rt.ArrayType(n1:rt.NatIdentifier,rt.ArrayType(m1:rt.NatIdentifier, rt.f32)),
      rt.FunType(rt.ArrayType(m2:rt.NatIdentifier, rt.ArrayType(n2:rt.NatIdentifier,rt.f32)),
      rt.ArrayType(n3:rt.NatIdentifier, rt.ArrayType(n4:rt.NatIdentifier,rt.f32))
      ))))        if n1.name.equals(n.name) && n2.name.equals(n.name) &&n3.name.equals(n.name) && n4.name.equals(n.name)
        && m1.name.equals(m.name) && m2.name.equals(m.name)
      => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }

    ex_f match {
      //Todo: How can I give rt.i32 to DepApp as second argument or how to do it else to give rt.i32 as an argument to an fkt
      case r.DepLambda(n, r.DepLambda(m,
      r.Lambda(r.Identifier("mat1"),r.Lambda(r.Identifier("mat2"),
      r.App(r.App(rp.mapSeq(_),
      r.Lambda(r.Identifier("vec1"),
      r.App(r.App(rp.mapSeq(_),

      r.Lambda(r.Identifier("vec2"),
      r.App(r.App(r.App(rp.reduceSeq(_), r.Lambda(
      r.Identifier("acc"),r.Lambda(
      r.Identifier("arg"), r.App(r.App(rp.add(_) , r.Identifier("acc")),r.Identifier("arg"))
      ))),r.Literal(rS.FloatData(0),_)),r.App(rp.toMem(_),
      r.App(r.App(rp.mapSeq(_), r.Lambda(r.Identifier("x"),
      r.App(r.App(rp.mul(_), r.App(rp.fst(_), r.Identifier("x"))),
      r.App(rp.snd(_), r.Identifier("x")) )
      )), r.App(r.App(rp.zip(_), r.Identifier("vec1")), r.Identifier("vec2")))))
      )),

      r.App(rp.transpose(_),r.Identifier("mat2"))
      ))),

      r.Identifier("mat1")
      ))
      )))=> true
      case r.DepLambda(n, e) => fail("Not correct deplambda: "
        +n.toString()+ " , " + e.toString())
      case a => fail("Not a DepLambda: " + a)
    }

    //    println("infer: " +rt.infer(ex_f))
    //Todo: What is this uniquenNames and infer, what I saw in https://github.com/rise-lang/rise/blob/feature/parallel-reduce/src/test/scala/rise/core/uniqueNamesCheck.scala
    //assert(r.uniqueNames.check(rt.infer(ex_f))) //Todo:This still fails
  }

  test("parser should be able to parse 'minus.rise'"){
    val fileName: String = testFilePath + "minus.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("x"), r.App(r.App(rp.sub(_), r.Identifier("x")), r.Literal(rS.FloatData(5), _))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'nbodyWithMoreParentheses.rise'"){
    val fileName: String = testFilePath + "nbodyWithMoreParentheses.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName2: String = "nbody"
    val ex_g: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))

    ex_g.t match {
      case rt.DepFunType(n:rt.NatIdentifier,rt.FunType(
      rt.ArrayType(n1: rt.NatIdentifier,rt.VectorType(num1:rt.Nat, rt.f32)),
      rt.FunType(
      rt.ArrayType(n2: rt.NatIdentifier,rt.VectorType(num2:rt.Nat, rt.f32)),
      rt.FunType(rt.f32, rt.FunType(rt.f32, rt.ArrayType(n3:rt.NatIdentifier,
      rt.PairType(rt.VectorType(num3:rt.Nat, rt.f32),rt.VectorType(num4:rt.Nat, rt.f32)))))
      )
      ))

        if n1.name.equals(n.name) && n2.name.equals(n.name)
          && n3.name.equals(n.name)
          &&num1.eval.equals(4)&&num2.eval.equals(4)&&num3.eval.equals(4)&&num4.eval.equals(4)
      => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }
    val N = 512
    val tileX = 256
    val tileY = 1

    val random = new scala.util.Random()
    val pos = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))
    val vel = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))

    val localSizeAMD = LocalSize(128)
    val globalSizeAMD = GlobalSize(N)

    val localSizeNVIDIA = LocalSize((tileX, tileY))
    val globalSizeNVIDIA = GlobalSize((N, tileY))

    test_util.runsWithSameResult(Seq(
      ("parser NVIDIA", runKernel(gen.OpenCLKernel(ex_g), localSizeAMD, globalSizeAMD, pos, vel)),
      ("original NVIDIA", runKernel(gen.OpenCLKernel(apps.nbody.nvidia), localSizeAMD, globalSizeAMD, pos, vel))
    ))
  }
  test("parser should be able to parse 'NBodyNatDeclaredBeforeAsConstant.rise'"){
    val fileName: String = testFilePath + "NBodyNatDeclaredBeforeAsConstant.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName2: String = "nbody"
    val ex_g: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))

    ex_g.t match {
      case rt.DepFunType(n:rt.NatIdentifier,rt.FunType(
      rt.ArrayType(n1: rt.NatIdentifier,rt.VectorType(num1:rt.Nat, rt.f32)),
      rt.FunType(
      rt.ArrayType(n2: rt.NatIdentifier,rt.VectorType(num2:rt.Nat, rt.f32)),
      rt.FunType(rt.f32, rt.FunType(rt.f32, rt.ArrayType(n3:rt.NatIdentifier,
      rt.PairType(rt.VectorType(num3:rt.Nat, rt.f32),rt.VectorType(num4:rt.Nat, rt.f32)))))
      )
      ))

        if n1.name.equals(n.name) && n2.name.equals(n.name)
          && n3.name.equals(n.name)
          &&num1.eval.equals(4)&&num2.eval.equals(4)&&num3.eval.equals(4)&&num4.eval.equals(4)
      => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }
    val N = 512
    val tileX = 256
    val tileY = 1

    val random = new scala.util.Random()
    val pos = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))
    val vel = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))

    val localSizeAMD = LocalSize(128)
    val globalSizeAMD = GlobalSize(N)

    val localSizeNVIDIA = LocalSize((tileX, tileY))
    val globalSizeNVIDIA = GlobalSize((N, tileY))

    test_util.runsWithSameResult(Seq(
      ("parser NVIDIA", runKernel(gen.OpenCLKernel(ex_g), localSizeAMD, globalSizeAMD, pos, vel)),
      ("original NVIDIA", runKernel(gen.OpenCLKernel(apps.nbody.nvidia), localSizeAMD, globalSizeAMD, pos, vel))
    ))
  }
  test("parser should be able to parse 'NBodyNatUndIDDeclaredBeforeAsConstant.rise'"){
    val fileName: String = testFilePath + "NBodyNatUndIDDeclaredBeforeAsConstant.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName2: String = "nbody"
    val ex_g: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))

    ex_g.t match {
      case rt.DepFunType(n:rt.NatIdentifier,rt.FunType(
      rt.ArrayType(n1: rt.NatIdentifier,rt.VectorType(num1:rt.Nat, rt.f32)),
      rt.FunType(
      rt.ArrayType(n2: rt.NatIdentifier,rt.VectorType(num2:rt.Nat, rt.f32)),
      rt.FunType(rt.f32, rt.FunType(rt.f32, rt.ArrayType(n3:rt.NatIdentifier,
      rt.PairType(rt.VectorType(num3:rt.Nat, rt.f32),rt.VectorType(num4:rt.Nat, rt.f32)))))
      )
      ))

        if n1.name.equals(n.name) && n2.name.equals(n.name)
          && n3.name.equals(n.name)
          &&num1.eval.equals(4)&&num2.eval.equals(4)&&num3.eval.equals(4)&&num4.eval.equals(4)
      => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }
    val N = 512
    val tileX = 256
    val tileY = 1

    val random = new scala.util.Random()
    val pos = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))
    val vel = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))

    val localSizeAMD = LocalSize(128)
    val globalSizeAMD = GlobalSize(N)

    val localSizeNVIDIA = LocalSize((tileX, tileY))
    val globalSizeNVIDIA = GlobalSize((N, tileY))

    test_util.runsWithSameResult(Seq(
      ("parser NVIDIA", runKernel(gen.OpenCLKernel(ex_g), localSizeAMD, globalSizeAMD, pos, vel)),
      ("original NVIDIA", runKernel(gen.OpenCLKernel(apps.nbody.nvidia), localSizeAMD, globalSizeAMD, pos, vel))
    ))
  }
  test("parser should be able to parse 'NBodyNatUndIdAsConstantAndCFunctionsUsed.rise'"){
    val fileName: String = testFilePath + "NBodyNatUndIdAsConstantAndCFunctionsUsed.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName2: String = "nbody"
    val ex_g: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))

    ex_g.t match {
      case rt.DepFunType(n:rt.NatIdentifier,rt.FunType(
      rt.ArrayType(n1: rt.NatIdentifier,rt.VectorType(num1:rt.Nat, rt.f32)),
      rt.FunType(
      rt.ArrayType(n2: rt.NatIdentifier,rt.VectorType(num2:rt.Nat, rt.f32)),
      rt.FunType(rt.f32, rt.FunType(rt.f32, rt.ArrayType(n3:rt.NatIdentifier,
      rt.PairType(rt.VectorType(num3:rt.Nat, rt.f32),rt.VectorType(num4:rt.Nat, rt.f32)))))
      )
      ))

        if n1.name.equals(n.name) && n2.name.equals(n.name)
          && n3.name.equals(n.name)
          &&num1.eval.equals(4)&&num2.eval.equals(4)&&num3.eval.equals(4)&&num4.eval.equals(4)
      => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }
    val N = 512
    val tileX = 256
    val tileY = 1

    val random = new scala.util.Random()
    val pos = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))
    val vel = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))

    val localSizeAMD = LocalSize(128)
    val globalSizeAMD = GlobalSize(N)

    val localSizeNVIDIA = LocalSize((tileX, tileY))
    val globalSizeNVIDIA = GlobalSize((N, tileY))

    test_util.runsWithSameResult(Seq(
      ("parser NVIDIA", runKernel(gen.OpenCLKernel(ex_g), localSizeAMD, globalSizeAMD, pos, vel)),
      ("original NVIDIA", runKernel(gen.OpenCLKernel(apps.nbody.nvidia), localSizeAMD, globalSizeAMD, pos, vel))
    ))
  }
  test("parser should be able to parse 'nbodyNatDeclaredBefore.rise'"){
    val fileName: String = testFilePath + "nbodyNatDeclaredBefore.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName2: String = "nbody"
    val ex_g: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))

    ex_g.t match {
      case rt.DepFunType(n:rt.NatIdentifier,rt.FunType(
      rt.ArrayType(n1: rt.NatIdentifier,rt.VectorType(num1:rt.Nat, rt.f32)),
      rt.FunType(
      rt.ArrayType(n2: rt.NatIdentifier,rt.VectorType(num2:rt.Nat, rt.f32)),
      rt.FunType(rt.f32, rt.FunType(rt.f32, rt.ArrayType(n3:rt.NatIdentifier,
      rt.PairType(rt.VectorType(num3:rt.Nat, rt.f32),rt.VectorType(num4:rt.Nat, rt.f32)))))
      )
      ))

        if n1.name.equals(n.name) && n2.name.equals(n.name)
          && n3.name.equals(n.name)
          &&num1.eval.equals(4)&&num2.eval.equals(4)&&num3.eval.equals(4)&&num4.eval.equals(4)
      => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }
    val N = 512
    val tileX = 256
    val tileY = 1

    val random = new scala.util.Random()
    val pos = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))
    val vel = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))

    val localSizeAMD = LocalSize(128)
    val globalSizeAMD = GlobalSize(N)

    val localSizeNVIDIA = LocalSize((tileX, tileY))
    val globalSizeNVIDIA = GlobalSize((N, tileY))

    test_util.runsWithSameResult(Seq(
      ("parser NVIDIA", runKernel(gen.OpenCLKernel(ex_g), localSizeAMD, globalSizeAMD, pos, vel)),
      ("original NVIDIA", runKernel(gen.OpenCLKernel(apps.nbody.nvidia), localSizeAMD, globalSizeAMD, pos, vel))
    ))
  }
  test("parser should be able to parse 'nbodyNatNotDeclaredBeforeButTheFunctionIDIsSeperated.rise'"){
    val fileName: String = testFilePath + "nbodyNatNotDeclaredBeforeButTheFunctionIDIsSeperated.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName2: String = "nbody"
    val ex_g: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))

    ex_g.t match {
      case rt.DepFunType(n:rt.NatIdentifier,rt.FunType(
      rt.ArrayType(n1: rt.NatIdentifier,rt.VectorType(num1:rt.Nat, rt.f32)),
      rt.FunType(
      rt.ArrayType(n2: rt.NatIdentifier,rt.VectorType(num2:rt.Nat, rt.f32)),
      rt.FunType(rt.f32, rt.FunType(rt.f32, rt.ArrayType(n3:rt.NatIdentifier,
      rt.PairType(rt.VectorType(num3:rt.Nat, rt.f32),rt.VectorType(num4:rt.Nat, rt.f32)))))
      )
      ))

        if n1.name.equals(n.name) && n2.name.equals(n.name)
          && n3.name.equals(n.name)
          &&num1.eval.equals(4)&&num2.eval.equals(4)&&num3.eval.equals(4)&&num4.eval.equals(4)
      => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }
    val N = 512
    val tileX = 256
    val tileY = 1

    val random = new scala.util.Random()
    val pos = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))
    val vel = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))

    val localSizeAMD = LocalSize(128)
    val globalSizeAMD = GlobalSize(N)

    val localSizeNVIDIA = LocalSize((tileX, tileY))
    val globalSizeNVIDIA = GlobalSize((N, tileY))

    test_util.runsWithSameResult(Seq(
      ("parser NVIDIA", runKernel(gen.OpenCLKernel(ex_g), localSizeAMD, globalSizeAMD, pos, vel)),
      ("original NVIDIA", runKernel(gen.OpenCLKernel(apps.nbody.nvidia), localSizeAMD, globalSizeAMD, pos, vel))
    ))
  }
  test("parser should be able to parse 'nbodyNatNotDeclaredBefore.rise'"){
    val fileName: String = testFilePath + "nbodyNatNotDeclaredBefore.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName2: String = "nbody"
    val ex_g: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))

    ex_g.t match {
      case rt.DepFunType(n:rt.NatIdentifier,rt.FunType(
      rt.ArrayType(n1: rt.NatIdentifier,rt.VectorType(num1:rt.Nat, rt.f32)),
      rt.FunType(
      rt.ArrayType(n2: rt.NatIdentifier,rt.VectorType(num2:rt.Nat, rt.f32)),
      rt.FunType(rt.f32, rt.FunType(rt.f32, rt.ArrayType(n3:rt.NatIdentifier,
      rt.PairType(rt.VectorType(num3:rt.Nat, rt.f32),rt.VectorType(num4:rt.Nat, rt.f32)))))
      )
      ))

        if n1.name.equals(n.name) && n2.name.equals(n.name)
          && n3.name.equals(n.name)
          &&num1.eval.equals(4)&&num2.eval.equals(4)&&num3.eval.equals(4)&&num4.eval.equals(4)
      => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }
    val N = 512
    val tileX = 256
    val tileY = 1

    val random = new scala.util.Random()
    val pos = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))
    val vel = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))

    val localSizeAMD = LocalSize(128)
    val globalSizeAMD = GlobalSize(N)

    val localSizeNVIDIA = LocalSize((tileX, tileY))
    val globalSizeNVIDIA = GlobalSize((N, tileY))

    test_util.runsWithSameResult(Seq(
      ("parser NVIDIA", runKernel(gen.OpenCLKernel(ex_g), localSizeAMD, globalSizeAMD, pos, vel)),
      ("original NVIDIA", runKernel(gen.OpenCLKernel(apps.nbody.nvidia), localSizeAMD, globalSizeAMD, pos, vel))
    ))
  }
  test("parser should be able to parse 'nbody.rise'"){
    val fileName: String = testFilePath + "nbody.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val calcAccName = "calcAcc"
    val ex_calcAcc = riseExprByIdent.get(calcAccName).getOrElse(fail("The function '" + calcAccName+ "' does not exist!!!"))
    println("\nexCalcAcc-------\n"+gen.OpenCLKernel(ex_calcAcc)+"\n-------\n")
    val functionName2: String = "nbody"
    val ex_g: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))

    ex_g.t match {
      case rt.DepFunType(n:rt.NatIdentifier,rt.FunType(
      rt.ArrayType(n1: rt.NatIdentifier,rt.VectorType(num1:rt.Nat, rt.f32)),
      rt.FunType(
      rt.ArrayType(n2: rt.NatIdentifier,rt.VectorType(num2:rt.Nat, rt.f32)),
      rt.FunType(rt.f32, rt.FunType(rt.f32, rt.ArrayType(n3:rt.NatIdentifier,
      rt.PairType(rt.VectorType(num3:rt.Nat, rt.f32),rt.VectorType(num4:rt.Nat, rt.f32)))))
      )
      ))

        if n1.name.equals(n.name) && n2.name.equals(n.name)
          && n3.name.equals(n.name)
          &&num1.eval.equals(4)&&num2.eval.equals(4)&&num3.eval.equals(4)&&num4.eval.equals(4)
      => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }
    //Todo: finish match
    //    ex_g match {
    //      case dep@r.DepLambda(n: rt.NatIdentifier, lam@r.Lambda(r.Identifier("pos"), lam2@r.Lambda(
    //      r.Identifier("vel"), lam3@r.Lambda(r.Identifier("espSqr"), lam4@r.Lambda(r.Identifier("deltaT"),
    //      r.App(rp.join(_), r.App(rp.join(_), r.App(r.App(op.mapWorkGroup(1,_),r.App(rp.join(_), r.App(
    //      r.App(op.mapWorkGroup(0,_),
    //
    //      lam5@r.Lambda(r.Identifier("p1Chunk"), lam6@r.Lambda(r.Identifier("newP1Chunk"),
    //      r.App(r.App(r.App(op.mapLocal(1,_),
    //
    //      lam7@r.Lambda(r.Identifier("bla"), r.App(r.App(op.mapLocal(0,_), lam8@r.Lambda(r.Identifier("p1A"),
    //      r.App(r.App(r.App(r.App(r.Identifier("update"), r.App(rp.fst(_), r.App(rp.fst(_), r.Identifier("p1A")))),
    //      r.App(rp.fst(_), r.App(rp.snd(_), r.Identifier("p1A")))), r.Identifier("deltaT")), r.App(rp.snd(_), r.Identifier("p1A"))))),
    //      r.App(r.App(rp.zip(_), r.Identifier("newP1Chunk")), r.Identifier("bla"))))
    //
    //      ),
    //      r.App(r.App(r.DepApp(op.oclReduceSeq(_), l:rt.AddressSpace.Local.type ),
    //      lam9@r.Lambda(r.Identifier("accA"), lam10@r.Lambda(r.Identifier("p2A"),
    //
    //      r.App(r.App(rp.let(_), r.App(r.App(r.DepApp(op.oclToMem(_),
    //      l1:rt.AddressSpace.Local.type
    //      ), r.App(op.mapLocal(1,_), r.App(op.mapLocal(0,_), lam11@r.Lambda(r.Identifier("x1"),r.Identifier("x1"))))),
    //      r.Identifier("p2A")
    //      )),
    //      lam12@r.Lambda(r.Identifier("p2Local"), r.App(r.App(op.mapLocal(1,_), lam13@r.Lambda(r.Identifier("accDim"),
    //      r.App(r.App(op.mapLocal(0,_),
    //      lam14@r.Lambda(r.Identifier("p1B"), r.App(r.App(r.App(r.DepApp(op.oclReduceSeq(_), p:rt.AddressSpace.Private.type ),
    //
    //      lam15@r.Lambda(r.Identifier("accB"), lam16@r.Lambda(r.Identifier("p2B"), r.App(r.App(r.App(r.App(r.App(r.Identifier("calcAcc"),
    //      r.App(rp.fst(_),
    //      r.App(rp.fst(_), r.Identifier("p1B")))),
    //      r.Identifier("p2B")), r.Identifier("deltaT")), r.Identifier("espSqr")), r.Identifier("accB")))
    //
    //      )), r.App(rp.snd(_), r.Identifier("p1B"))), r.App(rp.fst(_), r.Identifier("accDim2")))))
    //      , r.App(r.App(rp.zip(_), r.Identifier("newP1Chunk")), r.App(rp.snd(_), r.App(rp.snd(_), r.Identifier("accDim"))))
    //      ))),r.App(r.App(rp.zip(_), r.Identifier("p2Local")), r.Identifier("accA")))
    //
    //      ))
    //      ))),
    //      r.App(r.App(op.mapLocal(1,_), r.App(op.mapLocal(0,_),lam17@r.Lambda(r.Identifier("x2"),r.Identifier("x2")))),
    //      r.App(rp.generate(_),
    //
    //      r.App(rp.vectorFromScalar(_), r.Literal(rS.IntData(0), _))
    //
    //      ))
    //      )), //probably has the 0 of the Type Nat
    //
    //      r.App(r.DepApp(rp.split(_), n1:rt.Nat), r.App(r.DepApp(rp.split(_), n2:rt.Nat),
    //      r.Identifier("pos")))
    //      )
    //
    //      ))
    //
    //      ),
    //      r.DepApp(rp.split(_), n3:rt.Nat)
    //      ))
    //      ),
    //      r.App(r.DepApp(rp.split(_), n4:rt.NatIdentifier),
    //      r.App(r.App(rp.zip(_), r.Identifier("pos")), r.Identifier("vel"))
    //
    //      )
    //      ))
    //      ))))))
    //        if n4.name.equals(n.name)&& n1.eval.equals(1) &&n2.eval.equals(256)&&n3.eval.equals(256)
    //      => {
    //        dep.span match {
    //          case None => fail("The Span should not be None")
    //          case Some(Span(file, Range(begin, end))) => {
    //            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/nbody.rise")
    //            begin.row should equal(6)
    //            end.row should equal(3)
    //            begin.column should equal(6)
    //            end.column should equal(47)
    //          }
    //        }
    //        lam.span match {
    //          case None => fail("The Span should not be None")
    //          case Some(Span(file, Range(begin, end))) => {
    //            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/nbody.rise")
    //            begin.row should equal(14)
    //            end.row should equal(3)
    //            begin.column should equal(6)
    //            end.column should equal(47)
    //          }
    //        }
    //        lam2.span match {
    //          case None => fail("The Span should not be None")
    //          case Some(Span(file, Range(begin, end))) => {
    //            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/nbody.rise")
    //            begin.row should equal(20)
    //            end.row should equal(3)
    //            begin.column should equal(6)
    //            end.column should equal(47)
    //          }
    //        }
    //        lam3.span match {
    //          case None => fail("The Span should not be None")
    //          case Some(Span(file, Range(begin, end))) => {
    //            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/nbody.rise")
    //            begin.row should equal(26)
    //            end.row should equal(3)
    //            begin.column should equal(6)
    //            end.column should equal(47)
    //          }
    //        }
    //        lam4.span match {
    //          case None => fail("The Span should not be None")
    //          case Some(Span(file, Range(begin, end))) => {
    //            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/nbody.rise")
    //            begin.row should equal(35)
    //            end.row should equal(3)
    //            begin.column should equal(6)
    //            end.column should equal(47)
    //          }
    //        }
    //        lam5.span match {
    //          case None => fail("The Span should not be None")
    //          case Some(Span(file, Range(begin, end))) => {
    //            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/nbody.rise")
    //            begin.row should equal(8)
    //            end.row should equal(36)
    //            begin.column should equal(13)
    //            end.column should equal(41)
    //          }
    //        }
    //        lam6.span match {
    //          case None => fail("The Span should not be None")
    //          case Some(Span(file, Range(begin, end))) => {
    //            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/nbody.rise")
    //            begin.row should equal(8)
    //            end.row should equal(36)
    //            begin.column should equal(14)
    //            end.column should equal(41)
    //          }
    //        }
    //        lam7.span match {
    //          case None => fail("The Span should not be None")
    //          case Some(Span(file, Range(begin, end))) => {
    //            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/nbody.rise")
    //            begin.row should equal(10)
    //            end.row should equal(30)
    //            begin.column should equal(16)
    //            end.column should equal(21)
    //          }
    //        }
    //        lam8.span match {
    //          case None => fail("The Span should not be None")
    //          case Some(Span(file, Range(begin, end))) => {
    //            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/nbody.rise")
    //            begin.row should equal(12)
    //            end.row should equal(67)
    //            begin.column should equal(18)
    //            end.column should equal(19)
    //          }
    //        }
    //        lam9.span match {
    //          case None => fail("The Span should not be None")
    //          case Some(Span(file, Range(begin, end))) => {
    //            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/nbody.rise")
    //            begin.row should equal(10)
    //            end.row should equal(11)
    //            begin.column should equal(26)
    //            end.column should equal(37)
    //          }
    //        }
    //        lam10.span match {
    //          case None => fail("The Span should not be None")
    //          case Some(Span(file, Range(begin, end))) => {
    //            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/nbody.rise")
    //            begin.row should equal(29)
    //            end.row should equal(11)
    //            begin.column should equal(26)
    //            end.column should equal(37)
    //          }
    //        }
    //        lam11.span match {
    //          case None => fail("The Span should not be None")
    //          case Some(Span(file, Range(begin, end))) => {
    //            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/nbody.rise")
    //            begin.row should equal(48)
    //            end.row should equal(55)
    //            begin.column should equal(27)
    //            end.column should equal(27)
    //          }
    //        }
    //        lam12.span match {
    //          case None => fail("The Span should not be None")
    //          case Some(Span(file, Range(begin, end))) => {
    //            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/nbody.rise")
    //            begin.row should equal(12)
    //            end.row should equal(32)
    //            begin.column should equal(29)
    //            end.column should equal(36)
    //          }
    //        }
    //        lam13.span match {
    //          case None => fail("The Span should not be None")
    //          case Some(Span(file, Range(begin, end))) => {
    //            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/nbody.rise")
    //            begin.row should equal(14)
    //            end.row should equal(51)
    //            begin.column should equal(30)
    //            end.column should equal(35)
    //          }
    //        }
    //        lam14.span match {
    //          case None => fail("The Span should not be None")
    //          case Some(Span(file, Range(begin, end))) => {
    //            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/nbody.rise")
    //            begin.row should equal(16)
    //            end.row should equal(41)
    //            begin.column should equal(31)
    //            end.column should equal(34)
    //          }
    //        }
    //        lam15.span match {
    //          case None => fail("The Span should not be None")
    //          case Some(Span(file, Range(begin, end))) => {
    //            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/nbody.rise")
    //            begin.row should equal(18)
    //            end.row should equal(64)
    //            begin.column should equal(32)
    //            end.column should equal(33)
    //          }
    //        }
    //        lam16.span match {
    //          case None => fail("The Span should not be None")
    //          case Some(Span(file, Range(begin, end))) => {
    //            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/nbody.rise")
    //            begin.row should equal(31)
    //            end.row should equal(64)
    //            begin.column should equal(32)
    //            end.column should equal(33)
    //          }
    //        }
    //        lam17.span match {
    //          case None => fail("The Span should not be None")
    //          case Some(Span(file, Range(begin, end))) => {
    //            file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/nbody.rise")
    //            begin.row should equal(35)
    //            end.row should equal(42)
    //            begin.column should equal(39)
    //            end.column should equal(39)
    //          }
    //        }
    //      }
    //      case r.DepLambda(n, e) => {
    //        //print_nbody()
    //        fail("Not correct deplambda: "
    //          +n.toString()+ " , " + e.toString())
    //      }
    //      case a => fail("Not a DepLambda: " + a)
    //    }
    //    ex_g.span match {
    //      case None => fail("The Span should not be None")
    //      case Some(Span(file, Range(begin, end))) => {
    //        file.fileName should equal("src/test/scala/parser/readFiles/filesToLex/nbody.rise")
    //        begin.row should equal(6)
    //        end.row should equal(3)
    //        begin.column should equal(6)
    //        end.column should equal(47)
    //      }
    //    }

    val N = 512
    val tileX = 256
    val tileY = 1

    val random = new scala.util.Random()
    val pos = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))
    val vel = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))

    val localSizeAMD = LocalSize(128)
    val globalSizeAMD = GlobalSize(N)


    test_util.runsWithSameResult(Seq(
      ("parser NVIDIA", runKernel(gen.OpenCLKernel(ex_g), localSizeAMD, globalSizeAMD, pos, vel)),
      ("original NVIDIA", runKernel(gen.OpenCLKernel(apps.nbody.nvidia), localSizeAMD, globalSizeAMD, pos, vel))
    ))
  }

  test("parser should be able to parse 'negation.rise'"){
    val fileName: String = testFilePath + "negation.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("y"), r.App(rp.neg(_), r.Identifier("y"))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'negationWithBool.rise'"){
    val fileName: String = testFilePath + "negationWithBool.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("b"), r.App(rp.neg(_), r.Identifier("b"))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'not.rise'"){
    val fileName: String = testFilePath + "not.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("b"), r.App(rp.not(_), r.Identifier("b"))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  //Todo: Should be fixed in future work (The Error is, because we have in I32 and an Int Type and it automatically converts
  //the result into Int instead of i32): <inference exception: could not solve constraints List((i32 -> i32)  ~  (int -> int)) in Some((1,2-10)>
  ignore("parser should be able to parse 'plus.rise'" ) {
    val fileName: String = testFilePath + "plus.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex match {
      case r.Lambda(r.Identifier("x"), r.App(r.App(rp.add(_), r.Identifier("x")), r.Literal(rS.IntData(5), _))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'PrimitiveFstTwoDep.rise'"){
    val fileName: String = testFilePath + "PrimitiveFstTwoDep.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex_f.t match {
      case rt.DepFunType(d,rt.DepFunType(t,
      rt.FunType(rt.PairType(d1:rt.DataTypeIdentifier, t1:rt.DataTypeIdentifier), d2:rt.DataTypeIdentifier)))
        if d1.name.equals(d.name)&& d2.name.equals(d.name)
          && t1.name.equals(t.name) => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }

    ex_f match {
      //Todo: How can I give rt.i32 to DepApp as second argument or how to do it else to give rt.i32 as an argument to an fkt
      case r.DepLambda(d, r.DepLambda(t,
      r.Lambda(r.Identifier("t"), r.App(rp.fst(_), r.Identifier("t")))))
      => true
      case r.DepLambda(n, e) => fail("Not correct deplambda: "
        +n.toString()+ " , " + e.toString())
      case a => fail("Not a DepLambda: " + a)
    }
  }

  test("parser should be able to parse 'PrimitiveSndTwoDep.rise'"){
    val fileName: String = testFilePath + "PrimitiveSndTwoDep.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex_f.t match {
      case rt.DepFunType(d,rt.DepFunType(t,
      rt.FunType(rt.PairType(d1:rt.DataTypeIdentifier, t1:rt.DataTypeIdentifier), t2:rt.DataTypeIdentifier)))
        if d1.name.equals(d.name)
          && t1.name.equals(t.name) && t2.name.equals(t.name) => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }

    ex_f match {
      //Todo: How can I give rt.i32 to DepApp as second argument or how to do it else to give rt.i32 as an argument to an fkt
      case r.DepLambda(d, r.DepLambda(t,
      r.Lambda(r.Identifier("t"), r.App(rp.snd(_), r.Identifier("t")))))
      => true
      case r.DepLambda(n, e) => fail("Not correct deplambda: "
        +n.toString()+ " , " + e.toString())
      case a => fail("Not a DepLambda: " + a)
    }
  }

  test("parser should be able to parse 'PrimitiveSndDep.rise'"){
    val fileName: String = testFilePath + "PrimitiveSndDep.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex_f.t match {
      case rt.DepFunType(d,
      rt.FunType(rt.PairType(d1:rt.DataTypeIdentifier, rt.i32), rt.i32))
        if d1.name.equals(d.name) => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }

    ex_f match {
      //Todo: How can I give rt.i32 to DepApp as second argument or how to do it else to give rt.i32 as an argument to an fkt
      case r.DepLambda(d,
      r.Lambda(r.Identifier("t"), r.App(rp.snd(_), r.Identifier("t")))) => true
      case r.DepLambda(n, e) => fail("Not correct deplambda: "
        +n.toString()+ " , " + e.toString())
      case a => fail("Not a DepLambda: " + a)
    }
  }

  test("parser should be able to parse 'PrimitiveFstDep.rise'"){
    val fileName: String = testFilePath + "PrimitiveFstDep.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex_f.t match {
      case rt.DepFunType(d,
      rt.FunType(rt.PairType(d1:rt.DataTypeIdentifier, rt.i32), d2:rt.DataTypeIdentifier))
        if d1.name.equals(d.name) &&d2.name.equals(d.name)=> true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }

    ex_f match {
      //Todo: How can I give rt.i32 to DepApp as second argument or how to do it else to give rt.i32 as an argument to an fkt
      case r.DepLambda(d,
      r.Lambda(r.Identifier("t"), r.App(rp.fst(_), r.Identifier("t"))))
      => true
      case r.DepLambda(n, e) => fail("Not correct deplambda: "
        +n.toString()+ " , " + e.toString())
      case a => fail("Not a DepLambda: " + a)
    }
  }

  test("parser should be able to parse 'PrimitiveSnd.rise'"){
    val fileName: String = testFilePath + "PrimitiveSnd.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex_f.t match {
      case rt.FunType(rt.PairType(rt.i32, rt.i32), rt.i32) => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }
    ex_f match {
      //Todo: How can I give rt.i32 to DepApp as second argument or how to do it else to give rt.i32 as an argument to an fkt
      case r.Lambda(r.Identifier("t"), r.App(rp.snd(_), r.Identifier("t"))) => true
      case r.Lambda(n, e) => fail("Not correct lambda: "
        +n.toString()+ " , " + e.toString())
      case a => fail("Not a DepLambda: " + a)
    }
  }

  test("parser should be able to parse 'PrimitiveFst.rise'"){
    val fileName: String = testFilePath + "PrimitiveFst.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex_f.t match {
      case rt.FunType(rt.PairType(rt.i32, rt.i32), rt.i32) => true
      case t => fail("The Type '" + t + "' is not the expected type.")
    }

    ex_f match {
      //Todo: How can I give rt.i32 to DepApp as second argument or how to do it else to give rt.i32 as an argument to an fkt
      case r.Lambda(r.Identifier("t"), r.App(rp.fst(_), r.Identifier("t")))=> true
      case r.Lambda(n, e) => fail("Not correct lambda: "
        +n.toString()+ " , " + e.toString())
      case a => fail("Not a DepLambda: " + a)
    }
  }

  test("parser should be able to parse 'secondFktCallsFirstFkt.rise'"){
    val fileName: String = testFilePath + "secondFktCallsFirstFkt.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))
    val functionName2: String = "h"
    val ex_h: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))
    val functionName3: String = "g"
    val ex_g: r.Expr = riseExprByIdent.get(functionName3).getOrElse(fail("The function '" + functionName3 + "' does not exist!!!"))


    ex_f.t match {
      case rt.FunType(rt.f32, rt.f32) => true
      case t => fail("The Type '"+t+"' is not the expected type.")
    }
    ex_h.t match {
      case rt.FunType(rt.f32, rt.f32) => true
      case t => fail("The Type '"+t+"' is not the expected type.")
    }

    ex_h match {
      case r.Lambda(r.Identifier("x"), r.App(ex_f,
      r.App(r.App(rp.add(_), r.Identifier("x")),r.Identifier("x") ))) => ex_f match {
        case r.Lambda(r.Identifier("x"), r.Identifier("x")) => true
        case sub => fail("subexpr '"+ sub+"' is wrong")
      }
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }

    val f1 = gen.CProgram(ex_h)
    val f2 = gen.CProgram(ex_g)
    println("inOne:"+ f1)
    println("inTwo:"+ f2)
    f1.code should equal(f2.code)
  }
  test("parser should be able to parse 'secondFktCallsFirstFktAndItIsOnlyDeclared.rise'"){
    val fileName: String = testFilePath + "secondFktCallsFirstFktAndItIsOnlyDeclared.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))
    val functionName2: String = "h"
    val ex_h: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))

    ex_f.t match {
      case rt.FunType(rt.f32, rt.f32) => true
      case t => fail("The Type '"+t+"' is not the expected type.")
    }
    ex_h.t match {
      case rt.FunType(rt.f32, rt.f32) => true
      case t => fail("The Type '"+t+"' is not the expected type.")
    }

    ex_h match {
      case r.Lambda(r.Identifier("x"), r.App(ex_f,
      r.App(r.App(rp.add(_), r.Identifier("x")),r.Identifier("x") ))) => ex_f match {
        case r.Lambda(r.Identifier("x"), r.Identifier("x")) => true
        case sub => fail("subexpr '"+ sub+"' is wrong")
      }
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }

    val f1 = gen.CProgram(ex_h)
    println(f1)
  }

  test("parser should be able to parse 'secondFktCallsFirstFktAndItIsNotEvenDeclaredYet.rise'"){
    val fileName: String = testFilePath + "secondFktCallsFirstFktAndItIsNotEvenDeclaredYet.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))
    val functionName2: String = "h"
    val ex_h: r.Expr = riseExprByIdent.get(functionName2).getOrElse(fail("The function '" + functionName2 + "' does not exist!!!"))

    ex_f.t match {
      case rt.FunType(rt.f32, rt.f32) => true
      case t => fail("The Type '"+t+"' is not the expected type.")
    }
    ex_h.t match {
      case rt.FunType(rt.f32, rt.f32) => true
      case t => fail("The Type '"+t+"' is not the expected type.")
    }

    ex_h match {
      case r.Lambda(r.Identifier("x"), r.App(ex_f,
      r.App(r.App(rp.add(_), r.Identifier("x")),r.Identifier("x") ))) => ex_f match {
        case r.Lambda(r.Identifier("x"), r.Identifier("x")) => true
        case sub => fail("subexpr '"+ sub+"' is wrong")
      }
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }

    val f1 = gen.CProgram(ex_h)
    println(f1)
  }

  test("parser should be able to parse 'two_times_square.rise'"){
    val fileName: String = testFilePath + "two_times_square.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "two_times_square"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    val kernel = gen.OpenCLKernel(ex)
    println(kernel)
  }

  test("parser should be able to parse 'TupleType.rise'"){
    val fileName: String = testFilePath + "TupleType.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex.t match {
      case rt.FunType(rt.PairType(rt.i32,rt.f32), rt.i32) => true
      case t => fail("The Type '"+t+"' is not the expected type.")
    }

    ex match {
      case r.Lambda(r.Identifier("t"), r.App(rp.fst(_),r.Identifier("t"))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'TupleType2.rise'"){
    val fileName: String = testFilePath + "TupleType2.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex.t match {
      case rt.FunType(rt.PairType(rt.i32,rt.f32), rt.i32) => true
      case t => fail("The Type '"+t+"' is not the expected type.")
    }

    ex match {
      case r.Lambda(r.Identifier("t"), r.App(rp.fst(_),r.Identifier("t"))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'TupleType3.rise'"){
    val fileName: String = testFilePath + "TupleType3.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex.t match {
      case rt.FunType(rt.PairType(rt.i32,rt.ArrayType(n,rt.f32)), rt.i32) if n.eval.equals(2) => true
      case t => fail("The Type '"+t+"' is not the expected type.")
    }

    ex match {
      case r.Lambda(r.Identifier("t"), r.App(rp.fst(_),r.Identifier("t"))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should be able to parse 'TupleType4.rise'"){
    val fileName: String = testFilePath + "TupleType4.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex.t match {
      case rt.FunType(rt.PairType(rt.PairType(rt.i32,
      rt.ArrayType(n5,rt.ArrayType(n4, rt.ArrayType(n3, rt.ArrayType(n2, rt.i32))))),
      rt.ArrayType(n,rt.PairType(rt.i32, rt.i32))), rt.i32)
        if n5.eval.equals(5) && n4.eval.equals(4) && n3.eval.equals(3)&&n2.eval.equals(2) &&n.eval.equals(2)=> true
      case t => fail("The Type '"+t+"' is not the expected type.")
    }

    ex match {
      case r.Lambda(r.Identifier("t"), r.App(rp.fst(_), r.App(rp.fst(_),r.Identifier("t")))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  //Todo: not ignore; change it to an ErrorTest or change this test a little bit
  ignore("parser should be able to parse 'twoplus1extraDefintion.rise'"){
    val fileName: String = testFilePath + "twoplus1extraDefintion.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName_h: String = "h"
    val ex_h: r.Expr = riseExprByIdent.get(functionName_h).getOrElse(fail("The function '" + functionName_h + "' does not exist!!!"))
    val functionName_f: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName_f).getOrElse(fail("The function '" + functionName_f + "' does not exist!!!"))
    val functionName_z: String = "z"
    val ex_z = riseExprByIdent.get(functionName_z).getOrElse(fail("The function '" + functionName_z + "' does not exist!!!"))

    ex_h match {
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("fkt"), r.App(r.Identifier("fkt"), r.Identifier("x")))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }

    ex_f match {
      case r.Lambda(r.Identifier("y"), r.App(r.App(rp.add(_), r.Identifier("y")), r.Literal(rS.FloatData(5), _))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
//    ex_z match {
//      case rt.FunType(rt.i32, rt.i32) => true
//      case a => fail("not correct Type: " + a)
//    }

  }
  //Todo: not ignore; change it to an ErrorTest or change this test a little bit
  ignore("parser should not be able to parse 'twoplus1extraDefintionButSameNameInLocalVariable.rise'"){
    val fileName: String = testFilePath + "twoplus1extraDefintionButSameNameInLocalVariable.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)
    val functionName_h: String = "h"
    val ex_h: r.Expr = riseExprByIdent.get(functionName_h).getOrElse(fail("The function '" + functionName_h + "' does not exist!!!"))
    val functionName_f: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName_f).getOrElse(fail("The function '" + functionName_f + "' does not exist!!!"))
    val functionName_z: String = "z"
//    val ex_z: r.types.Type = riseExprByIdent.get(functionName_z).getOrElse(fail("The function '" + functionName_z + "' does not exist!!!")) match {
//      case HMExpr(lambda) => fail("it is no definition expected: " + lambda)
//      case HMType(t) => t
//      case HMNat(n) => fail("no nat is expected: "+n)
//    }

    ex_h match {
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("fkt"), r.App(r.Identifier("fkt"), r.Identifier("x")))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }

    ex_f match {
      case r.Lambda(r.Identifier("x"), r.App(r.App(rp.add(_), r.Identifier("x")), r.Literal(rS.FloatData(5), _))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
//    ex_z match {
//      case rt.FunType(rt.f32, rt.f32) => true
//      case a => fail("not correct Type: " + a)
//    }
  }

  test("parser should be able to parse 'twoSimpleFunctions.rise'"){
    val fileName: String = testFilePath + "twoSimpleFunctions.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName_h: String = "h"
    val ex_h: r.Expr = riseExprByIdent.get(functionName_h).getOrElse(fail("The function '" + functionName_h + "' does not exist!!!"))
    val functionName_f: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName_f).getOrElse(fail("The function '" + functionName_f + "' does not exist!!!"))
    val functionName_z: String = "z"
    if (riseExprByIdent.contains(functionName_z)) {
      fail("no Function with name '" + functionName_z + "' was declared")
    }

    ex_h match {
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("fkt"), r.App(r.Identifier("fkt"), r.Identifier("x")))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }

    ex_f match {
      case r.Lambda(r.Identifier("y"), r.App(r.App(rp.add(_), r.Identifier("y")), r.Literal(rS.FloatData(5), _))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  test("parser should not be able to parse 'twoSimpleFunctionsButWithSameLocalVarName.rise'"){
    val fileName: String = testFilePath + "twoSimpleFunctionsButWithSameLocalVarName.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)
    val functionName_h: String = "h"
    val ex_h: r.Expr = riseExprByIdent.get(functionName_h).getOrElse(fail("The function '" + functionName_h + "' does not exist!!!"))
    val functionName_f: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName_f).getOrElse(fail("The function '" + functionName_f + "' does not exist!!!"))

    ex_h match {
      case r.Lambda(r.Identifier("x"), r.Lambda(r.Identifier("fkt"), r.App(r.Identifier("fkt"), r.Identifier("x")))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }

    ex_f match {
      case r.Lambda(r.Identifier("x"), r.App(r.App(rp.add(_), r.Identifier("x")), r.Literal(rS.FloatData(5), _))) => true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }  }

  test("parser should be able to parse 'VecType1.rise'"){
    val fileName: String = testFilePath + "VecType1.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex.t match {
      case rt.VectorType(n :rt.Nat, rt.f32) if n.eval.equals(4) => true
      case t => fail("The Type '"+t+"' is not the expected type.")
    }

    ex match {
      case r.App(rp.vectorFromScalar(_), r.Literal(rS.FloatData(0), _)) => true
      case e => fail("not correct expression: " + e)
    }
  }



  test("parser should be able to parse 'VecType1WithLessParentheses.rise'"){
    val fileName: String = testFilePath + "VecType1WithLessParentheses.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex.t match {
      case rt.VectorType(n :rt.Nat, rt.f32) if n.eval.equals(4) => true
      case t => fail("The Type '"+t+"' is not the expected type.")
    }

    ex match {
      case r.App(rp.vectorFromScalar(_), r.Literal(rS.FloatData(0), _)) => true
      case e => fail("not correct expression: " + e)
    }
  }

  test("parser should be able to parse 'VecType2.rise'"){
    val fileName: String = testFilePath + "VecType2.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName: String = "f"
    val ex: r.Expr = riseExprByIdent.get(functionName).getOrElse(fail("The function '" + functionName + "' does not exist!!!"))

    ex.t match {
      case rt.PairType(rt.VectorType(n2:rt.Nat, rt.f32),rt.VectorType(n3:rt.Nat, rt.f32))
        if  n2.eval.equals(16) && n3.eval.equals(2)=> true
      case t => fail("The Type '"+t+"' is not the expected type.")
    }

    ex match {
      case r.App(r.App(rp.makePair(_), r.App(rp.vectorFromScalar(_), r.Literal(rS.FloatData(16), _))),
      r.App(rp.vectorFromScalar(_), r.Literal(rS.FloatData(0), _)))
      => true
      case e => fail("not correct expression: " + e)
    }
  }

  //Todo: not ignore; change it to an ErrorTest or change this test a little bit
  ignore("parser should be able to parse 'veryComplicated.rise'"){
    val fileName: String = testFilePath + "veryComplicated.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val riseExprByIdent = parse(lexer.tokens)

    val functionName_f: String = "f"
    val ex_f: r.Expr = riseExprByIdent.get(functionName_f).getOrElse(fail("The function '" + functionName_f + "' does not exist!!!"))
    val functionName_2: String = "specialFunctionOfChaos"
//    val type_2:r.types.Type= riseExprByIdent.get(functionName_2).getOrElse(fail("The function '" + functionName_f + "' does not exist!!!")) match {
//      case HMExpr(lambda) => fail("no definition is expected: "+ lambda)
//      case HMType(t) => t
//      case HMNat(n) => fail("no nat is expected: "+n)
//    }

//    type_2 match {
//      case rt.FunType(rt.bool, rt.FunType(rt.bool, rt.FunType(rt.i32,
//      rt.FunType(rt.f32, rt.f32)
//      ))) => true
//      case a => fail("it was a different definition expected, but we see: "+ a)
//    }

    ex_f match {
      case r.Lambda(r.Identifier("michael"), r.Lambda(r.Identifier("heinrich"),
      r.Lambda(r.Identifier("varX"), r.Lambda(r.Identifier("varY"),
      r.App(rp.not(_), r.App(r.App(rp.equal(_),r.App(
      r.App(rp.mod(_),

      r.App(r.App(rp.mul(_), r.Identifier("varX")),
      r.App(r.App(rp.mul(_), r.Identifier("varY"))
      ,

      r.App(r.App(rp.div(_), r.App(r.App(rp.sub(_),
      r.Literal(rS.FloatData(25), _)), r.Literal(rS.FloatData(a), _))
      ), r.Literal(rS.FloatData(b), _))

      ))),r.Literal(rS.FloatData(42), _))),
      r.Literal(rS.FloatData(0), _)))
      )))) if a == 10.5000 && b == 2.3000f=> true
      case r.Lambda(x, e) => fail("not correct Identifier or not correct expression: " + x + " , " + e)
      case a => fail("not a lambda: " + a)
    }
  }

  //-----------------------------------------------------------------------------------------------------
  //Error-Tests //Todo: " .. (.. (..).. " as an ErrorTest

  test("parser should be able to parse 'addMatrixWrongType.rise'"){
    val fileName: String = errorFilePath + "addMatrixWrongType.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val thrown = intercept[rt.InferenceException] {
      parse(lexer.tokens)
    }
    println(thrown.toString)
    //thrown.msg should equal(" fkt has no type")
  }
  /*
  ParserError: Arrow is only with one '-' not two '--' `-->`
 --> file:///home/visualjames/Documents/Universitaet/Bachelorarbeit/Repos/IntegrateRISE/shine/src/test/scala/parser/readFiles/filesToError/addMatrixTooLongArrow.rise:3:20
     |-------
3    |   mapGlobal 1 (\row-->mapLocal 0 (
     |                    ^^^ expected is an Arrow: '->'
     |-------
     | help: For comments you need a withespace after '--' and for Arrows you write only '->'
   */
  test("parser should be able to parse 'addMatrixTooLongArrow.rise'"){
    val fileName: String = errorFilePath/*"/home/visualjames/Test/"*/ + "addMatrixTooLongArrow.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val thrown = intercept[ParserException] {
      parse(lexer.tokens)
    }
    println(thrown.toString)
  }

  test("parser should be able to parse 'fx.rise'"){
    val fileName: String = errorFilePath + "fx.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val thrown = intercept[rt.InferenceException] {
      parse(lexer.tokens)
    }
    thrown.msg should equal(" fkt has no type")
  }

  /*
  ParserError: No Expression inside of the braces `( )`
 --> file:///home/visualjames/Documents/Universitaet/Bachelorarbeit/Repos/IntegrateRISE/shine/src/test/scala/parser/readFiles/filesToError/noExpressionInBraces.rise:2:7
     |-------
2    | f=\x->( )
     |       ^^^ no expression here
     |-------
     | help: braces need logic inside
   */
  test("parser should not be able to parse 'noExpressionInBraces.rise'"){
    val fileName: String = errorFilePath + "noExpressionInBraces.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val thrown = intercept[ParserException] {
      parse(lexer.tokens)
    }
    println(thrown.toString)
  }

  test("parser should not be able to parse 'two_times_square_test.rise'"){
    val fileName: String = errorFilePath + "two_times_square_test.rise"
    val file: FileReader = new FileReader(fileName)
    val lexer: RecognizeLexeme = new RecognizeLexeme(file)
    val thrown = intercept[ParserException] {
      parse(lexer.tokens)
    }
    println(thrown.toString)
  }

}