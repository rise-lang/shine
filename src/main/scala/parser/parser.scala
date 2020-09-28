package parser

import rise.core.{Lambda, semantics => rS, types => rt}
import rise.{core => r}
import util.Execute.Exception

object parse {

  //Todo: throwException in ParseError is not used and RuntimeException is not so good as normal Exception,
  // because no checking is needed:
  // https://stackoverflow.com/questions/19857008/extending-exception-runtimeexception-in-java
  abstract sealed class ParseErrorOrState()

  abstract sealed class ParseEnd() extends ParseErrorOrState()
  //Todo: I only want for ParseError the Throwable keyword, but it is the only thing, which should be throwable
  final case class ParseError(mes: String) extends ParseErrorOrState()
//  final case class ParseError(token: Token) extends ParseErrorOrState() {
//    val span = token.s
//    val fileReader = token.s.file
//
//    private def underline(str: String): String = {
//      var s: String = ""
//      for (a <- str) {
//        s = s + a + "\u0332"
//      }
//      s
//    }
//
//    private def importantPart(str: String): String = {
//      val s: String = underline(str)
//      s
//    }
//
//    def throwException(): Unit = {
//      val currentColumn: String = fileReader.sourceLines(span.begin.column)
//      val important: String = importantPart(currentColumn.substring(span.begin.row, span.end.row))
//      val codeLine: String = if (span.begin.row - 1 > 0) {
//        currentColumn.substring(0, span.begin.row) + important + currentColumn.substring(span.end.row, currentColumn.length)
//      } else {
//        important + currentColumn.substring(span.end.row, currentColumn.length)
//      }
//      val message: String = "ErrorToken: " + this.toString + " at " + span.toString + "\n" + codeLine
//      throw new Exception(message)
//    }

    ////    override def throw():Unit = throwException()
    //  }
    //  final case class HereIsATypeIdentifierExpected(token:Token) extends ParseError(token)//{
    ////    override def toString = "A Type is expected but " + token + " is not a Type"
    ////  }
    //  final case class TokenListIsEmpty() extends ParseEnd() //Todo: Better name for the classes which are clearly only for States in the parsing process
    //  final case class BacklashWasExpected(token:Token) extends ParseEnd()
    //  final case class IdentifierWasExpected(token:Token) extends ParseEnd()
    //  final case class NotAnAcceptedType(typeK:TypeKind) extends ParseEnd()
    //  final case class TypeAnnotationNotCorrect(token:Token) extends ParseError(token)
    //  final case class HereIsATypeAnnotationExpected(token:Token) extends ParseError(token)
    //  final case class ArrowWasExpected(token:Token) extends ParseEnd()
    //  final case class AnExpressionAndNotATypeWasExpected(typ:rt.Type) extends ParseEnd()
    //  final case class UnaryOperatorWasExpected(token:Token) extends ParseEnd()
    //  final case class notAnAcceptedIntegerOrFloatType(token:Token) extends ParseEnd()
    //  final case class BinaryOperatorWasExpected(token:Token) extends ParseEnd()
    //  final case class LBraceWasExpected(token:Token) extends ParseEnd()
    //  final case class RBraceWasExpected(token:Token) extends ParseEnd()
    //  final case class NoBinaryOperator() extends ParseEnd()
    //  final case class NoParseAppLeft() extends ParseEnd()

    /*
   * Precondition: Only valid Tokens in tokenList.
   */
    def apply(tokenList: List[Token]): r.Expr = {
      val parseState: ParseState = (tokenList, Nil, countHowManyParseApps(tokenList))
      val shineLambda: r.Expr = parseTopLevelLambda(parseState) match {
        case Right(ex) => ex
        case Left(errorOrState) => {
          println(errorOrState)
          throw new RuntimeException("failed parsing : " + errorOrState)
        }
      }
      println("parse: " + shineLambda)
      shineLambda
      //r.Identifier("placeholder")()
    }
  //
  //    def checkBracesNumber(tokenList: List[Token]): Unit = {
  //      val list: List[Either[PreAndErrorToken, Token]] = tokens
  //      val arr: Array[String] = fileReader.sourceLines
  //      val lB: Int = list.count(a => a match {
  //        case Right(LBrace(_)) => true
  //        case b => false
  //      })
  //      val rB: Int = list.count(a => a match {
  //        case Right(RBrace(_)) => true
  //        case b => false
  //      })
  //
  //      if (lB > rB) {
  //        //for example "( .. ( ... )"
  //        val loc: Location = Location(arr.length - 1, arr(arr.length - 1).length)
  //        val ex = RightBraceMissing(new Span(fileReader, loc), fileReader)
  //        ex.throwException()
  //      } else if (lB < rB) { //for example "( .. ) ... )"
  //        val loc: Location = Location(arr.length - 1, arr(arr.length - 1).length)
  //        val ex = LeftBraceMissing(new Span(fileReader, loc), fileReader)
  //        ex.throwException()
  //      }
  //    }

  sealed trait SyntaxElement
  final case class SExpr(expr: r.Expr) extends SyntaxElement
  final case class SType(t: r.types.Type) extends SyntaxElement

  //type ParseState = (List[Token], List[SyntaxElement])
  type ParseState = (List[Token], List[SyntaxElement], Int) //Int how many times can we still use parseApp?

  implicit class ParseStatePipe(val ps: Either[ParseErrorOrState,ParseState]) extends AnyVal {
    def |>(f: ParseState => Either[   ParseErrorOrState,ParseState]): Either[   ParseErrorOrState,ParseState] = {
      println("|> : " + ps)
      ps match {
        case Right(p) => f(p)
        case Left(e) => Left(e)
      }
    }
  }

  implicit class ParseStateElse(val leftF: ParseState => Either[ParseErrorOrState, ParseState]) extends AnyVal {
    def ||(
      rightF: ParseState => Either[ParseErrorOrState, ParseState]
   ): ParseState => Either[ParseErrorOrState, ParseState] = {
      ps =>
        leftF(ps) match {
          case Left(_) => rightF(ps)
          case Right(resPs) => Right(resPs)
        }
    }
  }




  //_________________________________________________________Lambda


  def parseBackslash(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    val (tokens, parsedExprs, c) = parseState
    if(tokens.isEmpty){
      return Left(ParseError("failed to parse Backlash: " + " List is empty"))
    }
    val nextToken :: restTokens = tokens

    nextToken match {
      case Backslash(_) =>
      case tok => {
        println("failed parseBacklash: "+ parseState)
        return Left(ParseError("failed to parse Backslash: " + tok + " is not an Backslash"))
      }
    }

    Right((restTokens, parsedExprs, c))
  }

  def parseIdent(parseState: ParseState): Either[ParseErrorOrState, ParseState] = {
    println("parseIdent: " + parseState)
    val (tokens, parsedSynElems, c) = parseState
    if(tokens.isEmpty){
      return Left(ParseError("failed to parse Ident: " + " List is empty"))
    }
    val nextToken :: restTokens = tokens

    nextToken match {
      case Identifier(name, _) =>
        Right((restTokens, SExpr(r.Identifier(name)()) :: parsedSynElems, c))
      case tok => {
        println("Abbruch parseIdent: " + tok +" : " + parseState)
        Left(ParseError("failed to parse Ident: " + tok + " is not an Identifier"))
      }
    }
  }

  def parseMaybeTypeAnnotation(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    val (tokens, parsedSynElems, c) = parseState
    val colonToken :: typeToken :: restTokens = tokens

    colonToken match {
      case Colon(_) => {
        //if a type Annotation exist, we set the type new of the Identifier
        typeToken match {
          case Type(typ, _) => {
            val t: Either[   ParseErrorOrState,rt.Type] = typ match {
              case ShortTyp() => Right(rt.i8)
              case IntTyp() => Right(rt.i32)
              case FloatTyp() => Right(rt.f32)
              case DoubleType() => Right(rt.f64)
              case BoolType() => Right(rt.bool)
              case notAtype => Left(ParseError("failed to parse Type: Not accepted Type" + notAtype))
            }
            t match {
              case Right(nowT) => Right((restTokens, SType(nowT) :: parsedSynElems, c))
              case Left(e) => Left(ParseError("failed to parse Type"))
            }
          }
          case notAtype => Left(ParseError("failed to parse Type: " + notAtype +" is not an Type"))
        }
      }
      case _ => Right(parseState)
    }
  }

  def parseTypeAnnotation(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    val (tokens, parsedSynElems, c) = parseState
    val colonToken :: typeToken :: restTokens = tokens

    colonToken match {
      case Colon(_) => {
        //if a type Annotation exist, we set the type new of the Identifier
        typeToken match {
          case Type(typ, _) => { //Todo: Here and in parseMaybeTypeAnnotation is this same match!
            val t: Either[   ParseErrorOrState, rt.Type] = typ match {
              case ShortTyp() => Right(rt.i8)
              case IntTyp() => Right(rt.i32)
              case FloatTyp() => Right(rt.f32)
              case DoubleType() => Right(rt.f64)
              case BoolType() => Right(rt.bool)
              case notAtype => Left(ParseError("failed to parse Type: Not accepted Type" + notAtype))
            }
            t match {
              case Right(nowT) => Right((restTokens, SType(nowT) :: parsedSynElems, c))
              case Left(e) => Left(ParseError("failed to parse Type"))
            }
          }
          case notAtype => Left(ParseError("failed to parse Type: " + notAtype +" is not an Type"))
        }
      }
      case notAColon => Left(ParseError("failed to parse Type: A TypeAnnotation is expected, but " + notAColon + " is not an Colon"))
    }
  }

  def parseArrow(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    val (tokens, parsedExprs, c) = parseState
    val nextToken :: restTokens = tokens

    nextToken match {
      case Arrow(_) =>
      case tok => Left(ParseError("failed to parse Type: " + tok + " is not an Arrow"))
    }

    Right((restTokens, parsedExprs, c))
  }

  private def countHowManyParseApps(tokens: List[Token]): Int = {
    //val max = tokens.length-1
    val numberBinOp = tokens.count(p => p.isInstanceOf[BinOp])
    val numberUnOp = tokens.count(p => p.isInstanceOf[UnOp])
    val res = numberBinOp*2 + numberUnOp*1
    res
  }

  /*
  top level Lambda expects that the type of the Identifier is defined!

  only use as top level!
   */
  def parseTopLevelLambda(parseState: ParseState): Either[ParseErrorOrState, r.Expr] = {
    require(parseState._2.isEmpty, "parseState is not empty, but nothing should be in it yet")
    val psLambda =
      Right(parseState)      |>
        parseBackslash      |>
        parseIdent          |>
        parseTypeAnnotation |>
        parseArrow |>
        parseExpression

      psLambda match {
      case Left(e) => Left(e)
      case Right(ps) => {
        // TODO if tokens left in ps._1, return error
        require(ps._1.isEmpty, "Everything should be computed!")
        //    require(ps._2.length == 3, "it should now have exactly 3 Exprs: Identifier, Type of the Identifier and the expression")

        val synElemList = ps._2
        val (expr, synElemListExpr) = (synElemList.head match {
          case SExpr(e) => e
          case a => throw new RuntimeException("Here is an Expression expected, but " + a +" ist not an Expression!")
        }, synElemList.tail)

        val (typedIdent, synElemListTIdent) =
          synElemListExpr.head match {
            case SType(t) =>
              synElemListExpr.tail.head match {
                case SExpr(i) => (i.setType(t), synElemListExpr.tail.tail)
                case a => throw new RuntimeException("Here is an Expression expected, but " + a +" ist not an Expression!")
              }
            case SExpr(i) => {
              println(i)
              throw new RuntimeException("We are in TopLevelLambda and there is an Type for the declared Identifier needed; " + i +" is not an r.Type!")
            }
          }

        require(synElemListTIdent.isEmpty, "the List has to be empty!")

        val lambda = Lambda(typedIdent.asInstanceOf[r.Identifier], expr)()
        println("lambda durchgelaufen: " + lambda)
        Right(lambda)
      }
    }

  }
  /*
  is the whole Syntax-Tree.
  the syntax-Tree has on top an Lambda-Expression
   */
  def parseLambda(parseState: ParseState): Either[ParseErrorOrState,ParseState] = {
    if(parseState._1.isEmpty){
      println("Abbruch; parseLambda: "+ parseState)
      return Right(parseState)
    }
    println("parseLambda: " +parseState)
    val psOld =
      Right(parseState) |>
        parseBackslash |>
        parseIdent |>
        parseMaybeTypeAnnotation |>
        parseArrow

        val ps = psOld match {
          case Right(p) => parseExpression(p)
          case Left(e) => {
            println("endLambda: "+ e)
            return Left(e)
          }
        }

    val (toks, synElemList, c) = ps match {
      case Right(a) => (a._1,a._2, a._3)
      case Left(e) => return Left(e)
    }
    val (expr, synElemListExpr) = (synElemList.head match {
      case SExpr(e) => e
      case a => throw new RuntimeException("Here is an Expression expected, but " + a +" ist not an Expression!")
    }, synElemList.tail)

    val (maybeTypedIdent, synElemListMaybeTIdent) =
      synElemListExpr.head match {
        case SType(t) =>
          synElemListExpr.tail.head match {
            case SExpr(i) => (i.setType(t), synElemListExpr.tail.tail)
            case a => throw new RuntimeException("Here is an Expression expected, but " + a +" ist not an Expression!")
          }
        case SExpr(i) => (i, synElemListExpr.tail)
      }

    val lambda = Lambda(maybeTypedIdent.asInstanceOf[r.Identifier], expr)()
    println("synElemListMaybeTIdent: " + synElemListMaybeTIdent +" ______ " + synElemListExpr)
    Right((toks, SExpr(lambda) :: synElemListMaybeTIdent, c))
  }

  //_________________________________________________________Lambda
  //_________________________________________________________Expres

  def parseExpressionSimplified(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    if(parseState._1.isEmpty){
      println("Abbruch; parseExpression: "+ parseState)
      return Right(parseState)
    }
    println("parseExpression: " + parseState)
    //FIXME parseState always true
    Right(parseState) |> (parseIdent _ || parseNumber ||
      parseUnOperatorApp || parseBracesExpr)
  }

  def parseExpression(parseState: ParseState): Either[ParseErrorOrState,ParseState] = {
    if(parseState._1.isEmpty){
      println("Abbruch; parseExpression: "+ parseState)
      return Right(parseState)
    }
    println("parseExpression: " + parseState)
    //FIXME parseState always true
      Right(parseState) |>
        (parseLambda _ || parseApp ||
          parseIdent || parseUnOperator || parseBinOperator || parseBracesExpr  ||
        parseNumber)

  }

  def parseBracesExpr(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    println("parseBracesExpr: "+ parseState)
    if(parseState._1.isEmpty){
      println("Abbruch; parseBracesExpr: "+ parseState)
      return Right(parseState)
    }
    val p =
      Right(parseState)  |>
        parseLeftBrace  |>
        parseExpression |>
        parseRightBrace

    //      if ( parsedExprs is with type) { } else {}
    p match {
      case Right(pState) => Right(pState)
      case Left(e) => Left(e)
    }
  }

  //  1 + 2 * 3 = mul (add 1 2) 3 = App(App(mul, App(App(add, 1), 2)), 3)
  def parseBinOperatorApp(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    println("parseBinOperatorApp: "+ parseState)
    if(parseState._1.isEmpty || !parseState._1.exists(t => t.isInstanceOf[BinOp])){
      println("Abbruch; parseBinOperatorApp: "+ parseState)
      return Left(ParseError("failed to parse BinOperatorApp"))
    }
    val p =
      Right(parseState)  |>
        parseExpressionSimplified |>
        parseBinOperator |>
        parseExpression

    val (tokens, parsedExprs, c):(List[Token], List[SyntaxElement], Int) = p match {
      case Right(parseState) => parseState
      case Left(e) => return Left(e) //Todo: how can I do that whitout this ugly return?
    }
    val expr2 :: binOp  :: expr1 :: restExpr= parsedExprs

    expr2 match {
      case SExpr(e2) => expr1 match {
        case SExpr(e1) =>  binOp match { //TODO: now it only calculates from right to left, but normaly is * before +
          case SExpr(op) => Right((tokens, SExpr(r.App(r.App(op, e2)(), e1)())  :: restExpr, c))
          case SType(t) => Left(ParseError("failed to parse BinOperatorApp: " + t +" is not the expected Type"))
        }
        case SType(t) => Left(ParseError("failed to parse BinOperatorApp: " + t +" is not the expected Type"))
      }
      case SType(t) => Left(ParseError("failed to parse BinOperatorApp: " + t +" is not the expected Type"))
    }
  }

  def parseApp(parseState: ParseState): Either[ParseErrorOrState, ParseState] = {
    println("parseApp: " + parseState)
    if(parseState._3==0){
      println("Abbruch; parseApp: "+ parseState)
      return Left(ParseError("failed to parse parseApp: " + " no parseApp is left"))
    }
    val pas = (parseState._1, parseState._2, parseState._3-1)
    val p =
      Right(pas)  |>
        parseExpression |>
        parseExpression

    val (tokens, parsedExprs, c): ParseState = p match {
      case Right(parseState) => parseState
      case Left(e) => return Left(e)
    }
    val expr2 :: expr1 :: restExpr= parsedExprs

    expr2 match {
      case SExpr(e2) => expr1 match {
        case SExpr(e1) =>  Right((tokens, SExpr(r.App(e1, e2)())  :: restExpr, c))
        case SType(t) => Left(ParseError("failed to parse parseApp: " + t + " is an Type but an Expression is expected"))
      }
      case SType(t) => Left(ParseError("failed to parse parseApp: " + t + " is an Type but an Expression is expected"))
    }
  }

  def parseUnOperatorApp(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    println("parseUnOperatorApp: "+ parseState)
    if(parseState._1.isEmpty){
      println("Abbruch; parseUnOperatorApp: "+ parseState)
      return Right(parseState)
    }
    val p = {
    Right(parseState)    |>
      parseUnOperator   |>
      parseExpression
    }
    println("parseUnOperatorApp durchlaufen: " + p)
    val (tokens, parsedExprs, c): ParseState = p match {
      case Right(parseState) => parseState
      case Left(e) => return Left(e)
    }
    val expr :: unop :: restExpr= parsedExprs

    unop match {
      case SExpr(op) => {
        op match {
          case r.primitives.Neg() =>
          case r.primitives.Not() =>
          case r.primitives.Sub() => throw Exception("Not and Sub got switched!")
          case a => {
            println("the Primitive '"+ a + "' is not expected!")
            throw Exception(a + " this should not be happening, because parseUnOperator has to be parsed an operator!")
          }
        }
        expr match {
          case SExpr(e) =>  Right((tokens, SExpr(r.App(op, e)())  :: restExpr, c))
          case SType(t) => Left(ParseError("failed to parse parseUnOperatorApp: " + t + " is an Type but an Expression is expected"))
        }
      }
      case SType(t) => Left(ParseError("failed to parse parseUnOperatorApp: " + t + " is an Type but an Expression is expected"))
    }
  }

  def parseUnOperator(parseState: ParseState): Either[ParseErrorOrState, ParseState] = {
    val nextToken :: restTokens = parseState._1

    val p = nextToken match {
      case UnOp(un, _) => un match {
        case OpType.UnaryOpType.NEG => Right((restTokens, SExpr(r.primitives.Neg()()) :: parseState._2, parseState._3))
        case OpType.UnaryOpType.NOT => Right((restTokens, SExpr(r.primitives.Not()()) :: parseState._2, parseState._3))
      }
      case tok => {
        println("UnaryOperatorWasExpected: "+ tok + ": " + restTokens)
        Left(ParseError("failed to parse parseUnOperator: " + tok + " is not an UnaryOperator"))
      }
    }
    p
  }

  def parseNumber(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    println("ParseNumber: " + parseState)
    val (tokens, parsedSynElems, c) = parseState
    if(tokens.isEmpty){
      return Left(ParseError("failed to parse Number: " + " List is empty"))
    }
    val nextToken :: restTokens = tokens

    nextToken match {
      case I8(number, _) =>
        Right((restTokens, SExpr(r.Literal(rS.IntData(number))) :: parsedSynElems, c))
      case I32(number, _) =>
        Right((restTokens, SExpr(r.Literal(rS.IntData(number))) :: parsedSynElems, c))
      case F32(number, _) =>
        Right((restTokens, SExpr(r.Literal(rS.FloatData(number))) :: parsedSynElems, c))
      case F64(number, _) =>
        Right((restTokens, SExpr(r.Literal(rS.DoubleData(number))) :: parsedSynElems, c))
      case tok => Left(ParseError("failed to parse Number: " + tok + " is not an accepted Integer of Float"))
    }
  }


  def parseBinOperator(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    println("parseBinOperator: "+ parseState)
    val (tokens, parsedSynElems, c) = parseState
    val nextToken :: restTokens = tokens

    nextToken match {
      case BinOp(op, _) => op match {
        case OpType.BinOpType.ADD =>
          Right((restTokens, SExpr(r.primitives.Add()()) :: parsedSynElems, c))
        case OpType.BinOpType.DIV =>
          Right((restTokens, SExpr(r.primitives.Div()()) :: parsedSynElems, c))
        case OpType.BinOpType.EQ =>
          Right((restTokens, SExpr(r.primitives.Equal()()) :: parsedSynElems, c))
        case OpType.BinOpType.GT =>
          Right((restTokens, SExpr(r.primitives.Gt()()) :: parsedSynElems, c))
        case OpType.BinOpType.LT =>
          Right((restTokens, SExpr(r.primitives.Lt()()) :: parsedSynElems, c))
        case OpType.BinOpType.MOD => Right((restTokens, SExpr(r.primitives.Mod()()) :: parsedSynElems, c))
        case OpType.BinOpType.MUL => Right((restTokens, SExpr(r.primitives.Mul()()) :: parsedSynElems, c))
        case OpType.BinOpType.SUB => Right((restTokens, SExpr(r.primitives.Sub()()) :: parsedSynElems, c))
        case tok => {
          println("Das hier kann nicht sein, weil alle Operatoren müsste ich abgedeckt haben. BinOp: '" + tok + "' is no BinOperator!")
          Left(ParseError("failed to parse BinOperator: " + tok + " is not an accepted BinOp"))
        }
      }
      case tok => {
        println("BinOp: '" + tok + "' is no BinOperator!")
        Left(ParseError("failed to parse BinOperator: " + tok + " is not an BinOp"))
      }
    }
  }

  def parseLeftBrace(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    val (tokens, parsedSynElems, c) = parseState
    val nextToken :: restTokens = tokens

    nextToken match {
      case LBrace(_) => Right((restTokens, parsedSynElems, c))
      case tok => Left(ParseError("failed to parse LeftBrace: " + tok + " is not an LeftBrace"))
    }
  }

  def parseRightBrace(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    val (tokens, parsedSynElems, c) = parseState
    val nextToken :: restTokens = tokens

    nextToken match {
      case RBrace(_) => Right((restTokens, parsedSynElems, c))
      case tok => Left(ParseError("failed to parse RightBrace: " + tok + " is not an RightBrace"))
    }
  }


  //_________________________________________________________Expres
}