package parser

import rise.core.{Lambda, semantics => rS, types => rt}
import rise.{core => r}
object parser {

  abstract sealed class ParseErrorOrState()

  abstract sealed class ParseEnd() extends ParseErrorOrState()

  final case class ParseError(mes: String) extends ParseErrorOrState()

  /*
   * Precondition: Only valid Tokens in tokenList.
   */
  def apply(tokenList: List[Token]): r.Expr = {
    val parseState: ParseState = ParseState(tokenList, Nil)
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

  final case class ParseState(tokenStream: List[Token], parsedSynElems: List[SyntaxElement])

  implicit class ParseStatePipe(val ps: Either[ParseErrorOrState, ParseState]) extends AnyVal {
    def |>(f: ParseState => Either[ParseErrorOrState, ParseState]): Either[ParseErrorOrState, ParseState] = {
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


  def parseBackslash(parseState: ParseState): Either[ParseErrorOrState, ParseState] = {
    val ParseState(tokens, parsedExprs) = parseState
    if (tokens.isEmpty) {
      return Left(ParseError("failed to parse Backlash: " + " List is empty"))
    }
    val nextToken :: restTokens = tokens

    nextToken match {
      case Backslash(_) =>
      case tok => {
        println("failed parseBacklash: " + parseState)
        return Left(ParseError("failed to parse Backslash: " + tok + " is not an Backslash"))
      }
    }

    Right(ParseState(restTokens, parsedExprs))
  }

  def parseIdent(parseState: ParseState): Either[ParseErrorOrState, ParseState] = {
    println("parseIdent: " + parseState)
    val ParseState(tokens, parsedSynElems) = parseState
    if (tokens.isEmpty) {
      return Left(ParseError("failed to parse Ident: " + " List is empty"))
    }
    val nextToken :: restTokens = tokens

    nextToken match {
      case Identifier(name, _) =>
        Right(ParseState(restTokens, SExpr(r.Identifier(name)()) :: parsedSynElems))
      case tok => {
        println("Abbruch parseIdent: " + tok + " : " + parseState)
        Left(ParseError("failed to parse Ident: " + tok + " is not an Identifier"))
      }
    }
  }

  private def getType(typ: TypeKind, restTokens: List[Token], parsedSynElems: List[SyntaxElement]): Either[ParseErrorOrState, ParseState] = {
    val t = typ match {
      case ShortTyp() => Right(rt.i8)
      case IntTyp() => Right(rt.i32)
      case FloatTyp() => Right(rt.f32)
      case DoubleType() => Right(rt.f64)
      case BoolType() => Right(rt.bool)
      case notAtype => Left(ParseError("failed to parse Type: Not accepted Type" + notAtype))
    }
    t match {
      case Right(nowT) => Right(ParseState(restTokens, SType(nowT) :: parsedSynElems))
      case Left(e) => Left(ParseError("failed to parse Type"))
    }
  }


  def parseMaybeTypeAnnotation(parseState: ParseState): Either[ParseErrorOrState, ParseState] = {
    val ParseState(tokens, parsedSynElems) = parseState
    val colonToken :: typeToken :: restTokens = tokens

    colonToken match {
      case Colon(_) => {
        //if a type Annotation exist, we set the type new of the Identifier
        typeToken match {
          case Type(typ, _) => getType(typ, restTokens, parsedSynElems)
          case notAtype => Left(ParseError("failed to parse Type: " + notAtype + " is not an Type"))
        }
      }
      case _ => Right(parseState)
    }
  }

  def parseTypeAnnotation(parseState: ParseState): Either[ParseErrorOrState,ParseState] = {
    val ParseState(tokens, parsedSynElems) = parseState
    val colonToken :: typeToken :: restTokens = tokens

    colonToken match {
      case Colon(_) => {
        //if a type Annotation exist, we set the type new of the Identifier
        typeToken match {
          case Type(typ, _) => getType(typ, restTokens, parsedSynElems)
          case notAtype => Left(ParseError("failed to parse Type: " + notAtype + " is not an Type"))
        }
      }
        case notAColon => Left(ParseError("failed to parse Type: A TypeAnnotation is expected, but " + notAColon + " is not an Colon"))
      }
    }

  def parseArrow(parseState: ParseState): Either[ParseErrorOrState,ParseState] = {
    val ParseState(tokens, parsedExprs) = parseState
    val nextToken :: restTokens = tokens

    nextToken match {
      case Arrow(_) =>
      case tok => Left(ParseError("failed to parse Type: " + tok + " is not an Arrow"))
    }

    Right(ParseState(restTokens, parsedExprs))
  }

  private def combineExpressionsUntilOnly2WithTypeor1ExpressionsAreLeft(synElemList: List[SyntaxElement]) : (r.Expr, List[SyntaxElement]) = {
    println("combineExpressionsUntilOnly2WithTypeor1ExpressionsAreLeft: "+ synElemList)
    var synE = synElemList.reverse.tail.head match {
      case SType(_) => synElemList.reverse.tail.tail
      case SExpr(_) => synElemList.reverse.tail
    }
    var e:r.Expr = synE.head match {
      case SExpr(expr) => {
        synE = synE.tail
        expr
      }
      case SType(t) => throw new RuntimeException("List should't have Types at this beginning position! " + t)
    }
    println("I will combine Expressions in Lambda: "+ synE + " <::> " + e)
    while(!synE.isEmpty){
      synE.head match {
        case SExpr(expr1) => {
          e = r.App(e, expr1)()
          synE = synE.tail
        }
        case SType(t) => throw new  RuntimeException("List should't have Types at this position! " + t)
      }
    }
    val l= synElemList.reverse.tail.head match {
      case SType(_) =>  synElemList.reverse.tail.head :: synElemList.reverse.head :: Nil
      case SExpr(_) => synElemList.reverse.head :: Nil
    }
    val res = (e,l)
    println("I have combined the Expressions in Lambda: "+ res)
    res
  }

  private def combineExpressions(synElemList: List[SyntaxElement]) : r.Expr = {
    if(synElemList.isEmpty){
      throw new IllegalArgumentException("the ElemList is empty!")
    }
    var synE = synElemList.reverse
    var e:r.Expr = synE.head match {
      case SExpr(expr) => {
        synE = synE.tail
        expr
      }
      case SType(t) => throw new RuntimeException("List should't have Types at this beginning position! " + t)
    }
    println("I will combine Expressions in Lambda: "+ synE + " <::> " + e)
    while(!synE.isEmpty){
      synE.head match {
        case SExpr(expr1) => {
          e = r.App(e, expr1)()
          synE = synE.tail
        }
        case SType(t) => throw new  RuntimeException("List should't have Types at this position! " + t)
      }
    }
    println("I have combined the Expressions in Lambda: "+ e)
    e
  }

  /*
  top level Lambda expects that the type of the Identifier is defined!

  only use as top level!
   */
  def parseTopLevelLambda(parseState: ParseState): Either[ParseErrorOrState, r.Expr] = {
    require(parseState.parsedSynElems.isEmpty, "parseState is not empty, but nothing should be in it yet")
    val psLambda =
      Right(parseState)      |>
        parseBackslash      |>
        parseIdent          |>
        parseTypeAnnotation |>
        parseArrow |>
        parseMaybeAppExpr

      psLambda match {
      case Left(e) => Left(e)
      case Right(ps) => {
        if(!ps.tokenStream.isEmpty){
          throw new RuntimeException("Every Token should be computed! " + ps)
        }
        val synElemList = ps.parsedSynElems
        val (expr, synElemListExpr) = combineExpressionsUntilOnly2WithTypeor1ExpressionsAreLeft(synElemList)

        if(!(synElemListExpr.length == 2)){
          throw new RuntimeException("it should now have exactly 2 Exprs: Identifier, " +
            "Type of the Identifier: " + synElemListExpr)
        }
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
    if(parseState.tokenStream.isEmpty){
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

        val psOrErr = psOld match {
          case Right(p) => parseMaybeAppExpr(ParseState(p.tokenStream,Nil))
          case Left(e) => {
            println("endLambda: "+ e)
            return Left(e)
          }
        }

    val (toks, synElemList) = psOrErr match {
      case Right(psNew) => {
        val expr = SExpr(combineExpressions(psNew.parsedSynElems))
        val newL = expr :: Nil
        val li:List[SyntaxElement] = psOld match {
          case Right(pa) => pa.parsedSynElems.reverse ++ newL
          case Left(_) => throw new RuntimeException("this should not be able to happen in parseLambdda, because I already have controlled this!")
        }
        val l = li.reverse
        (psNew.tokenStream,l)
      }
      case Left(e) => return Left(e)
    }

    val (expr, synElemListExpr) = (synElemList.head match {
      case SExpr(e) => e
      case a => throw new RuntimeException("Here is an Expression expected, but " + a +" ist not an Expression!")
    }, synElemList.tail)
    println("now in Lambda we want to combine our results: "+ expr +" # " + synElemListExpr)

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
    Right(ParseState(toks, SExpr(lambda) :: synElemListMaybeTIdent))
  }

  //_________________________________________________________Lambda
  //_________________________________________________________Expres

  def parseNoAppExpr(parseState: ParseState): Either[ParseErrorOrState,ParseState] = {
//    if(parseState.tokenStream.isEmpty){
//      println("Abbruch; parseExpression: "+ parseState)
//      return Right(parseState)
//    }
    println("parseLowExpression: " + parseState)
    //FIXME parseState always true
    Right(parseState) |>
      (parseLambda _ || parseBracesExpr ||
        parseUnOperator || parseBinOperator || parseIdent ||
        parseNumber)

  }

  def parseBracesExpr(parseState: ParseState): Either[ParseErrorOrState,ParseState] = {
    println("parseBracesExpr: "+ parseState)
    if(parseState.tokenStream.isEmpty){
      println("Abbruch; parseBracesExpr: "+ parseState)
      return Right(parseState)
    }
    val p =
      Right(ParseState(parseState.tokenStream,Nil))  |>
        parseLeftBrace  |>
        parseMaybeAppExpr |>
        parseRightBrace

    p match {
      case Right(pState) => {
        if(pState.parsedSynElems.isEmpty){
          val rBraceIndex = parseState.tokenStream.indexWhere(p=> p.isInstanceOf[RBrace])
          throw new RuntimeException("There was no Expression in Braces at posstion (" + 0 + " , " + rBraceIndex +
            " : "+ parseState.tokenStream.toString())
        }
        val expr = SExpr(combineExpressions(pState.parsedSynElems))
        val newL = expr :: Nil
        val li:List[SyntaxElement] = parseState.parsedSynElems.reverse ++ newL
        val l = li.reverse
        val newParse:ParseState = ParseState(pState.tokenStream, l)
        Right(newParse)
      }
      case Left(e) => Left(e)
    }
  }

  def parseMaybeAppExpr(parseState: ParseState): Either[ParseErrorOrState, ParseState] = {
    println("parseMaybeAppExpr: " + parseState)
    if(parseState.tokenStream.head.isInstanceOf[RBrace]){
      println("L" +
        "RBrace is at the beginning of parseApp: " + parseState)
      return Right(parseState)
    }
    val parseStateOrError =
      Right(parseState)  |>
        parseNoAppExpr
    println("parseApp after parseLowExpression: "+ parseStateOrError)
    parseStateOrError match {
      case Left(e) => Left(e)
      case Right(ps)=> if(ps.tokenStream.isEmpty){
                              println("parseApp End, because TokenList is empty: "+ ps)
                              Right(ps)
                            }else{
                              val p = parseMaybeAppExpr(ps)
                                  p
                            }
    }
  }


  def parseUnOperator(parseState: ParseState): Either[ParseErrorOrState, ParseState] = {
    val nextToken :: restTokens = parseState.tokenStream

    val p = nextToken match {
      case UnOp(un, _) => un match {
        case OpType.UnaryOpType.NEG => Right(ParseState(restTokens, SExpr(r.primitives.Neg()()) :: parseState.parsedSynElems))
        case OpType.UnaryOpType.NOT => Right(ParseState(restTokens, SExpr(r.primitives.Not()()) :: parseState.parsedSynElems))
      }
      case tok => {
        println("UnaryOperatorWasExpected: "+ tok + ": " + restTokens)
        Left(ParseError("failed to parse parseUnOperator: " + tok + " is not an UnaryOperator"))
      }
    }
    p
  }

  def parseNumber(parseState: ParseState): Either[ParseErrorOrState,ParseState] = {
    println("ParseNumber: " + parseState)
    val ParseState(tokens, parsedSynElems) = parseState
    if(tokens.isEmpty){
      return Left(ParseError("failed to parse Number: " + " List is empty"))
    }
    val nextToken :: restTokens = tokens

    nextToken match {
      case I8(number, _) =>
        Right(ParseState(restTokens, SExpr(r.Literal(rS.IntData(number))) :: parsedSynElems))
      case I32(number, _) =>
        Right(ParseState(restTokens, SExpr(r.Literal(rS.IntData(number))) :: parsedSynElems))
      case F32(number, _) =>
        Right(ParseState(restTokens, SExpr(r.Literal(rS.FloatData(number))) :: parsedSynElems))
      case F64(number, _) =>
        Right(ParseState(restTokens, SExpr(r.Literal(rS.DoubleData(number))) :: parsedSynElems))
      case tok => Left(ParseError("failed to parse Number: " + tok + " is not an accepted Integer of Float"))
    }
  }


  def parseBinOperator(parseState: ParseState): Either[ParseErrorOrState,ParseState] = {
    println("parseBinOperator: "+ parseState)
    val ParseState(tokens, parsedSynElems) = parseState
    val nextToken :: restTokens = tokens

    nextToken match {
      case BinOp(op, _) => op match {
        case OpType.BinOpType.ADD =>
          Right(ParseState(restTokens, SExpr(r.primitives.Add()()) :: parsedSynElems))
        case OpType.BinOpType.DIV =>
          Right(ParseState(restTokens, SExpr(r.primitives.Div()()) :: parsedSynElems))
        case OpType.BinOpType.EQ =>
          Right(ParseState(restTokens, SExpr(r.primitives.Equal()()) :: parsedSynElems))
        case OpType.BinOpType.GT =>
          Right(ParseState(restTokens, SExpr(r.primitives.Gt()()) :: parsedSynElems))
        case OpType.BinOpType.LT =>
          Right(ParseState(restTokens, SExpr(r.primitives.Lt()()) :: parsedSynElems))
        case OpType.BinOpType.MOD => Right(ParseState(restTokens, SExpr(r.primitives.Mod()()) :: parsedSynElems))
        case OpType.BinOpType.MUL => Right(ParseState(restTokens, SExpr(r.primitives.Mul()()) :: parsedSynElems))
        case OpType.BinOpType.SUB => Right(ParseState(restTokens, SExpr(r.primitives.Sub()()) :: parsedSynElems))
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

  def parseLeftBrace(parseState: ParseState): Either[ParseErrorOrState,ParseState] = {
    val ParseState(tokens, parsedSynElems) = parseState
    val nextToken :: restTokens = tokens

    nextToken match {
      case LBrace(_) => Right(ParseState(restTokens, parsedSynElems))
      case tok => Left(ParseError("failed to parse LeftBrace: " + tok + " is not an LeftBrace"))
    }
  }

  def parseRightBrace(parseState: ParseState): Either[ParseErrorOrState,ParseState] = {
    val ParseState(tokens, parsedSynElems) = parseState
    val nextToken :: restTokens = tokens

    nextToken match {
      case RBrace(_) => Right(ParseState(restTokens, parsedSynElems))
      case tok => Left(ParseError("failed to parse RightBrace: " + tok + " is not an RightBrace"))
    }
  }


  //_________________________________________________________Expres
}