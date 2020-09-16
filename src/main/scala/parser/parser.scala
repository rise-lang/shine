package parser

import rise.core.{Lambda, semantics => rS, types => rt}
import rise.{core => r}
import util.Execute.Exception

object parse {

  //Todo: throwException in ParseError is not used and RuntimeException is not so good as normal Exception,
  // because no checking is needed:
  // https://stackoverflow.com/questions/19857008/extending-exception-runtimeexception-in-java
  abstract sealed class ParseErrorOrState() extends RuntimeException

  abstract sealed class ParseEnd() extends ParseErrorOrState()
  //Todo: I only want for ParseError the Throwable keyword, but it is the only thing, which should be throwable
  abstract sealed class ParseError(token: Token) extends ParseErrorOrState(){
    val span = token.s
    val fileReader = token.s.file
    private def underline(str:String):String = {
      var s:String = ""
      for( a <- str){ s = s + a + "\u0332" }
      s
    }

    private def importantPart(str: String): String = {
      val s: String = underline(str)
      s
    }

    def throwException():Unit ={
      val currentColumn:String = fileReader.sourceLines(span.begin.column)
      val important:String = importantPart(currentColumn.substring(span.begin.row, span.end.row))
      val codeLine:String = if (span.begin.row-1 > 0){
        currentColumn.substring(0,span.begin.row) + important + currentColumn.substring(span.end.row, currentColumn.length)
      }else{
        important + currentColumn.substring(span.end.row, currentColumn.length)
      }
      val message:String = "ErrorToken: "+ this.toString +" at " + span.toString + "\n" + codeLine
      throw new Exception(message)
    }

//    override def throw():Unit = throwException()
  }
  final case class HereIsATypeIdentifierExpected(token:Token) extends ParseError(token)//{
//    override def toString = "A Type is expected but " + token + " is not a Type"
//  }
  final case class TokenListIsEmpty() extends ParseEnd() //Todo: Better name for the classes which are clearly only for States in the parsing process
  final case class BacklashWasExpected(token:Token) extends ParseError(token) //Todo: this is not correct, because this class is only used as a State!
  final case class IdentifierWasExpected(token:Token) extends ParseError(token)
  final case class NotAnAcceptedType(typeK:TypeKind) extends ParseEnd()
  final case class typeAnnotationNotCorrect(token:Token) extends ParseError(token)
  final case class HereIsATypeAnnotationExpected(token:Token) extends ParseError(token)
  final case class ArrowWasExpected(token:Token) extends ParseError(token)
  final case class AnExpressionAndNotATypeWasExpected(typ:rt.Type) extends ParseEnd()
  final case class UnaryOperatorWasExpected(token:Token) extends ParseError(token)
  final case class notAnAcceptedIntegerOrFloatType(token:Token) extends ParseError(token)
  final case class BinaryOperatorWasExpected(token:Token) extends ParseError(token)
  final case class LBraceWasExpected(token:Token) extends ParseError(token)
  final case class RBraceWasExpected(token:Token) extends ParseError(token)

  /*
   * Precondition: Only valid Tokens in tokenList.
   */
    def apply(tokenList: List[Token]): r.Expr = {
      val parseState: ParseState = (tokenList, Nil)
      val shineLambda:r.Expr = parseTopLevelLambda(parseState)
      println("parse: "+ shineLambda)
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

  type ParseState = (List[Token], List[SyntaxElement])

  implicit class ParseStatePipe(val ps: Either[   ParseErrorOrState,ParseState]) extends AnyVal {
    def |>(f: ParseState => Either[   ParseErrorOrState,ParseState]): Either[   ParseErrorOrState,ParseState] = {
      println("|> : " + ps)
      ps match {
        case Right(p) => f(p)
        case Left(e) => Left(e)
      }
    }
  }

  implicit class ParseStateElse(val ps: Either[   ParseErrorOrState,ParseState]) extends AnyVal {
    def ||(f: ParseState => Either[   ParseErrorOrState,ParseState]): Either[   ParseErrorOrState,ParseState] = {
      println("|| : " + ps)
      ps match {
        case Right(p) => f(p) match {
          case Left(_) => ps
          case Right(par) => Right(par)
        }
        case Left(e) => Left(e)
      }
    }
  }




  //_________________________________________________________Lambda


  def parseBackslash(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    val (tokens, parsedExprs) = parseState
    if(tokens.isEmpty){
      return Left(TokenListIsEmpty())
    }
    val nextToken :: restTokens = tokens

    nextToken match {
      case Backslash(_) =>
      case tok => {
        println("failed parseBacklash: "+ parseState)
        return Left(BacklashWasExpected(tok))
      }
    }

    Right((restTokens, parsedExprs))
  }

  def parseIdent(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    println("parseIdent: " + parseState)
    val (tokens, parsedSynElems) = parseState
    if(tokens.isEmpty){
      return Left(TokenListIsEmpty())
    }
    val nextToken :: restTokens = tokens

    nextToken match {
      case Identifier(name, _) =>
        Right((restTokens, SExpr(r.Identifier(name)()) :: parsedSynElems))
      case tok => {
        println("Abbruch parseIdent: " + tok +" : " + parseState)
        Left(IdentifierWasExpected(tok))
      }
    }
  }

  def parseMaybeTypeAnnotation(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    val (tokens, parsedSynElems) = parseState
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
              case notAtype => Left(NotAnAcceptedType(notAtype))
            }
            t match {
              case Right(nowT) => Right((restTokens, SType(nowT) :: parsedSynElems))
              case Left(e) => Left(e)
            }
          }
          case notAtype => Left(typeAnnotationNotCorrect(notAtype))
        }
      }
      case _ => Right(parseState)
    }
  }

  def parseTypeAnnotation(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    val (tokens, parsedSynElems) = parseState
    val colonToken :: typeToken :: restTokens = tokens

    colonToken match {
      case Colon(_) => {
        //if a type Annotation exist, we set the type new of the Identifier
        typeToken match {
          case Type(typ, _) => {
            val t: Either[   ParseErrorOrState, rt.Type] = typ match {
              case ShortTyp() => Right(rt.i8)
              case IntTyp() => Right(rt.i32)
              case FloatTyp() => Right(rt.f32)
              case DoubleType() => Right(rt.f64)
              case notAtype => Left(NotAnAcceptedType(notAtype))
            }
            t match {
              case Right(nowT) => Right((restTokens, SType(nowT) :: parsedSynElems))
              case Left(e) => Left(e)
            }
          }
          case notAtype => Left(typeAnnotationNotCorrect(notAtype))
        }
      }
      case notAColon => Left(HereIsATypeAnnotationExpected(notAColon))
    }
  }

  def parseArrow(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    val (tokens, parsedExprs) = parseState
    val nextToken :: restTokens = tokens

    nextToken match {
      case Arrow(_) =>
      case tok => Left(ArrowWasExpected(tok))
    }

    Right((restTokens, parsedExprs))
  }

  /*
  top level Lambda expects that the type of the Identifier is defined!

  only use as top level!
   */
  def parseTopLevelLambda(parseState: ParseState): r.Expr = {
    require(parseState._2.isEmpty, "parseState is not empty, but nothing should be in it yet")
    val psOld =
      Right(parseState)      |>
        parseBackslash      |>
        parseIdent          |>
        parseTypeAnnotation |>
        parseArrow          |>
        parseExpression

    println("durchgelaufen: "+ psOld)

    val ps:ParseState = psOld match {
      case Left(e) => throw e
      case Right(p) => p
    }
    require(ps._1.isEmpty, "Everything should be computed!")
    require(ps._2.length == 3, "it should now have exactly 3 Exprs: Identifier, Type of the Identifier and the expression")


    val synElemList = ps._2
    val (expr, synElemListExpr) = (synElemList.head match {
      case SExpr(e) => e
      case _ => ???
    }, synElemList.tail)

    val (typedIdent, synElemListTIdent) =
      synElemListExpr.head match {
        case SType(t) =>
          synElemListExpr.tail.head match {
            case SExpr(i) => (i.setType(t), synElemListExpr.tail.tail)
            case _ => ???
          }
        case SExpr(i) => ???
      }

    require(synElemListTIdent.isEmpty, "the List has to be empty!")

    val lambda = Lambda(typedIdent.asInstanceOf[r.Identifier], expr)()
    println("lambda durchgelaufen: " + lambda)
    lambda
  }
  /*
  is the whole Syntax-Tree.
  the syntax-Tree has on top an Lambda-Expression
   */
  def parseLambda(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
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

    val (toks, synElemList) = ps match {
      case Right(a) => (a._1,a._2)
      case Left(e) => return Left(e)
    }
    val (expr, synElemListExpr) = (synElemList.head match {
      case SExpr(e) => e
      case _ => ???
    }, synElemList.tail)

    val (maybeTypedIdent, synElemListMaybeTIdent) =
      synElemListExpr.head match {
        case SType(t) =>
          synElemListExpr.tail.head match {
            case SExpr(i) => (i.setType(t), synElemListExpr.tail.tail)
            case _ => ???
          }
        case SExpr(i) => (i, synElemList.tail)
      }

    val lambda = Lambda(maybeTypedIdent.asInstanceOf[r.Identifier], expr)()

    Right((toks, SExpr(lambda) :: synElemListMaybeTIdent))
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
    Right(parseState) || parseIdent || parseNumber ||
      parseUnOperatorApp || parseBracesExpr
  }

  def parseExpression(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    if(parseState._1.isEmpty){
      println("Abbruch; parseExpression: "+ parseState)
      return Right(parseState)
    }
    println("parseExpression: " + parseState)
    //FIXME parseState always true
    Right(parseState) || parseIdent || parseNumber ||
      parseUnOperatorApp || parseBracesExpr  ||
      parseBinOperatorApp || parseLambda // || parseApp //Todo: parseApp creates an endless loop because an expression
                                                          //Todo: creates two expression and so on...
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
      return Right(parseState)
    }
    val p =
      Right(parseState)  |>
        parseExpressionSimplified |>
        parseBinOperator |>
        parseExpressionSimplified

    val (tokens, parsedExprs):(List[Token], List[SyntaxElement]) = p match {
      case Right(parseState) => parseState
      case Left(e) => return Left(e) //Todo: how can I do that whitout this ugly return?
    }
    val expr2 :: binOp  :: expr1 :: restExpr= parsedExprs

    expr2 match {
      case SExpr(e2) => expr1 match {
        case SExpr(e1) =>  binOp match { //TODO: now it only calculates from right to left, but normaly is * before +
          case SExpr(op) => Right((tokens, SExpr(r.App(r.App(op, e2)(), e1)())  :: restExpr))
          case SType(t) => Left(AnExpressionAndNotATypeWasExpected(t))
        }
        case SType(t) => Left(AnExpressionAndNotATypeWasExpected(t))
      }
      case SType(t) => Left(AnExpressionAndNotATypeWasExpected(t))
    }
  }

  def parseApp(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    println("parseApp: " + parseState)
    //TODO: Here endless loop because Expression is everything!!!
    if(parseState._1.isEmpty){
      println("Abbruch; parseApp: "+ parseState)
      return Right(parseState)
    }//TODO: Here endless loop because Expression is everything!!!
    val p =
      Right(parseState)  |>
        parseExpressionSimplified |>
        parseExpressionSimplified

    val (tokens, parsedExprs): ParseState = p match {
      case Right(parseState) => parseState
      case Left(e) => return Left(e)
    }
    val expr2 :: expr1 :: restExpr= parsedExprs

    expr2 match {
      case SExpr(e2) => expr1 match {
        case SExpr(e1) =>  Right((tokens, SExpr(r.App(e1, e2)())  :: restExpr))
        case SType(t) => Left(AnExpressionAndNotATypeWasExpected(t))
      }
      case SType(t) => Left(AnExpressionAndNotATypeWasExpected(t))
    }
  }

  def parseUnOperatorApp(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    println("parseUnOperatorApp: "+ parseState)
    if(parseState._1.isEmpty || !parseState._1.exists(t => t.isInstanceOf[BinOp])){
      println("Abbruch; parseUnOperatorApp: "+ parseState)
      return Right(parseState)
    }
    val p =
    Right(parseState)    |>
      parseUnOperator   |>
      parseExpressionSimplified

    val (tokens, parsedExprs): ParseState = p match {
      case Right(parseState) => parseState
      case Left(e) => return Left(e)
    }
    val unop :: expr:: restExpr= parsedExprs

    unop match {
      case SExpr(op) => {
        op match {
          case r.primitives.Neg() =>
          case r.primitives.Not() =>
          case _ => throw Exception("this should not be happening, because parseUnOperator has to be parsed an operator!")
        }
        expr match {
          case SExpr(e) =>  Right((tokens, SExpr(r.App(op, e)())  :: restExpr))
          case SType(t) => Left(AnExpressionAndNotATypeWasExpected(t))
        }
      }
      case SType(t) => Left(AnExpressionAndNotATypeWasExpected(t))
    }
  }

  def parseUnOperator(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {

    val nextToken :: restTokens = parseState._1

    val p = nextToken match {
      case UnOp(un, _) => un match {
        case OpType.UnaryOpType.NEG => Right((restTokens, SExpr(r.primitives.Neg()()) :: parseState._2))
        case OpType.UnaryOpType.NOT => Right((restTokens, SExpr(r.primitives.Not()()) :: parseState._2))
      }
      case tok => Left(UnaryOperatorWasExpected(tok))
    }
    p
  }

  def parseNumber(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    val (tokens, parsedSynElems) = parseState
    if(tokens.isEmpty){
      return Left(TokenListIsEmpty())
    }
    val nextToken :: restTokens = tokens

    nextToken match {
      case I8(number, _) =>
        Right((restTokens, SExpr(r.Literal(rS.IntData(number))) :: parsedSynElems))
      case I32(number, _) =>
        Right((restTokens, SExpr(r.Literal(rS.IntData(number))) :: parsedSynElems))
      case F32(number, _) =>
        Right((restTokens, SExpr(r.Literal(rS.FloatData(number))) :: parsedSynElems))
      case F64(number, _) =>
        Right((restTokens, SExpr(r.Literal(rS.DoubleData(number))) :: parsedSynElems))
      case tok => Left(notAnAcceptedIntegerOrFloatType(tok))
    }
  }


  def parseBinOperator(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    val (tokens, parsedSynElems) = parseState
    val nextToken :: restTokens = tokens

    nextToken match {
      case BinOp(op, _) => op match {
        case OpType.BinOpType.ADD =>
          Right((restTokens, SExpr(r.primitives.Add()()) :: parsedSynElems))  //Todo: Or r.DSL.add ?
        case OpType.BinOpType.DIV =>
          Right((restTokens, SExpr(r.primitives.Div()()) :: parsedSynElems))
        case OpType.BinOpType.EQ =>
          Right((restTokens, SExpr(r.primitives.Equal()()) :: parsedSynElems))
        case OpType.BinOpType.GT =>
          Right((restTokens, SExpr(r.primitives.Gt()()) :: parsedSynElems))
        case OpType.BinOpType.LT =>
          Right((restTokens, SExpr(r.primitives.Lt()()) :: parsedSynElems))
        case OpType.BinOpType.MOD => Right((restTokens, SExpr(r.primitives.Mod()()) :: parsedSynElems))
        case OpType.BinOpType.MUL => Right((restTokens, SExpr(r.primitives.Mul()()) :: parsedSynElems))
        case OpType.BinOpType.SUB => Right((restTokens, SExpr(r.primitives.Sub()()) :: parsedSynElems))
        case tok => {
          println("Das hier kann nicht sein, weil alle Operatoren müsste ich abgedeckt haben. BinOp: '" + tok + "' is no BinOperator!")
          Left(BinaryOperatorWasExpected(nextToken)) //Todo: Bessere Fehlermeldung!
        }
      }
      case tok => {
        println("BinOp: '" + tok + "' is no BinOperator!")
        Left(BinaryOperatorWasExpected(tok))
      }
    }
  }

  def parseLeftBrace(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    val (tokens, parsedSynElems) = parseState
    val nextToken :: restTokens = tokens

    nextToken match {
      case LBrace(_) => Right((restTokens, parsedSynElems))
      case tok => Left(LBraceWasExpected(tok))
    }
  }

  def parseRightBrace(parseState: ParseState): Either[   ParseErrorOrState,ParseState] = {
    val (tokens, parsedSynElems) = parseState
    val nextToken :: restTokens = tokens

    nextToken match {
      case RBrace(_) => Right((restTokens, parsedSynElems))
      case tok => Left(RBraceWasExpected(tok))
    }
  }


  //_________________________________________________________Expres
}