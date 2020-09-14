package parser.lexer

import rise.core.{Lambda, semantics => rS, types => rt}
import rise.{core => r}
import util.Execute.Exception

object parse {

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

  implicit class ParseStatePipe(val ps: Option[ParseState]) extends AnyVal {
    def |>(f: ParseState => Option[ParseState]): Option[ParseState] = {
      println("yuhu")
      ps match {
        case Some(p) => f(p)
        case None => None
      }
    }
  }

  implicit class ParseStateElse(val ps: Option[ParseState]) extends AnyVal {
    def ||(f: ParseState => Option[ParseState]): Option[ParseState] = {
      println("hi")
      ps match {
        case Some(p) => f(p) match {
          case None => ps
          case Some(par) => Some(par)
        }
        case None => None
      }
    }
  }




  //_________________________________________________________Lambda


  def parseBackslash(parseState: ParseState): Option[ParseState] = {
    val (tokens, parsedExprs) = parseState
    if(tokens.isEmpty){
      return None
    }
    val nextToken :: restTokens = tokens

    nextToken match {
      case Backslash(_) =>
      case _ => None
    }

    Some((restTokens, parsedExprs))
  }

  def parseIdent(parseState: ParseState): Option[ParseState] = {
    println("parseIdent: " + parseState)
    val (tokens, parsedSynElems) = parseState
    if(tokens.isEmpty){
      return None
    }
    val nextToken :: restTokens = tokens

    nextToken match {
      case Identifier(name, _) =>
        Some((restTokens, SExpr(r.Identifier(name)()) :: parsedSynElems))
      case _ => None
    }
  }

  def parseMaybeTypeAnnotation(parseState: ParseState): Option[ParseState] = {
    val (tokens, parsedSynElems) = parseState
    val colonToken :: typeToken :: restTokens = tokens

    colonToken match {
      case Colon(_) => {
        //if a type Annotation exist, we set the type new of the Identifier
        typeToken match {
          case Type(typ, _) => {
            val t: Option[rt.Type] = typ match {
              case ShortTyp() => Some(rt.i8)
              case IntTyp() => Some(rt.i32)
              case FloatTyp() => Some(rt.f32)
              case DoubleType() => Some(rt.f64)
              case _ => None
            }
            t match {
              case Some(t) => Some((restTokens, SType(t) :: parsedSynElems))
              case None => None
            }
          }
          case _ => None
        }
      }
      case _ => Some(parseState)
    }
  }

  def parseTypeAnnotation(parseState: ParseState): Option[ParseState] = {
    val (tokens, parsedSynElems) = parseState
    val colonToken :: typeToken :: restTokens = tokens

    colonToken match {
      case Colon(_) => {
        //if a type Annotation exist, we set the type new of the Identifier
        typeToken match {
          case Type(typ, _) => {
            val t: Option[rt.Type] = typ match {
              case ShortTyp() => Some(rt.i8)
              case IntTyp() => Some(rt.i32)
              case FloatTyp() => Some(rt.f32)
              case DoubleType() => Some(rt.f64)
              case _ => None
            }
            t match {
              case Some(t) => Some((restTokens, SType(t) :: parsedSynElems))
              case None => None
            }
          }
          case _ => None
        }
      }
      case _ => None
    }
  }

  def parseArrow(parseState: ParseState): Option[ParseState] = {
    val (tokens, parsedExprs) = parseState
    val nextToken :: restTokens = tokens

    nextToken match {
      case Arrow(_) =>
      case _ => None
    }

    Some((restTokens, parsedExprs))
  }

  /*
  top level Lambda expects that the type of the Identifier is defined!

  only use as top level!
   */
  def parseTopLevelLambda(parseState: ParseState): r.Expr = {
    require(parseState._2.isEmpty, "parseState is not empty, but nothing should be in it yet")
    val ps =
      Some(parseState)      |>
        parseBackslash      |>
        parseIdent          |>
        parseTypeAnnotation |>
        parseArrow          |>
        parseExpression

    println("durchgelaufen: "+ ps)

    require(ps.get._1.isEmpty, "Everything should be computed!")
    require(ps.get._2.length == 3, "it should now have exactly 3 Exprs: Identifier, Type of the Identifier and the expression")


    val synElemList = ps.get._2
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
  def parseLambda(parseState: ParseState): Option[ParseState] = {
    if(parseState._1.isEmpty){
      println("Abbruch; parseLambda: "+ parseState)
      return Some(parseState)
    }
    println("parseLambda: " +parseState)
    val ps =
      Some(parseState) |>
        parseBackslash |>
        parseIdent |>
        parseMaybeTypeAnnotation |>
        parseArrow |>
        parseExpression

    val synElemList:List[SyntaxElement] = ps match {
      case Some(a) => a._2
      case None => return None
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

    Some((ps.get._1, SExpr(lambda) :: synElemListMaybeTIdent))
  }

  //_________________________________________________________Lambda
  //_________________________________________________________Expres

  def parseExpression(parseState: ParseState): Option[ParseState] = {
    if(parseState._1.isEmpty){
      println("Abbruch; parseExpression: "+ parseState)
      return Some(parseState)
    }
    println("parseExpression: " + parseState)
    //FIXME parseState always true
    Some(parseState) || parseIdent ||
      parseLambda || parseApp ||
      parseBinOperatorApp || parseUnOperatorApp ||
      parseBracesExpr  || parseNumber
  }

  def parseBracesExpr(parseState: ParseState): Option[ParseState] = {
    if(parseState._1.isEmpty){
      println("Abbruch; parseBracesExpr: "+ parseState)
      return Some(parseState)
    }
    val p =
      Some(parseState)  |>
        parseLeftBrace  |>
        parseExpression |>
        parseRightBrace

    //      if ( parsedExprs is with type) { } else {}
    p match {
      case Some(pState) => Some(pState)
      case None => None
    }
  }

  //  1 + 2 * 3 = mul (add 1 2) 3 = App(App(mul, App(App(add, 1), 2)), 3)
  def parseBinOperatorApp(parseState: ParseState): Option[ParseState] = {
    if(parseState._1.isEmpty){
      println("Abbruch; parseBinOperatorApp: "+ parseState)
      return Some(parseState)
    }
    val p =
      Some(parseState)  |>
        parseExpression |>
        parseBinOperator |>
        parseExpression

    val (tokens, parsedExprs):(List[Token], List[SyntaxElement]) = p match {
      case Some(parseState) => parseState
      case None => return None //Todo: how can I do that whitout this ugly return?
    }
    val expr2 :: binOp  :: expr1 :: restExpr= parsedExprs

    expr2 match {
      case SExpr(e2) => expr1 match {
        case SExpr(e1) =>  binOp match { //TODO: now it only calculates from right to left, but normaly is * before +
          case SExpr(op) => Some((tokens, SExpr(r.App(r.App(op, e2)(), e1)())  :: restExpr))
          case _ => None
        }
        case _ => None
      }
      case _ => None
    }
  }

  def parseApp(parseState: ParseState): Option[ParseState] = {
    if(parseState._1.isEmpty){
      println("Abbruch; parseApp: "+ parseState)
      return Some(parseState)
    }
    val p =
      Some(parseState)  |>
        parseExpression |>
        parseExpression

    val (tokens, parsedExprs): ParseState = p match {
      case Some(parseState) => parseState
      case None => return None
    }
    val expr2 :: expr1 :: restExpr= parsedExprs

    expr2 match {
      case SExpr(e2) => expr1 match {
        case SExpr(e1) =>  Some((tokens, SExpr(r.App(e1, e2)())  :: restExpr))
        case _ => None
      }
      case _ => None
    }
  }

  def parseUnOperatorApp(parseState: ParseState): Option[ParseState] = {
    if(parseState._1.isEmpty){
      println("Abbruch; parseUnOperatorApp: "+ parseState)
      return Some(parseState)
    }
    val p =
    Some(parseState)    |>
      parseUnOperator   |>
      parseExpression

    val (tokens, parsedExprs): ParseState = p match {
      case Some(parseState) => parseState
      case None => return None
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
          case SExpr(e) =>  Some((tokens, SExpr(r.App(op, e)())  :: restExpr))
          case _ => None
        }
      }
      case _ => None
    }
  }

  def parseUnOperator(parseState: ParseState): Option[ParseState] = {

    val nextToken :: restTokens = parseState._1

    val p = nextToken match {
      case UnOp(un, _) => un match {
        case OpType.UnaryOpType.NEG => Some((restTokens, SExpr(r.primitives.Neg()()) :: parseState._2))
        case OpType.UnaryOpType.NOT => Some((restTokens, SExpr(r.primitives.Not()()) :: parseState._2))
      }
      case _ => None
    }
    p
  }

  def parseNumber(parseState: ParseState): Option[ParseState] = {
    val (tokens, parsedSynElems) = parseState
    if(tokens.isEmpty){
      return None
    }
    val nextToken :: restTokens = tokens

    nextToken match {
      case I8(number, _) =>
        Some((restTokens, SExpr(r.Literal(rS.IntData(number))) :: parsedSynElems))
      case I32(number, _) =>
        Some((restTokens, SExpr(r.Literal(rS.IntData(number))) :: parsedSynElems))
      case F32(number, _) =>
        Some((restTokens, SExpr(r.Literal(rS.FloatData(number))) :: parsedSynElems))
      case F64(number, _) =>
        Some((restTokens, SExpr(r.Literal(rS.DoubleData(number))) :: parsedSynElems))
      case _ => None
    }
  }


  def parseBinOperator(parseState: ParseState): Option[ParseState] = {
    val (tokens, parsedSynElems) = parseState
    val nextToken :: restTokens = tokens

    nextToken match {
      case BinOp(op, _) => op match {
        case OpType.BinOpType.ADD =>
          Some((restTokens, SExpr(r.primitives.Add()()) :: parsedSynElems))  //Todo: Or r.DSL.add ?
        case OpType.BinOpType.DIV =>
          Some((restTokens, SExpr(r.primitives.Div()()) :: parsedSynElems))
        case OpType.BinOpType.EQ =>
          Some((restTokens, SExpr(r.primitives.Equal()()) :: parsedSynElems))
        case OpType.BinOpType.GT =>
          Some((restTokens, SExpr(r.primitives.Gt()()) :: parsedSynElems))
        case OpType.BinOpType.LT =>
          Some((restTokens, SExpr(r.primitives.Lt()()) :: parsedSynElems))
        case OpType.BinOpType.MOD => Some((restTokens, SExpr(r.primitives.Mod()()) :: parsedSynElems))
        case OpType.BinOpType.MUL => Some((restTokens, SExpr(r.primitives.Mul()()) :: parsedSynElems))
        case OpType.BinOpType.SUB => Some((restTokens, SExpr(r.primitives.Sub()()) :: parsedSynElems))
      }
      case _ => None
    }
  }

  def parseLeftBrace(parseState: ParseState): Option[ParseState] = {
    val (tokens, parsedSynElems) = parseState
    val nextToken :: restTokens = tokens

    nextToken match {
      case LBrace(_) => Some((restTokens, parsedSynElems))
      case _ => None
    }
  }

  def parseRightBrace(parseState: ParseState): Option[ParseState] = {
    val (tokens, parsedSynElems) = parseState
    val nextToken :: restTokens = tokens

    nextToken match {
      case RBrace(_) => Some((restTokens, parsedSynElems))
      case _ => None
    }
  }


  //_________________________________________________________Expres
}