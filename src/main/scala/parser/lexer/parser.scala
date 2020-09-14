package parser.lexer

import rise.core.{Lambda, semantics => rS, types => rt}
import rise.{core => r}

object parse {

  /*
   * Precondition: Only valid Tokens in tokenList.
   */
    def apply(tokenList: List[Token]): r.Expr = {
      //val parseState: ParseState = (tokenList, Nil)
  //TODO use:    val shineLambda = parseLambda(parseState)
      r.Identifier("placeholder")()
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
      f(ps.get)
    }
  }

  implicit class ParseStateElse(val ps: Option[ParseState]) extends AnyVal {
    def ||(f: ParseState => Option[ParseState]): Option[ParseState] = {
      f(ps.get) match {
        case None => ps
        case Some(ps) => Some(ps)
      }
    }
  }




  //_________________________________________________________Lambda


  def parseBackslash(parseState: ParseState): Option[ParseState] = {
    val (tokens, parsedExprs) = parseState
    val nextToken :: restTokens = tokens

    nextToken match {
      case Backslash(_) =>
      case _ => None
    }

    Some((restTokens, parsedExprs))
  }

  def parseIdent(parseState: ParseState): Option[ParseState] = {
    val (tokens, parsedSynElems) = parseState
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
  is the whole Syntax-Tree.
  the syntax-Tree has on top an Lambda-Expression
   */
  def parseLambda(parseState: ParseState): Option[ParseState] = {
    val ps =
      Some(parseState) |>
        parseBackslash |>
        parseIdent |>
        parseMaybeTypeAnnotation |>
        parseArrow |>
        parseExpression

    val synElemList = ps.get._2
    val (expr, synElemListExpr) = (synElemList.head match {
      case SExpr(e) => e
      case _ => ???
    }, synElemList.tail)

    val (maybeTypedIdent, synElemListMaybeTIdent) =
      synElemListExpr.head match {
        case SType(t) =>
          synElemListExpr.tail.head match {
            case SExpr(i) => (i.setType(t), synElemList.tail.tail)
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
    //FIXME parseState always true
    Some(parseState) || parseLambda || parseApp ||
      parseBinOperatorApp || parseUnOperatorApp ||
      parseBracesExpr || parseIdent || parseNumber
  }

  def parseBracesExpr(parseState: ParseState): Option[ParseState] = {
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
    val p =
      Some(parseState)  |>
        parseExpression |>
        parseBinOperator |>
        parseExpression

    val (tokens, parsedExprs) = p match {
      case Some(parseState) => parseState
      case None => None
    }
    val expr2 :: binOp  :: expr1 :: restExpr= parsedExprs

//    expr2 match {
//      case SExpr(e2) => expr1 match {
//        case SExpr(e1) =>  binOp match { //Todo: I want to create a Tree but I have problems to create a Tree for BinaryOperators SExpr(BinaryExpr(e1, prim, e2))
//          case SPrim(prim) => Some((tokens, SExpr(r.substitute.exprInExpr(e1, prim, e2))  :: restExpr)) //Todo: What is r.substitute.exprInExpr(e1, prim, e2) ?
//          case _ => None
//        }
//        case _ => None
//      }
//      case _ => None
//    }
    ???
  }

  def parseApp(parseState: ParseState): Option[ParseState] = {
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

  //TODO maybe remove
  def parseUnOperatorApp(parseState: ParseState): Option[ParseState] = {
    // TODO use:
//    val (tokens, parsedSynElems) = parseState
//    val nextToken :: restTokens = tokens

//    val p = nextToken match {
//      case UnOp(un, _) => un match {
//        case OpType.UnaryOpType.NEG => Some((restTokens, SPrim(r.primitives.Neg()()) :: parsedSynElems))
//        case OpType.UnaryOpType.NOT => Some((restTokens, SPrim(r.primitives.Not()()) :: parsedSynElems))
//      }
//      case _ => None
//    }


//    val (tokens, parsedExprs):ParseState = p match {
//      case Some(parseState) => parseState
//      case None => None
//    }
//    val expr :: unOp :: restExpr= parsedExprs
//
//    val e = expr match {
//      case SExpr(e) =>  unOp match {
//          case SPrim(r.primitives.Neg()) => SExpr(r.TypedDSL.neg(e))
//          case SPrim(r.primitives.Not()) => SExpr(r.DSL.not(e))
//          case _ => None
//        }
//      case _ => None
//    }
//
//    e match {
//      case SExpr(ex) => Some((tokens, ex:: restExpr))
//      case _ => None
//    }
    ???
  }

  def parseNumber(parseState: ParseState): Option[ParseState] = {
    val (tokens, parsedSynElems) = parseState
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