package parser.lexer

import rise.core.{Lambda, semantics => rS, types => rt}
import rise.{core => r}

object parse {

  /*
   * Precondition: Only valid Tokens in tokenList.
   */
    def apply(tokenList: List[Token]): Expr = {
      val parseState: ParseState = (tokenList, Nil)
      val shineLambda = parseLambda(parseState)
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
  final case class SPrim(prim: r.Primitive) extends SyntaxElement

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
    val p =
      Some(parseState) |>
        parseBackslash |>
        parseIdent |>
        parseMaybeTypeAnnotation |>
        parseArrow |>
        parseExpression

    //      if ( parsedExprs is with type) { } else {}
    p match {
      case Some(pState) => {
        val (tokens, parsedExprs) = parseState
        val expr :: maybeTypeOrIdent  :: restExprs= parsedExprs
        val e: r.Expr = expr match {
          case SExpr(expr) => expr
          case SPrim(prim) => prim
          case SType(t) => throw new Exception("not type expected")
        }
        maybeTypeOrIdent match {
          case SType(t) => {
              val ident :: rExpr = restExprs
              ident match {
                case SExpr(r.Identifier(a)) => Some(tokens, SExpr(Lambda(r.Identifier(a)(), e)(t))::rExpr)
                case _ => None
              }
          }
          case SExpr(r.Identifier(a)) => Some(tokens, SExpr(Lambda(r.Identifier(a)(), e)()):: restExprs)
          case _ => None
        }
      }
      case None => None
    }

  }

  //_________________________________________________________Lambda
  //_________________________________________________________Expres

  def parseExpression(parseState: ParseState): Option[ParseState] = {
    Some(parseState) || parseLambda || parseApp || parseBinOperatorAnnotation || parseUnOperatorAnnotation || parseBracesAnnotation || parseIdent || parseNumber
  }

  def parseBracesAnnotation(parseState: ParseState): Option[ParseState] = {
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

  def parseBinOperatorAnnotation(parseState: ParseState): Option[ParseState] = {
    val p =
      Some(parseState)  |>
        parseExpression |>
        parseBinOperator |>
        parseExpression

    //      if ( parsedExprs is with type) { } else {}
    p match {
      case Some(pState) => Some(pState)
      case None => None
    }
  }

  def parseApp(parseState: ParseState): Option[ParseState] = {
    val p =
      Some(parseState)  |>
        parseExpression |>
        parseExpression

    //      if ( parsedExprs is with type) { } else {}
    p match {
      case Some(pState) => Some(pState)
      case None => None
    }
  }

  def parseUnOperatorAnnotation(parseState: ParseState): Option[ParseState] = {
    val (tokens, parsedSynElems) = parseState
    val nextToken :: restTokens = tokens

    val p = nextToken match {
      case UnOp(un, _) => un match {
        case OpType.UnaryOpType.NEG => Some((restTokens, SPrim(r.primitives.Neg()()) :: parsedSynElems))
        case OpType.UnaryOpType.NOT => Some((restTokens, SPrim(r.primitives.Not()()) :: parsedSynElems))
      }
      case _ => None
    }


    //      if ( parsedExprs is with type) { } else {}
    p match {
      case Some(pState) => Some(pState) |> parseExpression
      case None => None
    }

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
        case OpType.BinOpType.ADD => Some((restTokens, SPrim(r.primitives.Add()()) :: parsedSynElems))
        case OpType.BinOpType.DIV => Some((restTokens, SPrim(r.primitives.Div()()) :: parsedSynElems))
        case OpType.BinOpType.EQ => Some((restTokens, SPrim(r.primitives.Equal()()) :: parsedSynElems))
        case OpType.BinOpType.GT => Some((restTokens, SPrim(r.primitives.Gt()()) :: parsedSynElems))
        case OpType.BinOpType.LT => Some((restTokens, SPrim(r.primitives.Lt()()) :: parsedSynElems))
        case OpType.BinOpType.MOD => Some((restTokens, SPrim(r.primitives.Mod()()) :: parsedSynElems))
        case OpType.BinOpType.MUL => Some((restTokens, SPrim(r.primitives.Mul()()) :: parsedSynElems))
        case OpType.BinOpType.SUB => Some((restTokens, SPrim(r.primitives.Sub()()) :: parsedSynElems))
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