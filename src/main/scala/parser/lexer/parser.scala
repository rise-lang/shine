package parser.lexer

import rise.core.Expr
import rise.core.{types => rt}
import rise.{core => r}

object parse {

  /*
   * Precondition: Only valid Tokens in tokenList.
   */
//  def apply(tokenList: List[Token]): Expr = {
//    val shineLambda = parseLambda()
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
              val t:Option[rt.Type] = typ match {
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
    val (tokenList, _) = parseState
    val p =
      Some((tokenList, Nil)) |>
        parseBackslash |>
        parseIdent |>
        parseMaybeTypeAnnotation |>
        parseArrow //|>
    //parseExpression

    //      if ( parsedExprs is with type) { } else {}
    p match {
      case Some(pState) => Some(pState)
      case None => None
    }

  }

  def parseExpression(parseState: ParseState): ParseState = {
    val (tokens, parsedExprs) = parseState
    val nextToken :: restTokens = tokens

    Some(parseState) || parseLambda || parseApp || parseIdent
  }
}