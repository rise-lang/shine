package parser.lexer

import rise.core.Expr
import rise.core.types.{f32, f64, i32, i8}
import rise.{core => r}
import shine._


object parse {
  /*
   * Precondition: Only valid Tokens in tokenList.
   */
  def apply(tokenList: List[Token]): Expr = {
    val shineLambda = creat()

    def checkBracesNumber(tokenList: List[Token]): Unit = {
      val list: List[Either[PreAndErrorToken, Token]] = tokens
      val arr: Array[String] = fileReader.sourceLines
      val lB: Int = list.count(a => a match {
        case Right(LBrace(_)) => true
        case b => false
      })
      val rB: Int = list.count(a => a match {
        case Right(RBrace(_)) => true
        case b => false
      })

      if (lB > rB) {
        //for example "( .. ( ... )"
        val loc: Location = Location(arr.length - 1, arr(arr.length - 1).length)
        val ex = RightBraceMissing(new Span(fileReader, loc), fileReader)
        ex.throwException()
      } else if (lB < rB) { //for example "( .. ) ... )"
        val loc: Location = Location(arr.length - 1, arr(arr.length - 1).length)
        val ex = LeftBraceMissing(new Span(fileReader, loc), fileReader)
        ex.throwException()
      }
    }

    type ParseState = (List[Token], List[Expr])

    def parseBackslash(parseState: ParseState): ParseState = {
      val (tokens, parsedExprs) = parseState
      val nextToken :: restTokens = tokens

      nextToken match {
        case Backslash(_) =>
        case _ => throw new Exception("not a backslash")
      }

      (restTokens, parsedExprs)
    }

    def parseIdent(parseState: ParseState): ParseState = {
      val (tokens, parsedExprs) = parseState
      val nextToken :: restTokens = tokens

      val (ident,spanIndent) = nextToken match {
        case Identifier(name, span) => {
          if(parsedExprs.contains(Identifier)){
            (r.Identifier(name)(parsedExprs.findLast(a:Identifier => a == Identifier)) , span)
          }else{
            (r.Identifier(name)(i32), span)
          }
        }
        case _ => throw new Exception("not an identifier")
      }

      (restTokens, ident :: parsedExprs)
    }

    def parseMaybeTypeAnnotation(parseState: ParseState): ParseState = {
      val (tokens, parsedExprs) = parseState
      val colonToken :: typeToken :: restTokens = tokens

      colonToken match {
        case Colon(_) =>
        case _ => return parseState
      }
    //if a type Annotation exist, we set the type new of the Identifier
      val (t,tSpan) = typeToken match {
        case Type(t, span) => {
          t match {
            case ShortTyp() => (i8, span)
            case IntTyp() => (i32, span)
            case FloatTyp() => (f32, span)
            case DoubleType() => (f64, span)
            case _ => throw new Exception("not an accepted Type")
          }
        }
        case _ => throw new Exception("not a Type")
      }

      val inden :: exprs = parsedExprs
      inden.setType(t)

      (restTokens, inden :: parsedExprs)
    }

    def parseArrow(parseState: ParseState): ParseState = {
      val (tokens, parsedExprs) = parseState
      val nextToken :: restTokens = tokens

      nextToken match {
        case Arrow(_) =>
        case _ => throw new Exception("not an arrow")
      }

      (restTokens, parsedExprs)
    }

    /*
  is the whole Syntax-Tree.
  the syntax-Tree has on top an Lambda-Expression
   */
    def parseLambda(parseState: ParseState): ParseState = {
      val (restTokens, parsedExprs) =
        (tokenList, Nil) |>
        parseBackslash |>
        parseIdent |>
        parseMaybeTypeAnnotation |>
        parseArrow //|>
        //parseExpression

      if ( parsedExprs is with type) { } else {}

    }

    /*
  create an syntaxTree for an expression, which begins at postion pos
   */
    private def createExpression(pos: Int, oldIdentifier: Id[ScalarType]): Phrase[ExpType]

    =
    {
      val identifierName: String = giveIdentifierName(pos)._1
      require(identifierName == oldIdentifier.name, "the Identifier is not declared yet")
      return oldIdentifier

    }


    /*
give me the StringName of the Identifier in the Position in pos
no PreAndErrorToken is expected
*/
    private def giveIdentifierName(pos: Int): (String, Span)

    =
    {
      tokens(pos) match {
        case Right(Identifier(identifierType, span)) => (identifierType.identifier, span)
        case Right(a) => throw WrongToken(pos, tokens, "Identifier")
        case Left(a) => throw NoPreAndErrorTokenIsExpected(pos, tokens)
      }
    }

    /*
give me the StringName of the Identifier in the Position in pos
no PreAndErrorToken is expected
*/
    private def giveIdentifierType(pos: Int): (DataType, Span)

    =
    {
      tokens(pos) match {
        case Right(Type(typType, span)) => typType match {
          case ShortTyp() => (i8, span)
          case IntTyp() => (i32, span)
          case FloatTyp() => (f32, span)
          case DoubleType() => (f64, span)
        }
        case Right(a) => throw WrongToken(pos, tokens, "Identifier")
        case Left(a) => throw NoPreAndErrorTokenIsExpected(pos, tokens)
      }
    }

    /*
give me the Token in Position pos
no PreAndErrorToken is expected
 */
    private def giveToken(pos: Int): Token

    =
    {
      tokens(pos) match {
        case Right(a) => a
        case Left(a) => throw NoPreAndErrorTokenIsExpected(pos, tokens)
      }
    }

    /*
  check if the token in position pos is an Arrow
   */
    private def isArrow(pos: Int): Boolean

    =
    {
      tokens(pos) match {
        case Right(Arrow(_)) => true
        case Right(a) => false
        case Left(a) => false
      }
    }

    /*
check if the token in position pos is are Dots
 */
    private def isColon(pos: Int): Boolean

    =
    {
      tokens(pos) match {
        case Right(Colon(_)) => true
        case Right(a) => false
        case Left(a) => false
      }
    }

    /*
check if the token in position pos is an Backlash
 */
    private def isBacklash(pos: Int): Boolean

    =
    {
      tokens(pos) match {
        case Right(Backslash(_)) => true
        case Right(a) => false
        case Left(a) => false
      }
    }
  }
}
