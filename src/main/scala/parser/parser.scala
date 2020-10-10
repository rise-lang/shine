package parser

import rise.core.{Lambda, semantics => rS, types => rt}
import rise.{core => r}

import scala.collection.mutable
object parser {

  abstract sealed class ParseErrorOrState()

  abstract sealed class ParseEnd() extends ParseErrorOrState()

  final case class ParseError(mes: String) extends ParseErrorOrState()

  /*
   * Precondition: Only valid Tokens in tokenList.
   */
  def apply(tokenList: List[Token]): MapFkt = {
    val parseState: ParseState = ParseState(tokenList, Nil, new MapFkt)
    val shineLambda: MapFkt = parseTypAnnotatedIdentAndThenNamedExprAndOtherTypAnnotatedIdens(parseState) match {
      case Left(map) => map
      case Right(errorOrState) => {
        println(errorOrState)
        throw new RuntimeException("failed parsing : " + errorOrState)
      }
    }
    println("parse: " + shineLambda)
    shineLambda
    //r.Identifier("placeholder")()
  }

  sealed trait SyntaxElement

  final case class SExpr(expr: r.Expr) extends SyntaxElement

  final case class SType(t: r.types.Type) extends SyntaxElement

  //Todo: if I have Identifier, I have to get the right Span and the Span is differntly each time
  type MapFkt = mutable.HashMap[String, Either[r.Expr, List[SyntaxElement]]]

  final case class ParseState(tokenStream: List[Token], parsedSynElems: List[SyntaxElement], map: MapFkt)

  implicit class ParseStatePipe(val ps: Either[ParseState, ParseErrorOrState]) extends AnyVal {
    def |>(f: ParseState => Either[ParseState, ParseErrorOrState]): Either[ParseState, ParseErrorOrState] = {
      println("|> : " + ps)
      ps match {
        case Left(p) => f(p)
        case Right(e) => Right(e)
      }
    }
  }

  implicit class ParseStateElse(val leftF: ParseState => Either[ParseState, ParseErrorOrState]) extends AnyVal {
    def ||(
            rightF: ParseState => Either[ParseState, ParseErrorOrState]
          ): ParseState => Either[ParseState, ParseErrorOrState] = {
      ps =>
        leftF(ps) match {
          case Right(_) => {
            println("|| : " + ps)
            rightF(ps)
          }
          case Left(resPs) => Left(resPs)
        }
    }
  }


  //_________________________________________________________Lambda


  def parseBackslash(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedExprs, map) = parseState
    if (tokens.isEmpty) {
      return Right(ParseError("failed to parse Backlash: " + " List is empty"))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Backslash(_) =>
      case tok => {
        println("failed parseBacklash: " + parseState)
        return Right(ParseError("failed to parse Backslash: " + tok + " is not an Backslash"))
      }
    }

    Left(ParseState(remainderTokens, parsedExprs, map))
  }

  def parseIdent(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseIdent: " + parseState)
    val ParseState(tokens, parsedSynElems, map) = parseState
    if (tokens.isEmpty) {
      return Right(ParseError("failed to parse Ident: " + " List is empty"))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Identifier(name, _) =>
        Left(ParseState(remainderTokens, SExpr(r.Identifier(name)()) :: parsedSynElems, map))
      case tok => {
        println("Abbruch parseIdent: " + tok + " : " + parseState)
        Right(ParseError("failed to parse Ident: " + tok + " is not an Identifier"))
      }
    }
  }

  private def getType(typ: TypeKind, remainderTokens: List[Token], parsedSynElems: List[SyntaxElement], map: MapFkt): Either[ParseState, ParseErrorOrState] = {
    val t = typ match {
      case ShortTyp() => Left(rt.i8)
      case IntTyp() => Left(rt.i32)
      case FloatTyp() => Left(rt.f32)
      case DoubleType() => Left(rt.f64)
      case BoolType() => Left(rt.bool)
      case notAtype => Right(ParseError("failed to parse Type: Not accepted Type" + notAtype))
    }
    t match {
      case Left(nowT) => Left(ParseState(remainderTokens, SType(nowT) :: parsedSynElems, map))
      case Right(e) => Right(ParseError("failed to parse Type"))
    }
  }


  def parseMaybeTypeAnnotation(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map) = parseState
    val colonToken :: typeToken :: remainderTokens = tokens

    colonToken match {
      case Colon(_) => {
        //if a type Annotation exist, we set the type new of the Identifier
        typeToken match {
          case Type(typ, _) => getType(typ, remainderTokens, parsedSynElems, map)
          case notAtype => Right(ParseError("failed to parse Type: " + notAtype + " is not an Type"))
        }
      }
      case _ => Left(parseState)
    }
  }

  def parseTypeAnnotation(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map) = parseState
    val colonToken :: typeToken :: remainderTokens = tokens

    colonToken match {
      case Colon(_) => {
        //if a type Annotation exist, we set the type new of the Identifier
        typeToken match {
          case Type(typ, _) => getType(typ, remainderTokens, parsedSynElems, map)
          case notAtype => Right(ParseError("failed to parse Type: " + notAtype + " is not an Type"))
        }
      }
      case notAColon => Right(ParseError("failed to parse Type: A TypeAnnotation is expected, but " + notAColon + " is not an Colon"))
    }
  }

  def parseTypeWithArrow(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map) = parseState
    val arrowToken :: typeToken :: remainderTokens = tokens

    println("parseTypeWithArrow: " + parseState)
    arrowToken match {
      case Arrow(_) => {

        typeToken match {
          case Type(typ, _) => {
            println("Type was in parseTypeWithArrow parsed: " + typ)
            getType(typ, remainderTokens, parsedSynElems, map)
          }
          case notAtype => Right(ParseError("failed to parse Type: " + notAtype + " is not an Type"))
        }
      }
      case notAnArrow => Right(ParseError("failed to parse Type: A TypeAnnotation is expected, but " + notAnArrow + " is not an Arrow"))
    }
  }

  def parseType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map) = parseState
    val typeToken :: remainderTokens = tokens

        typeToken match {
          case Type(typ, _) => getType(typ, remainderTokens, parsedSynElems, map)
          case notAtype => Right(ParseError("failed to parse Type: " + notAtype + " is not an Type"))
        }
  }

  def parseArrow(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedExprs, map) = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Arrow(_) =>
      case tok => Right(ParseError("failed to parse Type: " + tok + " is not an Arrow"))
    }

    Left(ParseState(remainderTokens, parsedExprs, map))
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

  def parseTypAnnotatedIdentAndThenNamedExprAndOtherTypAnnotatedIdens(parseState:ParseState): Either[MapFkt, ParseErrorOrState] = {
    if(parseState.tokenStream.isEmpty){
      throw new IllegalArgumentException("TokenStream is empty")
    }
    if(!parseState.parsedSynElems.isEmpty){
      throw new IllegalArgumentException("parsedSynElemnts has to be empty: " + parseState.parsedSynElems)
    }
    if(!parseState.map.isEmpty){
      throw new IllegalArgumentException("map has to be empty: " + parseState.map)
    }

    var (t, map): (List[Token], MapFkt) = parseState.tokenStream match {
      case BeginTypAnnotatedIdent(_) :: remainderTokens => {
        val ps:ParseState = ParseState(remainderTokens, Nil, parseState.map)
        val psNew = parseTypAnnotatedIdent(ps)
        psNew match {
          case Left((tokens, m)) => if(!tokens.isEmpty){
            (tokens, m)
          }else{
            throw new IllegalStateException("We need an NamedExpr too, because of that it " +
              "should not be possible only to have an TypAnnotatedIdent")
          }
          case Right(e) => return Right(e)
        }
      }
      case BeginNamedExpr(_):: remainderTokens => {
        throw new IllegalArgumentException("You aren't allowed to start with an NamedExpr")
      }
      case a => throw new IllegalArgumentException("You have started with something different: " + a)
    }

    while(!t.isEmpty) {
      println("tokens: " + t + " ,MapFkt: " + map)
       t match {
        case BeginTypAnnotatedIdent(_) :: remainderTokens => {
          val p:ParseState = ParseState(remainderTokens, Nil, map)
          val psNew = parseTypAnnotatedIdent(p)
          psNew match {
            case Left((tokens, m)) => {
              t = tokens
              map = m
            }
            case Right(e) => return Right(e)
          }
        }
        case BeginNamedExpr(_):: remainderTokens => {
          val p:ParseState = ParseState(remainderTokens, Nil, map)
          println("p: " + p)
          val psNew = parseNamedExpr(p)
          println("psNew: " + psNew)
          psNew match {
            case Left((tokens, m)) => {
              t = tokens
              map = m
            }
            case Right(e) => return Right(e)
          }
        }
        case a => throw new IllegalArgumentException("You have started with something different: " + a)
      }
    }

    Left(map)
  }

  /*
  top level Lambda expects that the type of the Identifier is defined!

  only use as top level!
   */
  def parseNamedExpr(parseState: ParseState): Either[(List[Token], MapFkt) , ParseErrorOrState] = {
    require(parseState.parsedSynElems.isEmpty, "parseState is not empty, but nothing should be in it yet")
    val psLambdaOld =
      Left(parseState)      |>
        parseIdent

    val (ps, identifierFkt, typesDefined) : (ParseState, r.Identifier, List[SyntaxElement]) = psLambdaOld match {
      case Right(e) => return Right(e)
      case Left(p) => {
        p.parsedSynElems.head match {
          case SExpr(r.Identifier(n))=>
            p.map.get(n) match {
              case None => {
                println("Identifier doesn't exist: " + n + " , " + psLambdaOld)
                throw new IllegalStateException("We want to parse an NamedExpr for " + n + " but this Identifier is not declared yet!")
              }
              case Some(Left(e)) => throw new IllegalStateException("The Lambda-Fkt should't be initiated yet!: "+ e)
              case Some(Right(l)) => if(l.isEmpty){
                throw new IllegalStateException("The List should not be empty!")
              }else{
                (ParseState(p.tokenStream, parseState.parsedSynElems, p.map), r.Identifier(n)(), l)
              }
            }
          case SExpr(expr) => throw new IllegalStateException("it is an Identifier expected: "+ expr)
          case SType(t) => throw new IllegalStateException("it is an Identifier expected but an Type is completely false: "+ t)
        }
      }
    }

    val psNamedExprBefore = {
        Left(ps)        |>
        parseEqualsSign |>
        parseMaybeAppExpr
    }

    val psNamedExpr = psNamedExprBefore match {
      case Right(e) => return Right(e)
      case Left(p) => {
        p.tokenStream match {

          case EndNamedExpr(_) :: remainderTokens => {
            val m = p.map
            m.get(identifierFkt.name) match {
              case None => throw new IllegalStateException("Identifier seems not to be in the Map: " + identifierFkt.name + " , " + m)
              case Some(Left(e)) => throw new IllegalStateException("The Lambda-Fkt should't be initiated yet!: "+ e)
              case Some(Right(l)) => if(!l.isEmpty){
                throw new IllegalStateException("The List should be empty! But it isn't. " +
                  "Probably we have one or more Types in the " +
                  "TypAnnotationIdent declared than the NamedExpr really has. Types left: " + l + "\nTypes defined: " + typesDefined + "\nNamedExpr: " + p.parsedSynElems)
              }else{
                //Todo: We have to give the Identifier (identifierFkt/p.map.get(n)) now a Type

                (remainderTokens, p.parsedSynElems, m)
              }
            }
          }
          case _ => {
            throw new IllegalStateException("NewExpr ends with an EndNamedExpr, but we have no EndNamedExpr at the end")
          }
        }
      }
    }

        val synElemList = psNamedExpr._2
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
        println("lambda finished: " + lambda)
        val m = psNamedExpr._3
        m.update(identifierFkt.name, Left(lambda))
        println("map updated: " + m + "\nRemainderTokens: " + psNamedExpr._1)
        Left((psNamedExpr._1, m))
      }

  def parseTypAnnotatedIdent(parseState: ParseState): Either[(List[Token], MapFkt) , ParseErrorOrState] = {
    require(parseState.parsedSynElems.isEmpty, "parseState is not empty, but nothing should be in it yet")
    val psLambdaOld =
      Left(parseState)      |>
        parseIdent

    val (ps, identifierFkt) : (ParseState, r.Identifier) = psLambdaOld match {
      case Right(e) => return Right(e)
      case Left(p) => {
        p.parsedSynElems.head match {
          case SExpr(r.Identifier(n))=> if(p.map.contains(n)){
            println("Identifier does already exist: " + n + " , " + psLambdaOld)
            throw new IllegalStateException("We want to parse an TypAnnotatedIdent for " + n + " but this Identifier is already declared!")
          }else{
            (p, r.Identifier(n)())
          }
          case SExpr(expr) => throw new IllegalStateException("it is an Identifier expected: "+ expr)
          case SType(t) => throw new IllegalStateException("it is an Identifier expected but an Type is completely false: "+ t)
        }
      }
    }

    val psNamedExprBefore = {
      Left(ParseState(ps.tokenStream, Nil, ps.map))        |>
        parseDoubleColons |>
        parseType |>
        parseMaybeAppTypAnnotatedIdentExpr
    }


    val psNew = psNamedExprBefore match {
      case Right(e) => return Right(e)
      case Left(p) => p
    }

    println("in the middle of TypAnnotatedIden: "+ psNew)
    psNew.tokenStream match {
      case EndTypAnnotatedIdent(_) :: remainderTokens => {
        val m = psNew.map
        m.put(identifierFkt.name, Right(psNew.parsedSynElems))
        println("return TypAnnotatedIdent: "+ remainderTokens+ " <<<<>>>> " + m )
        Left((remainderTokens, m))
      }
      case EndNamedExpr(_) :: remainderTokens => {
        throw new IllegalStateException("TypAnnotatedIdent ends with an EndTypAnnotatedIdent, but end with EndNamedExpr")
      }
      case a => {
        throw new IllegalStateException("TypAnnotatedIdent ends with an EndTypAnnotatedIdent, but we have no EndTypAnnotatedIdent at the end: " + a)
      }
    }
  }

  def parseMaybeAppTypAnnotatedIdentExpr(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    if(parseState.tokenStream.head.isInstanceOf[EndTypAnnotatedIdent]||parseState.tokenStream.head.isInstanceOf[RBrace]){
      println("Abbruch; parseMaybeAppTypAnnotatedIdentExpr: "+ parseState)
      return Left(parseState)
    }
    println("parseMaybeAppTypAnnotatedIdentExpr: "+ parseState)
    val ps: Either[ParseState, ParseErrorOrState]  =
      Left(parseState) |>
    (parseBracesExprType _ ||
      parseTypeWithArrow)

    ps |> parseMaybeAppTypAnnotatedIdentExpr
  }
  /*
  is the whole Syntax-Tree.
  the syntax-Tree has on top an Lambda-Expression
   */
  def parseLambda(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    if(parseState.tokenStream.isEmpty){
      println("Abbruch; parseLambda: "+ parseState)
      return Left(parseState)
    }
    println("parseLambda: " +parseState)
    val psOld =
      Left(parseState) |>
        parseBackslash |>
        parseIdent     |>
        parseMaybeTypeAnnotation |>
        parseArrow

        val psOrErr = psOld match {
          case Left(p) => parseMaybeAppExpr(ParseState(p.tokenStream,Nil, p.map))
          case Right(e) => {
            println("endLambda: "+ e)
            return Right(e)
          }
        }

    val (toks, synElemList, map) = psOrErr match {
      case Left(psNew) => {
        val expr = SExpr(combineExpressions(psNew.parsedSynElems))
        val newL = expr :: Nil
        val li:List[SyntaxElement] = psOld match {
          case Left(pa) => pa.parsedSynElems.reverse ++ newL
          case Right(_) => throw new RuntimeException("this should not be able to happen in parseLambdda, because I already have controlled this!")
        }
        val l = li.reverse
        (psNew.tokenStream,l, psNew.map)
      }
      case Right(e) => return Right(e)
    }

    val (expr, synElemListExpr) = (synElemList.head match {
      case SExpr(e) => e
      case a => throw new RuntimeException("Here is an Expression expected, but " + a +" ist not an Expression!")
    }, synElemList.tail)
    println("now in Lambda we want to combine our results: "+ expr +" # " + synElemListExpr)

    val (maybeTypedIdent, synElemListMaybeTIdent) =
      synElemListExpr.head match {
        case SType(t) => if(t.equals())
          synElemListExpr.tail.head match {
            case SExpr(i) => (i.setType(t), synElemListExpr.tail.tail)
            case a => throw new RuntimeException("Here is an Expression expected, but " + a +" is not an Expression!")
          }
        case SExpr(i) => (i, synElemListExpr.tail)
      }

    val lambda = Lambda(maybeTypedIdent.asInstanceOf[r.Identifier], expr)()
    println("synElemListMaybeTIdent: " + synElemListMaybeTIdent +" ______ " + synElemListExpr)
    Left(ParseState(toks, SExpr(lambda) :: synElemListMaybeTIdent, map))
  }

  //_________________________________________________________Lambda
  //_________________________________________________________Expres

  def parseNoAppExpr(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
//    if(parseState.tokenStream.head.isInstanceOf[EndNamedExpr]){
//      println("Abbruch; parseExpression: "+ parseState)
//      return Left(parseState)
//    }
    println("parseLowExpression: " + parseState)
    //FIXME parseState always true
    Left(parseState) |>
      (parseLambda _ || parseBracesExpr ||
        parseUnOperator || parseBinOperator || parseIdent ||
        parseNumber)

  }

  def parseBracesExpr(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseBracesExpr: "+ parseState)
    if(parseState.tokenStream.isEmpty){
      println("Abbruch; parseBracesExpr: "+ parseState)
      return Left(parseState)
    }
    val p =
      Left(ParseState(parseState.tokenStream,Nil, parseState.map))  |>
        parseLeftBrace  |>
        parseMaybeAppExpr |>
        parseRightBrace

    p match {
      case Left(pState) => {
        if(pState.parsedSynElems.isEmpty){
          val rBraceIndex = parseState.tokenStream.indexWhere(p=> p.isInstanceOf[RBrace])
          throw new RuntimeException("There was no Expression in Braces at posstion (" + 0 + " , " + rBraceIndex +
            " : "+ parseState.tokenStream.toString())
        }
        val expr = SExpr(combineExpressions(pState.parsedSynElems))
        val newL = expr :: Nil
        val li:List[SyntaxElement] = parseState.parsedSynElems.reverse ++ newL
        val l = li.reverse
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.map)
        Left(newParse)
      }
      case Right(e) => Right(e)
    }
  }

  def parseBracesExprType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseBracesExprType: "+ parseState)
    val p =
      Left(ParseState(parseState.tokenStream,Nil, parseState.map))  |>
        parseLeftBrace  |>
        parseType |>
        parseMaybeAppTypAnnotatedIdentExpr |>
        parseRightBrace

    p match {
      case Left(pState) => {
        if(pState.parsedSynElems.isEmpty){
          val rBraceIndex = parseState.tokenStream.indexWhere(p=> p.isInstanceOf[RBrace])
          throw new RuntimeException("There was no Expression in Braces at posstion (" + 0 + " , " + rBraceIndex +
            " : "+ parseState.tokenStream.toString())
        }
        val expr = SExpr(combineExpressions(pState.parsedSynElems))
        val newL = expr :: Nil
        val li:List[SyntaxElement] = parseState.parsedSynElems.reverse ++ newL
        val l = li.reverse
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.map)
        Left(newParse)
      }
      case Right(e) => Right(e)
    }
  }

  def parseMaybeAppExpr(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseMaybeAppExpr: " + parseState)
    if(parseState.tokenStream.head.isInstanceOf[RBrace]){
      println("L" +
        "RBrace is at the beginning of parseApp: " + parseState)
      return Left(parseState)
    }else if(parseState.tokenStream.head.isInstanceOf[EndNamedExpr]){
      println("EndNamedExpr sighted in ParseMaybeAppExpr: "+ parseState)
      return Left(parseState)
    }
    val parseStateOrError =
      Left(parseState)  |>
        parseNoAppExpr
    println("parseApp after parseLowExpression: "+ parseStateOrError)
    parseStateOrError match {
      case Right(e) => Right(e)
      case Left(ps)=> if(ps.tokenStream.isEmpty){
                              println("parseApp End, because TokenList is empty: "+ ps)
                              Left(ps)
                            }else{
                              val p = parseMaybeAppExpr(ps)
                                  p
                            }
    }
  }

  def parseEqualsSign(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedExprs, map) = parseState
    if (tokens.isEmpty) {
      return Right(ParseError("failed to parse EqualsSign: " + " List is empty"))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case EqualsSign(_) =>
      case tok => {
        println("failed parseBacklash: " + parseState)
        return Right(ParseError("failed to parse Backslash: " + tok + " is not an Backslash"))
      }
    }

    Left(ParseState(remainderTokens, parsedExprs, map))
  }

  def parseDoubleColons(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedExprs, map) = parseState
    if (tokens.isEmpty) {
      return Right(ParseError("failed to parse DoubleColons: " + " List is empty"))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case DoubleColons(_) =>
      case tok => {
        println("failed parseBacklash: " + parseState)
        return Right(ParseError("failed to parse Backslash: " + tok + " is not an Backslash"))
      }
    }

    Left(ParseState(remainderTokens, parsedExprs, map))
  }


  def parseUnOperator(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val nextToken :: remainderTokens = parseState.tokenStream

    val p = nextToken match {
      case UnOp(un, _) => un match {
        case OpType.UnaryOpType.NEG => Left(ParseState(remainderTokens, SExpr(r.primitives.Neg()()) :: parseState.parsedSynElems, parseState.map))
        case OpType.UnaryOpType.NOT => Left(ParseState(remainderTokens, SExpr(r.primitives.Not()()) :: parseState.parsedSynElems, parseState.map))
      }
      case tok => {
        println("UnaryOperatorWasExpected: "+ tok + ": " + remainderTokens)
        Right(ParseError("failed to parse parseUnOperator: " + tok + " is not an UnaryOperator"))
      }
    }
    p
  }

  def parseNumber(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("ParseNumber: " + parseState)
    val ParseState(tokens, parsedSynElems, map) = parseState
    if(tokens.isEmpty){
      return Right(ParseError("failed to parse Number: " + " List is empty"))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case I8(number, _) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.IntData(number))) :: parsedSynElems, map))
      case I32(number, _) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.IntData(number))) :: parsedSynElems, map))
      case F32(number, _) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.FloatData(number))) :: parsedSynElems, map))
      case F64(number, _) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.DoubleData(number))) :: parsedSynElems, map))
      case tok => Right(ParseError("failed to parse Number: " + tok + " is not an accepted Integer of Float"))
    }
  }


  def parseBinOperator(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseBinOperator: "+ parseState)
    val ParseState(tokens, parsedSynElems, map) = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case BinOp(op, _) => op match {
        case OpType.BinOpType.ADD =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.Add()()) :: parsedSynElems, map))
        case OpType.BinOpType.DIV =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.Div()()) :: parsedSynElems, map))
        case OpType.BinOpType.EQ =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.Equal()()) :: parsedSynElems, map))
        case OpType.BinOpType.GT =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.Gt()()) :: parsedSynElems, map))
        case OpType.BinOpType.LT =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.Lt()()) :: parsedSynElems, map))
        case OpType.BinOpType.MOD => Left(ParseState(remainderTokens, SExpr(r.primitives.Mod()()) :: parsedSynElems, map))
        case OpType.BinOpType.MUL => Left(ParseState(remainderTokens, SExpr(r.primitives.Mul()()) :: parsedSynElems, map))
        case OpType.BinOpType.SUB => Left(ParseState(remainderTokens, SExpr(r.primitives.Sub()()) :: parsedSynElems, map))
        case tok => {
          println("Das hier kann nicht sein, weil alle Operatoren müsste ich abgedeckt haben. BinOp: '" + tok + "' is no BinOperator!")
          Right(ParseError("failed to parse BinOperator: " + tok + " is not an accepted BinOp"))
        }
      }
      case tok => {
        println("BinOp: '" + tok + "' is no BinOperator!")
        Right(ParseError("failed to parse BinOperator: " + tok + " is not an BinOp"))
      }
    }
  }

  def parseLeftBrace(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map) = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case LBrace(_) => Left(ParseState(remainderTokens, parsedSynElems, map))
      case tok => Right(ParseError("failed to parse LeftBrace: " + tok + " is not an LeftBrace"))
    }
  }

  def parseRightBrace(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map) = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case RBrace(_) => Left(ParseState(remainderTokens, parsedSynElems, map))
      case tok => Right(ParseError("failed to parse RightBrace: " + tok + " is not an RightBrace"))
    }
  }


  //_________________________________________________________Expres
}