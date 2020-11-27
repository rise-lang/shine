package parser

import rise.core.{Lambda, primitives => rp, semantics => rS, types => rt}
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
    val parseState: ParseState = ParseState(tokenList, Nil, new MapFkt, new MapDepL)
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

  sealed trait NatElement
    final case class NNumber(nat: r.Nat) extends NatElement
    final case class NIdentifier(nat: rt.NatIdentifier) extends NatElement

  sealed trait DataElement
    final case class DIdentifier(data: rt.DataTypeIdentifier) extends DataElement
    final case class DType(data: rt.DataType) extends DataElement

  sealed trait SyntaxElement
    final case class SExpr(expr: r.Expr) extends SyntaxElement
    final case class SType(t: rt.Type) extends SyntaxElement
    final case class SAnyRef(value: AnyRef) extends SyntaxElement //Todo: AnyRef entfernen
    final case class SNat(nat: NatElement) extends SyntaxElement
    final case class SData(data: DataElement) extends SyntaxElement

  sealed trait RiseKind //Todo: Scoping einbauen, also Kind nennen und Token explizit immer hinzufügen
    final case class RData() extends RiseKind
    final case class RNat() extends RiseKind
    final case class RAddrSpace() extends RiseKind

  //Todo: if I have Identifier, I have to get the right Span and the Span is differntly each time
  type MapFkt = mutable.HashMap[String, Either[r.Expr, r.types.Type]]
  type MapDepL = mutable.HashMap[String, RiseKind]

  final case class ParseState(tokenStream: List[Token], parsedSynElems: List[SyntaxElement], mapFkt: MapFkt, mapDepL:MapDepL)

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
    val ParseState(tokens, parsedExprs, mapFkt, mapDepL) = parseState
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

    Left(ParseState(remainderTokens, parsedExprs, mapFkt, mapDepL))
  }



  //Todo: maybe it works if i change AnyRef to r.Primitive and delete annotation and makeArray
  def matchPrimitiveOrIdentifier(name:String): Either[r.Primitive, r.Identifier]= {
    require(name.matches("[a-z][a-zA-Z0-9_]*"), "'"+name+ "' has not the preffered structure")
    name match {
        //Todo: annotation and makeArray
        //      case "annotation" => Right(PrimitiveAnnotation())
        //      case "makeArray" => Right(PrimitiveMakeArray())
      case "cast" => Left(rp.Cast()())
      case "depJoin" => Left(rp.DepJoin()())
      case "depMapSeq" => Left(rp.DepMapSeq()())
      case "depZip" => Left(rp.DepZip()())
      case "drop" => Left(rp.Drop()())
      case "fst" => Left(rp.Fst()())
      case "gather" => Left(rp.Gather()())
      case "generate" => Left(rp.Generate()())
      case "idx" => Left(rp.Idx()())
      case "id" => Left(rp.Id()())
      case "indexAsNat" => Left(rp.IndexAsNat()())
      case "iterate" => Left(rp.Iterate()())
      case "join" => Left(rp.Join()())
      case "let" => Left(rp.Let()())
      case "map" => Left(rp.Map()())
      case "mapFst" => Left(rp.MapFst()())
      case "mapSnd" => Left(rp.MapSnd()())
      case "mapSeq" => Left(rp.MapSeq()())
      case "mapStream" => Left(rp.MapStream()())
      case "iterateStream" => Left(rp.IterateStream()())
      case "mapSeqUnroll" => Left(rp.MapSeqUnroll()())
      case "toMem" => Left(rp.ToMem()())
      case "natAsIndex" => Left(rp.NatAsIndex()())
      case "padEmpty" => Left(rp.PadEmpty()())
      case "padClamp" => Left(rp.PadClamp()())
      case "partition" => Left(rp.Partition()())
      case "pair" => Left(rp.Pair()())
      case "reduce" => Left(rp.Reduce()())
      case "reduceSeq" => Left(rp.ReduceSeq()())
      case "reduceSeqUnroll" => Left(rp.ReduceSeqUnroll()())
      case "reorder" => Left(rp.Reorder()())
      case "scanSeq" => Left(rp.ScanSeq()())
      case "slide" => Left(rp.Slide()())
      case "circularBuffer" => Left(rp.CircularBuffer()())
      case "rotateValues" => Left(rp.RotateValues()())
      case "snd" => Left(rp.Snd()())
      case "split" => Left(rp.Split()())
      case "take" => Left(rp.Take()())
      case "transpose" => Left(rp.Transpose()())
      case "select" => Left(rp.Select()())
      case "zip" => Left(rp.Zip()())
      case "neg" => Left(rp.Neg()())
      case "not" => Left(rp.Not()())
      case "add" => Left(rp.Add()())
      case "sub" => Left(rp.Sub()())
      case "mul" => Left(rp.Mul()())
      case "div" => Left(rp.Div()())
      case "mod" => Left(rp.Mod()())
      case "gt" => Left(rp.Gt()())
      case "lt" => Left(rp.Lt()())
      case "equal" => Left(rp.Equal()())
      case "asVectorAligned" => Left(rp.AsVectorAligned()())
      case "asVector" => Left(rp.AsVector()())
      case "asScalar" => Left(rp.AsScalar()())
      case "vectorFromScalar" => Left(rp.VectorFromScalar()())
      case "printType" => Left(rp.PrintType()())
      case "typeHole" => Left(rp.TypeHole()())
      case _ => Right(r.Identifier(name)())
    }
  }

  def parseIdent(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseIdent: " + parseState)
    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
    if (tokens.isEmpty) {
      return Right(ParseError("failed to parse Ident: " + " List is empty"))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Identifier(name, _) => {
        matchPrimitiveOrIdentifier(name) match {
          case Right(i) => Left(ParseState(remainderTokens, SExpr(i) :: parsedSynElems, map, mapDepL))
          case Left(prim) =>  Left(ParseState(remainderTokens, SExpr(prim) :: parsedSynElems, map, mapDepL))
        }
      }
      case tok => {
        println("Abbruch parseIdent: " + tok + " : " + parseState)
        Right(ParseError("failed to parse Ident: " + tok + " is not an Identifier"))
      }
    }
  }

  def parseTypeIdentToCorrectForm(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseTypeIdentToCorrectForm: "+ parseState)
    parseState.tokenStream.head match {
      case TypeIdentifier(name, _) => parseState.mapDepL.get(name) match {
        case None =>  Right(ParseError("It exists no DepLambda with this Name: "+ name))
        case Some(RNat()) =>
          Left(ParseState(parseState.tokenStream.tail, SNat(NIdentifier(rt.NatIdentifier(name))) :: parseState.parsedSynElems, parseState.mapFkt, parseState.mapDepL))
        case Some(RData()) =>
          Left(ParseState(parseState.tokenStream.tail, SData(DIdentifier(rt.DataTypeIdentifier(name))) :: parseState.parsedSynElems, parseState.mapFkt, parseState.mapDepL))
        case Some(RAddrSpace()) =>throw new IllegalStateException("DepAddrSpace is not implemented yet")
      }
      case t => Right(ParseError("Not an TypeIdentifier: "+ t))
    }
  }

  def parseTypeIdent(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseTypeIdent: " + parseState)
    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
    if (tokens.isEmpty) {
      return Right(ParseError("failed to parse Ident: " + " List is empty"))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case TypeIdentifier(name, _) => Left(ParseState(remainderTokens, SType(rt.TypeIdentifier(name)) :: parsedSynElems, map, mapDepL))
      case tok => {
        println("Abbruch parseTypeIdent: " + tok + " : " + parseState)
        Right(ParseError("failed to parse TypeIdent: " + tok + " is not an TypeIdentifier"))
      }
    }
  }

  //Todo: In Parser ArrayType etc. has to be created!!!
//  private def getType(typ: ConcreteType): Option[rt.DataType] = typ match {
//      case ArrayType(nat, concreteType) => {
//        val conT = getType(concreteType) match {
//          case None => return None
//          case Some(t) => t
//        }
//        Some(rt.ArrayType(getNat(nat),conT))
//      }
//      case TupleType(typ1, typ2) => {
//        val t1 = getType(typ1) match {
//          case None => return None
//          case Some(t) => t
//        }
//        val t2 = getType(typ2) match {
//          case None => return None
//          case Some(t) => t
//        }
//        Some(rt.PairType(t1, t2))
//      }
//      case IndexType(nat) => Some(rt.IndexType(getNat(nat)))
//      case otherType => getScalarType(otherType)
//    }

  private def getScalarType(typ: ConcreteType): Option[rt.DataType] = typ match {
    case ShortTyp() => Some(rt.i8)
    case IntTyp() => Some(rt.i32)
    case FloatTyp() => Some(rt.f32)
    case DoubleType() => Some(rt.f64)
    case BoolType() => Some(rt.bool)
    case notAtype => {
      println("                        This is not an accepted Type: "+ notAtype)
      None
    }
  }
  //Todo: Im Parser soll N von Lexer.TypeIdentifier zu rt.NatIdentifier übersetzt werden.
//  private def getNat(nat: NatType):r.Nat= {
//    nat match {
//      case NatNumber(number) => number
//      case NatIdent(name) => rt.NatIdentifier(name)
//    }
//  }


//  def parseMaybeTypeAnnotation(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
//    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
//    val colonToken :: typeToken :: remainderTokens = tokens
//
//    colonToken match {
//      case Colon(_) => {
//        //if a type Annotation exist, we set the type new of the Identifier
//        typeToken match {
//            //Todo: Here is only for ScalarType yet, but I have to implement my complex Alg. now for ArrayType etc. here in parser
//          case ScalarType(typ, _) => {
//            val t = getScalarType(typ)
//            t match {
//              case None => Right(ParseError("failed to parse ScalarType: " + typ + " is not an accpeted Type"))
//              case Some(parsedType) => Left(ParseState(remainderTokens, SType(parsedType)::parseState.parsedSynElems, parseState.mapFkt, mapDepL))
//            }
//          }
//          case notAtype => Right(ParseError("failed to parse(match) ScalarType: " + notAtype + " is not an Type"))
//        }
//      }
//      case _ => Left(parseState)
//    }
//  }

  def parseMaybeTypeAnnotation(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
    val colonToken :: typeTokenWithremainderTokens = tokens

    colonToken match {
      case Colon(_) => {
        //if a type Annotation exist, we set the type new of the Identifier
        parseType(ParseState(typeTokenWithremainderTokens, Nil, map, mapDepL)) match {
          case Right(e) => Right(e)
          case Left(newPS) => if (newPS.parsedSynElems.length == 1) {
            return Left(ParseState(newPS.tokenStream, newPS.parsedSynElems.head :: parsedSynElems, newPS.mapFkt, newPS.mapDepL))
          } else {
            throw new IllegalStateException("It should only be one argument in the end of the computing of the Type exist")
          }
        }
      }
      case _ => Left(parseState)
    }
  }

  def parseTypeAnnotation(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
    val colonToken :: typeToken :: remainderTokens = tokens

    colonToken match {
      case Colon(_) => {
        //if a type Annotation exist, we set the type new of the Identifier
        typeToken match {
          case ScalarType(typ, _) =>  {
            //Todo: Complex Alg for Types Parsing
            val t = getScalarType(typ)
            t match {
              case None => Right(ParseError("failed to parse ScalarType: " + typ + " is not an accpeted ScalarType"))
              case Some(parsedType) => Left(ParseState(remainderTokens, SType(parsedType)::parseState.parsedSynElems, parseState.mapFkt, mapDepL))
            }
          }
          case notAtype => Right(ParseError("failed to parse Type: " + notAtype + " is not an ScalarType"))
        }
      }
      case notAColon => Right(ParseError("failed to parse Type: A TypeAnnotation is expected, but " + notAColon + " is not an Colon"))
    }
  }

  def parseScalarType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
      val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
      val inputType :: remainderTokens = tokens

      println("parseTypeWithoutArrow: " + parseState)
          inputType match {
            case ScalarType(typ, _) => {
              println("Type was in parseTypeWithoutArrow parsed: " + typ)
              //Todo: Complex Alg for Type Parsing
              val parsedInType = getScalarType(typ)
              val inT = parsedInType.getOrElse(return Right(ParseError("IllegalInputScalaType")))
              Left(ParseState(remainderTokens, SType(inT):: parseState.parsedSynElems, parseState.mapFkt, mapDepL))
            }
            case notAtype => Right(ParseError("failed to parse ScalarType (in parseScalarType): " + notAtype + " is not an Type"))
          }
  }

  def parseKindWithDepArrowAfterForDepLambda(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseKind: " + parseState)
    val nameOfIdentifier:String = parseState.parsedSynElems.head match {
      case SType(rt.TypeIdentifier(name)) => name
      case t => throw new IllegalStateException("Here should be an TypeIdentifier and not '" + t + "'")
    }
    val newPS = parseState.parsedSynElems.tail

    val tokens = parseState.tokenStream
    tokens.head match {
      case Kind(concreteKind, span) => tokens.tail.head match{
        case DepArrow(_) =>       {
          println("Kind was in parseDepFunctionType parsed: " + concreteKind)
          parseMaybeAppExpr(ParseState(tokens.tail.tail, Nil, parseState.mapFkt, parseState.mapDepL)) match {
            case Right(e) => Right(e)
            case Left(pS) => {
              println("In the middle of parseDepFunctionType: " + pS)
              if (pS.parsedSynElems.tail.nonEmpty) return Right(ParseError("ParsedSynElems.tail has to be empty!"))
              val depLam:SExpr = pS.parsedSynElems.head match {
                case SExpr(outT) => {
                  concreteKind match {
                    case Data() => SExpr(r.DepLambda[rt.DataKind](rt.DataTypeIdentifier(nameOfIdentifier), outT)())
                    case Nat() => SExpr(r.DepLambda[rt.NatKind](rt.NatIdentifier(nameOfIdentifier), outT)())
                    case AddrSpace() => SExpr(r.DepLambda[rt.AddressSpaceKind](rt.AddressSpaceIdentifier(nameOfIdentifier), outT)())
                    case ki => return Right(ParseError("Not an accepted Kind: "+ ki))
                  }
                }
                case _ => return Right(ParseError("Not a Type"))
              }
              Left(ParseState(pS.tokenStream, depLam:: newPS, pS.mapFkt, pS.mapDepL))
            }
          }
        }
        case notAnDepArrow => Right(ParseError("failed to parse DepArrow: " + notAnDepArrow + " is not an DepArrow"))
      }
      case Arrow(_)=> Right(ParseError("DepArrow and not Arrow was expected"))
      case t => Right(ParseError("DepArrow and not "+ t + " was expected"))
    }
  }




  def parseKindWithDepArrowAfterForDepFunctionType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseKind: " + parseState)
    val nameOfIdentifier:String = parseState.parsedSynElems.head match {
      case SType(rt.TypeIdentifier(name)) => name
      case t => throw new IllegalStateException("Here should be an TypeIdentifier and not '" + t + "'")
    }
    val newPS = parseState.parsedSynElems.tail

    val tokens = parseState.tokenStream
     tokens.head match {
      case Kind(concreteKind, span) => tokens.tail.head match{
        case DepArrow(_) =>       {
          if(parseState.mapDepL.contains(nameOfIdentifier)){
            throw new IllegalArgumentException("It exists already an DepLambda with this Name: "+ nameOfIdentifier)
          }else if(parseState.mapFkt.contains(nameOfIdentifier)){
            throw new IllegalArgumentException("It exists already an Fkt with this Name: "+ nameOfIdentifier)
          }
          concreteKind match {
            case Data() => parseState.mapDepL.update(nameOfIdentifier, RData())
            case Nat() => parseState.mapDepL.update(nameOfIdentifier, RNat())
            case AddrSpace() => parseState.mapDepL.update(nameOfIdentifier, RAddrSpace())
            case ki => return Right(ParseError("Not an accepted Kind: "+ ki))
          }
          println("Kind was in parseDepFunctionType parsed: " + concreteKind)
          parseCompleteType(ParseState(tokens.tail.tail, Nil, parseState.mapFkt, parseState.mapDepL)) match {
            case Right(e) => Right(e)
            case Left(pS) => {
              if (pS.parsedSynElems.tail.nonEmpty) return Right(ParseError("ParsedSynElems.tail has to be empty!"))
              val depFun:SType = pS.parsedSynElems.head match {
                case SType(outT) => {
                  concreteKind match {
                    case Data() => SType(rt.DepFunType[rt.DataKind, rt.Type](rt.DataTypeIdentifier(nameOfIdentifier), outT))
                    case Nat() => SType(rt.DepFunType[rt.NatKind, rt.Type](rt.NatIdentifier(nameOfIdentifier), outT))
                    case AddrSpace() => SType(rt.DepFunType[rt.AddressSpaceKind, rt.Type](rt.AddressSpaceIdentifier(nameOfIdentifier), outT))
                    case ki => return Right(ParseError("Not an accepted Kind: "+ ki))
                  }
                }
                case _ => return Right(ParseError("Not a Type"))
              }
              Left(ParseState(pS.tokenStream, depFun:: newPS, pS.mapFkt, pS.mapDepL))
            }
          }
        }
        case notAtype => Right(ParseError("failed to parse Type (in DepFunction): " + notAtype + " is not an Type"))
      }
      case Arrow(_)=> Right(ParseError("DepArrow and not Arrow was expected"))
      case t => Right(ParseError("DepArrow and not "+ t + " was expected"))
      }
  }

  def parseDepFunctionType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseDepFunctionType: " + parseState)
    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
    if( tokens.length < 3){
      return Right(ParseError("only "+ tokens.length + " arguments are in the TokenList, but we need minimum 3!"))
    }

    val psOld =
      Left(parseState) |>
        parseTypeIdent     |>
        parseColon |>
        parseKindWithDepArrowAfterForDepFunctionType
    psOld
  }

  def parseDepLambda(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseDepFunctionType: " + parseState)
    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
    if( tokens.length < 3){
      return Right(ParseError("only "+ tokens.length + " arguments are in the TokenList, but we need minimum 3!"))
    }

    val psOld =
      Left(parseState) |>
        parseBackslash |>
        parseTypeIdent     |>
        parseColon |>
        parseKindWithDepArrowAfterForDepLambda
    psOld
  }

  def parseFunType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
    val inputType :: arrowToken :: remainderTokens = tokens

    println("parseTypeWithArrow: " + parseState)
    arrowToken match {
      case Arrow(_) => {

        inputType match {
          case ScalarType(typ, _) => {
            println("Type was in parseTypeWithArrow parsed: " + typ)
            //Todo: Complex Alg for Type Parsing
            val parsedInType = getScalarType(typ)
            val inT = parsedInType.getOrElse(return Right(ParseError("IllegalInputScalaType")))
            parseCompleteType(ParseState(remainderTokens, Nil, parseState.mapFkt, parseState.mapDepL)) match {
              case Right(e) => Right(e)
              case Left(pS) => {
                if (pS.parsedSynElems.tail.nonEmpty) return Right(ParseError("ParsedSynElems.tail has to be empty!"))
                pS.parsedSynElems.head match {
                  case SType(outT) => Left(ParseState(pS.tokenStream, SType(rt.FunType(inT, outT)):: parseState.parsedSynElems, pS.mapFkt, pS.mapDepL))
                  case _ => Right(ParseError("Not a Type"))
                }
              }
            }
          }
          case notAtype => Right(ParseError("failed to parse Type (in FunType): " + notAtype + " is not an Type"))
        }
      }
      case notAnArrow => Right(ParseError("failed to parse Arrow: A TypeAnnotation is expected, but " + notAnArrow + " is not an Arrow"))
    }
  }

  def parseArrow(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedExprs, map, mapDepL) = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Arrow(_) =>
      case tok => Right(ParseError("failed to parse Arrow: " + tok + " is not an Arrow"))
    }

    Left(ParseState(remainderTokens, parsedExprs, map, mapDepL))
  }

  private def combineTypes(typesList: List[r.types.Type]) : r.types.Type = {
    if(typesList.isEmpty){
      throw new IllegalArgumentException("the ElemList is empty!")
    }
    if(typesList.tail.isEmpty){
      typesList.head
    }else{
      rt.FunType(typesList.head, combineTypes(typesList.tail))
    }
  }

//  private def combineExpressions(synElemList: List[SyntaxElement]) : r.Expr = {
//    if(synElemList.isEmpty){
//      throw new IllegalArgumentException("the ElemList is empty!")
//    }
//    var synE = synElemList.reverse
//    var e:r.Expr = synE.head match {
//      case SExpr(expr) => {
//        synE = synE.tail
//        expr
//      }
//      case SAnyRef(anyref) => throw new RuntimeException("AnyRefs aren't supported yet: " + anyref + " , "+ synElemList)
//      case SType(t) => throw new RuntimeException("List should't have Types at this beginning position! " + t)
//      case SData(t) => throw new RuntimeException("List should't have any Data at this position! " + t)
//      case SNat(t) => throw new RuntimeException("List should't have any Nats at this position! " + t)
//    }
//    println("I will combine Expressions in Lambda: "+ synE + " <::> " + e)
//    while(!synE.isEmpty){
//      synE.head match {
//        case SExpr(expr1) => {
//          e = r.App(e, expr1)()
//          synE = synE.tail
//        }
//      case SAnyRef(anyref) => throw new RuntimeException("AnyRefs aren't supported yet: " + anyref + " , "+ synElemList)
//      case SType(t) => throw new  RuntimeException("List should't have Types at this position! " + t)
//        case SData(t) => throw new RuntimeException("List should't have any Data at this position! " + t)
//        case SNat(t) => throw new RuntimeException("List should't have any Nats at this position! " + t)
//      }
//    }
//    println("I have combined the Expressions in Lambda: "+ e)
//    e
//  }

  private def combineExpressionsDependent(synElemList: List[SyntaxElement]) : r.Expr = {
    if(synElemList.isEmpty){
      throw new IllegalArgumentException("the ElemList is empty!")
    }
    var synE = synElemList.reverse
    var e:r.Expr = synE.head match {
      case SExpr(expr) => {
        synE = synE.tail
        expr
      }
      case SAnyRef(anyref) => throw new RuntimeException("AnyRefs aren't supported yet: " + anyref + " , "+ synElemList)
      case SType(t) => throw new RuntimeException("List should't have Types at this beginning position! " + t)
      case SData(t) => throw new RuntimeException("List should't have any Data at this position! " + t)
      case SNat(t) => throw new RuntimeException("List should't have any Nats at this position! " + t)
    }
    println("I will combine Expressions in Lambda: "+ synE + " <::> " + e)
    while(!synE.isEmpty){
      synE.head match {
        case SExpr(expr1) => {
          e = r.App(e, expr1)()
          synE = synE.tail
        }
        case SData(DIdentifier(rt.DataTypeIdentifier(name,_))) => {

//          case Data() => SExpr(r.DepLambda[rt.DataKind](rt.DataTypeIdentifier(nameOfIdentifier), outT)())
          e = r.DepApp[rt.DataKind](e, rt.DataTypeIdentifier(name,true))()
          synE = synE.tail
        }
        case SAnyRef(anyref) => throw new RuntimeException("AnyRefs aren't supported yet: " + anyref + " , "+ synElemList)
        case SType(t) => throw new  RuntimeException("List should't have Types at this position! " + t)
        case SData(t) => throw new RuntimeException("List should't have any Data at this position! " + t)
        case SNat(t) => throw new RuntimeException("List should't have any Nats at this position! " + t)
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
    if(!parseState.mapFkt.isEmpty){
      throw new IllegalArgumentException("map has to be empty: " + parseState.mapFkt)
    }

    var (t, map): (List[Token], MapFkt) = parseState.tokenStream match {
      case BeginTypAnnotatedIdent(_) :: remainderTokens => {
        val ps:ParseState = ParseState(remainderTokens, Nil, parseState.mapFkt, parseState.mapDepL)
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
          val p:ParseState = ParseState(remainderTokens, Nil, map, parseState.mapDepL)
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
          val p:ParseState = ParseState(remainderTokens, Nil, map, parseState.mapDepL)
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

    val (ps, identifierFkt, typeOfFkt) : (ParseState, r.Identifier, r.types.Type) = psLambdaOld match {
      case Right(e) => return Right(e)
      case Left(p) => {
        p.parsedSynElems.head match {
          case SExpr(r.Identifier(n))=>
            p.mapFkt.get(n) match {
              case None => {
                println("Identifier doesn't exist: " + n + " , " + psLambdaOld)
                throw new IllegalStateException("We want to parse an NamedExpr for " + n + " but this Identifier is not declared yet!")
              }
              case Some(Left(e)) => throw new IllegalStateException("The Lambda-Fkt should't be initiated yet!: "+ e)
              case Some(Right(typeFkt)) =>
                (ParseState(p.tokenStream, parseState.parsedSynElems, p.mapFkt, p.mapDepL), r.Identifier(n)(typeFkt), typeFkt)
            }
          case SExpr(expr) => throw new IllegalStateException("it is an Identifier expected: "+ expr)
          case SType(t) => throw new IllegalStateException("it is an Identifier expected but an Type is completely false: "+ t)
          case SAnyRef(anyref) => throw new RuntimeException("AnyRefs aren't supported yet: " + anyref + " , "+ p)
          case SData(t) => throw new RuntimeException("List should't have any Data at this position! " + t)
          case SNat(t) => throw new RuntimeException("List should't have any Nats at this position! " + t)
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
            val m = p.mapFkt
            m.get(identifierFkt.name) match {
              case None => throw new IllegalStateException("Identifier seems not to be in the Map: " + identifierFkt.name + " , " + m)
              case Some(Left(e)) => throw new IllegalStateException("The Lambda-Fkt should't be initiated yet!: "+ e)
              case Some(Right(l)) =>
                //Todo: I have to add Types in the Identifiers in Lambda and delete it after it, after this this if-clause makes sense
//                if(!l.isEmpty){
//                throw new IllegalStateException("The List should be empty! But it isn't. " +
//                  "Probably we have one or more Types in the " +
//                  "TypAnnotationIdent declared than the NamedExpr really has. Types left: " + l + "\nTypes defined: " + typesDefined + "\nNamedExpr: " + p.parsedSynElems)
//              }else{
                //Todo: We have to give the Identifier (identifierFkt/p.map.get(n)) now a Type

                (remainderTokens, p.parsedSynElems, m)
//              }
            }
          }
          case _ => {
            throw new IllegalStateException("NewExpr ends with an EndNamedExpr, but we have no EndNamedExpr at the end")
          }
        }
      }
    }

        val synElemList = psNamedExpr._2
        var expr = combineExpressionsDependent(synElemList)
        expr = expr.setType(typeOfFkt)

        println("expr finished: " + expr + " with type: " + expr.t + "   (should have Type: " + typeOfFkt + " ) ")
        val m = psNamedExpr._3
        m.update(identifierFkt.name, Left(expr))
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
          case SExpr(r.Identifier(n))=> if(p.mapFkt.contains(n)){
            println("Identifier does already exist: " + n + " , " + psLambdaOld)
            throw new IllegalStateException("We want to parse an TypAnnotatedIdent for " + n + " but this Identifier is already declared!")
          }else if(p.mapDepL.contains(n)){
            throw new IllegalArgumentException("We want to parse an TypAnnotatedIdent for " + n + " but it exists already an DepLambda with this Name")
          }else{
            (p, r.Identifier(n)())
          }
          case SExpr(expr) => throw new IllegalStateException("it is an Identifier expected: "+ expr)
          case SType(t) => throw new IllegalStateException("it is an Identifier expected but an Type is completely false: "+ t)
          case SAnyRef(anyref) => throw new RuntimeException("AnyRefs aren't supported yet: " + anyref + " , "+ p)
          case SData(t) => throw new RuntimeException("List should't have any Data at this position! " + t)
          case SNat(t) => throw new RuntimeException("List should't have any Nats at this position! " + t)
        }
      }
    }

    val psNamedExprBefore = {
      Left(ParseState(ps.tokenStream, Nil, ps.mapFkt, ps.mapDepL))        |>
        parseDoubleColons |>
        parseCompleteType
    }


    val psNew = psNamedExprBefore match {
      case Right(e) => return Right(e)
      case Left(p) => p
    }

    println("in the middle of TypAnnotatedIden: "+ psNew)
    psNew.tokenStream match {
      case EndTypAnnotatedIdent(_) :: remainderTokens => {
        val m = psNew.mapFkt
        val typesList: List[r.types.Type] = getTypesInList(psNew.parsedSynElems)
        if(typesList.length!=1){
          throw new IllegalStateException("The TypesList should have lenght 1: "+ typesList)
        }
        m.put(identifierFkt.name, Right(typesList.head))
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

  def getTypesInList(synElems: List[SyntaxElement]): List[r.types.Type]= {
    if( !synElems.isEmpty){
      synElems.head match {
        case SType(typ) => typ :: getTypesInList(synElems.tail)
        case SExpr(e) => throw new IllegalArgumentException("in getTypesInList we have as head a not Type: "+ e)
        case SAnyRef(anyref) => throw new RuntimeException("AnyRefs aren't supported yet: " + anyref + " , "+ synElems)
        case SData(t) => t match {
          case DIdentifier(data) => data :: getTypesInList(synElems.tail)
          case DType(data) => data :: getTypesInList(synElems.tail)
        }
        case SNat(t) => throw new RuntimeException("List should't have any Nats at this position! " + t)
      }
    }else{
      Nil
    }
  }

  def parseType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseType: " + parseState)
    val ps: Either[ParseState, ParseErrorOrState] =
      Left(parseState) |>
        (parseBracesExprType _ || parseDepFunctionType ||
          parseTupleType || parseIndexType || parseArrayType || //parseFunType ||
          parseScalarType || parseData ) //Todo: The Function parseTypeIdentToCorrectForm is not good, because it is not clear what we should parse. I have an Function for parseData, but I don't need a function for parseNat, because Nat should not be returned. For ArrayType i am also unsure.
    ps
  }

  def parseCompleteType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ps = parseType(parseState)
    ps match {
      case Right(e)=> Right(e)
      case Left(p)=> if(p.tokenStream.isEmpty){
        throw new IllegalStateException("Tokens are empty: "+ p)
      }else {
        p.tokenStream.head match {
          case Arrow(_) => {
            val newParse = parseCompleteType(ParseState(p.tokenStream.tail, Nil, p.mapFkt, p.mapDepL)) match{
              case Left(pars) => pars
              case Right(e) => return Right(e)
            }
            val typesList:List[r.types.Type] = combineTypes(getTypesInList(p.parsedSynElems))::
              combineTypes(getTypesInList(newParse.parsedSynElems))::Nil
            val newType: r.types.Type = combineTypes(typesList)
            Left(ParseState(newParse.tokenStream, SType(newType)::Nil, newParse.mapFkt, p.mapDepL))
          }
          //Todo: Maybe remove already here the EndTypAnnotatedIdent or RBrace from the TokenList
          case EndTypAnnotatedIdent(_) => Left(p)
          case RParentheses(_) => Left(p)
          case a => return Right(ParseError("the Token '"+ a + "' is not here expected!!!"))
        }
      }
    }
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
          case Left(p) => parseMaybeAppExpr(ParseState(p.tokenStream,Nil, p.mapFkt, p.mapDepL))
          case Right(e) => {
            println("endLambda: "+ e)
            return Right(e)
          }
        }

    val (toks, synElemList, map, mapDepL) = psOrErr match {
      case Left(psNew) => {
        val expr = SExpr(combineExpressionsDependent(psNew.parsedSynElems))
        val newL = expr :: Nil
        val li:List[SyntaxElement] = psOld match {
          case Left(pa) => pa.parsedSynElems.reverse ++ newL
          case Right(_) => throw new RuntimeException("this should not be able to happen in parseLambdda, because I already have controlled this!")
        }
        val l = li.reverse
        (psNew.tokenStream,l, psNew.mapFkt, psNew.mapDepL)
      }
      case Right(e) => return Right(e)
    }

    val (expr, synElemListExpr) = (synElemList.head match {
      case SExpr(e) => e
      case a => throw new RuntimeException("Here is an Expression expected, but " + a +" ist not an Expression!")
    }, synElemList.tail)
    println("now in Lambda we want to combine our results: "+ expr +" # " + synElemListExpr)

    val (maybeTypedIdent, synElemListMaybeTIdent):(r.Expr, List[SyntaxElement]) =
      synElemListExpr.head match {
        case SType(t) => //Todo: Here ask it t equals the head of the list of map.get(identifier.name) if(t.equals())
          synElemListExpr.tail.head match {
            case SExpr(i) => (i.setType(t), synElemListExpr.tail.tail)
            case a => throw new RuntimeException("Here is an Expression expected, but " + a +" is not an Expression!")
          }
        case SExpr(i) => (i, synElemListExpr.tail)
        case SAnyRef(anyref) => throw new RuntimeException("AnyRefs aren't supported yet: " + anyref + " , "+ synElemListExpr)
        case SData(t) => t match {
          case DIdentifier(data) => synElemListExpr.tail.head match {
            case SExpr(i) => (i.setType(data), synElemListExpr.tail.tail)
            case a => throw new RuntimeException("Here is an Expression expected, but " + a +" is not an Expression!")
          }
          case DType(data) => synElemListExpr.tail.head match {
            case SExpr(i) => (i.setType(data), synElemListExpr.tail.tail)
            case a => throw new RuntimeException("Here is an Expression expected, but " + a +" is not an Expression!")
          }
        }
        case SNat(t) => throw new RuntimeException("List should't have any Nats at this position! " + t)
      }
    val identifierName = maybeTypedIdent.asInstanceOf[r.Identifier]
    val lambda = Lambda(identifierName, expr)()
    println("synElemListMaybeTIdent: " + synElemListMaybeTIdent +" ______ " + synElemListExpr)

    //local variables are in the list, so that not two same localVariables are declared
    if (map.contains(identifierName.name)) {
      throw new IllegalArgumentException("A variable or function with the exact same name '"+ identifierName.name + "' is already declared! <- " + map.get(identifierName.name))
    }
    map.update(identifierName.name, Left(identifierName))

    val myNewParseState = ParseState(toks, SExpr(lambda) :: synElemListMaybeTIdent, map,mapDepL)
    println("myNewParseState: "+ myNewParseState)
    Left(myNewParseState)
  }

  //_________________________________________________________Lambda
  //_________________________________________________________Expres

  def parseNoAppExpr(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
//    if(parseState.tokenStream.head.isInstanceOf[EndNamedExpr]){
//      println("Abbruch; parseExpression: "+ parseState)
//      return Left(parseState)
//    }
    println("parseLowExpression: " + parseState)
    Left(parseState) |>
      (parseLambda _ || parseDepLambda || parseBracesExpr ||
        parseUnOperator || parseBinOperator || parseIdent ||
        parseNumber || parseTypeinNoAppExpr//|| parseDependencies
        )

  }

  def ParseTypesUntilRBracket(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    if(parseState.tokenStream.isEmpty||parseState.tokenStream.head.isInstanceOf[RBracket]){
      println("Abbruch; endlessPossibleParseType: "+ parseState)
      return Left(parseState)
    }
    val p =
      Left(ParseState(parseState.tokenStream,Nil, parseState.mapFkt, parseState.mapDepL))  |>
        parseType |>
        ParseTypesUntilRBracket
    p
  }

  def parseTypeinNoAppExpr(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseTypeinNoAppExpr: "+ parseState)
    if(parseState.tokenStream.isEmpty){
      println("Abbruch; parseTypeinNoAppExpr: "+ parseState)
      return Left(parseState)
    }
    val p =
      Left(ParseState(parseState.tokenStream,Nil, parseState.mapFkt, parseState.mapDepL))  |>
        parseLeftBracket  |>
        ParseTypesUntilRBracket |>
        parseRightBracket

    p
  }

  def parseBracesExpr(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseBracesExpr: "+ parseState)
    if(parseState.tokenStream.isEmpty){
      println("Abbruch; parseBracesExpr: "+ parseState)
      return Left(parseState)
    }
    val p =
      Left(ParseState(parseState.tokenStream,Nil, parseState.mapFkt, parseState.mapDepL))  |>
        parseLeftParentheses  |>
        parseMaybeAppExpr |>
        parseRightParentheses

    p match {
      case Left(pState) => {
        if(pState.parsedSynElems.isEmpty){
          val rBraceIndex = parseState.tokenStream.indexWhere(p=> p.isInstanceOf[RParentheses])
          throw new RuntimeException("There was no Expression in Braces at posstion (" + 0 + " , " + rBraceIndex +
            " : "+ parseState.tokenStream.toString())
        }
        val expr = SExpr(combineExpressionsDependent(pState.parsedSynElems))
        val newL = expr :: Nil
        val li:List[SyntaxElement] = parseState.parsedSynElems.reverse ++ newL
        val l = li.reverse
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.mapFkt, pState.mapDepL)
        Left(newParse)
      }
      case Right(e) => Right(e)
    }
  }

  def parseArrayType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseIndexType: " + parseState)
    val p =
      Left(ParseState(parseState.tokenStream,Nil, parseState.mapFkt, parseState.mapDepL))  |>
        parseNat |>
        parseDot |>
        parseType

    p match {
      case Left(pState) => {
        require(pState.parsedSynElems.length==2, "It should exactly be 1 Nat and one Identifer for IndexType in the list!")
        val ty = pState.parsedSynElems.reverse match {
          case SNat(nat)::SData(data) :: Nil => nat match {
            case NNumber(n) => data match {
              case DIdentifier(d) => SType(rt.ArrayType(n, d))
              case DType(d) => SType(rt.ArrayType(n,d))
            }
            case NIdentifier(n) => data match {
              case DIdentifier(d) => SType(rt.ArrayType(n, d))
              case DType(d) => SType(rt.ArrayType(n,d))
            }
          }
          case SNat(nat)::SType(t) :: Nil => nat match {
            case NNumber(n) => SType(rt.ArrayType(n, t.asInstanceOf[rt.DataType]))
            case NIdentifier(n) => SType(rt.ArrayType(n, t.asInstanceOf[rt.DataType]))
          }
          case a => throw new RuntimeException("List should't have only Nat at this position! " + a)
        }
        val newL = ty :: Nil
        val li:List[SyntaxElement] = parseState.parsedSynElems.reverse ++ newL
        val l = li.reverse
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.mapFkt, pState.mapDepL)
        Left(newParse)
      }
      case Right(e) => Right(e)
    }
  }

  def parseIndexType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseIndexType: " + parseState)
    val p =
      Left(ParseState(parseState.tokenStream,Nil, parseState.mapFkt, parseState.mapDepL))  |>
        parseTypeIdent |>
        parseLeftBracket |>
        parseNat |>
        parseRightBracket

    p match {
      case Left(pState) => {
        require(pState.parsedSynElems.length==2, "It should exactly be 1 Nat and one Identifer for IndexType in the list!")
        val ty = pState.parsedSynElems.reverse match {
          case SType(rt.TypeIdentifier("Idx")) :: SNat(nat)::Nil => nat match {
            case NNumber(nat) => SType(rt.IndexType(nat))
            case NIdentifier(nat) => SType(rt.IndexType(nat))
          }
          case a => throw new RuntimeException("List should't have only Identifier at this position! " + a)
        }
        val newL = ty :: Nil
        val li:List[SyntaxElement] = parseState.parsedSynElems.reverse ++ newL
        val l = li.reverse
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.mapFkt, pState.mapDepL)
        Left(newParse)
      }
      case Right(e) => Right(e)
    }
  }
  def parseTupleType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseTupleType: "+ parseState)
    val p =
      Left(ParseState(parseState.tokenStream,Nil, parseState.mapFkt, parseState.mapDepL))  |>
        parseLeftParentheses  |>
        parseType |>
        parseComma |>
        parseCompleteType |>
        parseRightParentheses

    p match {
      case Left(pState) => {
        if(pState.parsedSynElems.isEmpty){
          val rBraceIndex = parseState.tokenStream.indexWhere(p=> p.isInstanceOf[RParentheses])
          throw new RuntimeException("There was no Expression in Braces at posstion (" + 0 + " , " + rBraceIndex +
            " : "+ parseState.tokenStream.toString())
        }
        val typesList = getTypesInList(pState.parsedSynElems).reverse
        require(typesList.length==2, "It should exactly be two Types for PairType in the list!")
        val ty = SType(rt.PairType(typesList.head.asInstanceOf[rt.DataType], typesList.tail.head.asInstanceOf[rt.DataType]))
        val newL = ty :: Nil
        val li:List[SyntaxElement] = parseState.parsedSynElems.reverse ++ newL
        val l = li.reverse
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.mapFkt, pState.mapDepL)
        Left(newParse)
      }
      case Right(e) => Right(e)
    }
  }

  def parseBracesExprType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseBracesExprType: "+ parseState)
    val p =
      Left(ParseState(parseState.tokenStream,Nil, parseState.mapFkt, parseState.mapDepL))  |>
        parseLeftParentheses  |>
        parseCompleteType |>
        parseRightParentheses

    p match {
      case Left(pState) => {
        if(pState.parsedSynElems.isEmpty){
          val rBraceIndex = parseState.tokenStream.indexWhere(p=> p.isInstanceOf[RParentheses])
          throw new RuntimeException("There was no Expression in Braces at posstion (" + 0 + " , " + rBraceIndex +
            " : "+ parseState.tokenStream.toString())
        }
        val ty = SType(combineTypes(getTypesInList(pState.parsedSynElems)))
        val newL = ty :: Nil
        val li:List[SyntaxElement] = parseState.parsedSynElems.reverse ++ newL
        val l = li.reverse
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.mapFkt, pState.mapDepL)
        Left(newParse)
      }
      case Right(e) => Right(e)
    }
  }

  def parseMaybeAppExpr(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseMaybeAppExpr: " + parseState)
    if(parseState.tokenStream.head.isInstanceOf[RParentheses]){
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
    val ParseState(tokens, parsedExprs, map, mapDepL) = parseState
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

    Left(ParseState(remainderTokens, parsedExprs, map, mapDepL))
  }

  def parseDoubleColons(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedExprs, map, mapDepL) = parseState
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

    Left(ParseState(remainderTokens, parsedExprs, map, mapDepL))
  }


  def parseUnOperator(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val nextToken :: remainderTokens = parseState.tokenStream

    val p = nextToken match {
      case UnOp(un, _) => un match {
        case OpType.UnaryOpType.NEG => Left(ParseState(remainderTokens, SExpr(r.primitives.Neg()()) :: parseState.parsedSynElems, parseState.mapFkt, parseState.mapDepL))
        case OpType.UnaryOpType.NOT => Left(ParseState(remainderTokens, SExpr(r.primitives.Not()()) :: parseState.parsedSynElems, parseState.mapFkt, parseState.mapDepL))
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
    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
    if(tokens.isEmpty){
      return Right(ParseError("failed to parse Number: " + " List is empty"))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case I8(number, _) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.IntData(number))) :: parsedSynElems, map, mapDepL))
      case I32(number, _) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.IntData(number))) :: parsedSynElems, map, mapDepL))
      case F32(number, _) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.FloatData(number))) :: parsedSynElems, map, mapDepL))
      case F64(number, _) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.DoubleData(number))) :: parsedSynElems, map, mapDepL))
      case tok => Right(ParseError("failed to parse Number: " + tok + " is not an accepted Integer of Float"))
    }
  }


  def parseBinOperator(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseBinOperator: "+ parseState)
    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case BinOp(op, _) => op match {
        case OpType.BinOpType.ADD =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.Add()()) :: parsedSynElems, map, mapDepL))
        case OpType.BinOpType.DIV =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.Div()()) :: parsedSynElems, map, mapDepL))
        case OpType.BinOpType.EQ =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.Equal()()) :: parsedSynElems, map, mapDepL))
        case OpType.BinOpType.GT =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.Gt()()) :: parsedSynElems, map, mapDepL))
        case OpType.BinOpType.LT =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.Lt()()) :: parsedSynElems, map, mapDepL))
        case OpType.BinOpType.MOD => Left(ParseState(remainderTokens, SExpr(r.primitives.Mod()()) :: parsedSynElems, map, mapDepL))
        case OpType.BinOpType.MUL => Left(ParseState(remainderTokens, SExpr(r.primitives.Mul()()) :: parsedSynElems, map, mapDepL))
        case OpType.BinOpType.SUB => Left(ParseState(remainderTokens, SExpr(r.primitives.Sub()()) :: parsedSynElems, map, mapDepL))
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


  def parseComma(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseComma: " + parseState)
    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Comma(_) => Left(ParseState(remainderTokens, parsedSynElems, map, mapDepL))
      case tok => Right(ParseError("failed to parse Comma: " + tok + " is not an Comma"))
    }
  }



  def parseDepArrow(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case DepArrow(_) => Left(ParseState(remainderTokens, parsedSynElems, map, mapDepL))
      case tok => Right(ParseError("failed to parse DepArrow: " + tok + " is not an DepArrow"))
    }
  }

  def parseColon(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Colon(_) => Left(ParseState(remainderTokens, parsedSynElems, map, mapDepL))
      case tok => Right(ParseError("failed to parse Colon: " + tok + " is not an Colon"))
    }
  }

  def parseDot(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
    if(tokens.isEmpty||tokens.length<=1){
      return Right(ParseError("failed to parse Dot, because List is empty"))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Dot(_) => Left(ParseState(remainderTokens, parsedSynElems, map, mapDepL))
      case tok => Right(ParseError("failed to parse Dot: " + tok + " is not an Dot"))
    }
  }

  def parseNat(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case NatNumber(number, _) => Left(ParseState(remainderTokens, SNat(NNumber(number:r.Nat))::parsedSynElems, map, mapDepL))
      case TypeIdentifier(name, _) =>Left(ParseState(remainderTokens, SNat(NIdentifier(rt.NatIdentifier(name)))::parsedSynElems, map, mapDepL))
      case tok => Right(ParseError("failed to parse Nat: " + tok + " is not an Nat"))
    }
  }

  def parseData(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
        //Todo: I have to accept any explicit type too and because of that, I have to parseType extra here too!!!!
      case TypeIdentifier(name, _) =>Left(ParseState(remainderTokens, SData(DIdentifier(rt.DataTypeIdentifier(name)))::parsedSynElems, map, mapDepL))
      case _ => {
        parseType(ParseState(tokens, Nil, map, mapDepL)) match {
          case Right(e) => Right(e)
          case Left(p) => {
            if(p.parsedSynElems.length!=1){
              throw new IllegalStateException("It has to be 1 elemnt long in parseData: "+ p.parsedSynElems)
            }
            p.parsedSynElems.head match {
              case SType(t) => Left(ParseState(p.tokenStream, SData(DType(t.asInstanceOf[rt.DataType]))::parsedSynElems, map, mapDepL))
              case a => throw new IllegalStateException("Only Type is expected in parseData: " + a)
            }
          }
        }
      }
    }
  }

  def parseLeftBracket(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
    if(tokens.isEmpty||tokens.length<=1){
      return Right(ParseError("failed to parse LeftBracket, because List is empty"))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case LBracket(_) => Left(ParseState(remainderTokens, parsedSynElems, map, mapDepL))
      case tok => Right(ParseError("failed to parse LeftBracket: " + tok + " is not an LeftBracket"))
    }
  }

  def parseRightBracket(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case RBracket(_) => Left(ParseState(remainderTokens, parsedSynElems, map, mapDepL))
      case tok => Right(ParseError("failed to parse RightBracket: " + tok + " is not an RightBracket"))
    }
  }

  def parseLeftParentheses(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case LParentheses(_) => Left(ParseState(remainderTokens, parsedSynElems, map, mapDepL))
      case tok => Right(ParseError("failed to parse LeftParentheses: " + tok + " is not an LeftParentheses"))
    }
  }

  def parseRightParentheses(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL) = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case RParentheses(_) => Left(ParseState(remainderTokens, parsedSynElems, map, mapDepL))
      case tok => Right(ParseError("failed to parse RightParentheses: " + tok + " is not an RightParentheses"))
    }
  }


  //_________________________________________________________Expres
}