package parser //old branch 17. Dezember 2020

import rise.{core => r, openCL => o}
import r.{DSL => rd, primitives => rp, semantics => rS, types => rt}
import o.{primitives => op}
import rise.core.DSL.ToBeTyped
import rise.core.DSL.Type.TypeConstructors
import rise.core.ForeignFunction
import rise.core.types.{PairType, f32, vec}

import scala.collection.mutable

object parse {

  val SpanPlaceholder = Span(FileReader("src/test/scala/parser/readFiles/Placeholder.rise"), Location(0,0), Location(0,0))

  /*
   * Precondition: Only valid Tokens in tokenList.
   */
  def apply(tokenList: List[Token]): MapFkt = {
    val parseState: ParseState = ParseState(tokenList, Nil, None, None)
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

  final case class NNumber(nat: rt.Nat) extends NatElement

  final case class NIdentifier(nat: rt.NatIdentifier) extends NatElement

  sealed trait DataElement

  final case class DIdentifier(data: rt.DataTypeIdentifier) extends DataElement

  final case class DType(data: rt.DataType) extends DataElement

  sealed trait SIntToExprAlternatives

  final case class AltMapGlobal() extends SIntToExprAlternatives

  final case class AltMapLocal() extends SIntToExprAlternatives

  final case class AltMapWorkGroup() extends SIntToExprAlternatives

  final case class AltMakeArray() extends SIntToExprAlternatives

  sealed trait SyntaxElement

  final case class SExprClutched(expr: r.Expr, spanClutch: Span) extends SyntaxElement

  final case class SExpr(expr: r.Expr) extends SyntaxElement

  final case class SLet(span: Span) extends SyntaxElement

  final case class SType(t: rt.Type, span: Span) extends SyntaxElement

  final case class SIntToExpr(name: SIntToExprAlternatives, span: Span) extends SyntaxElement

  final case class SNat(nat: NatElement, span: Span) extends SyntaxElement

  final case class SData(data: DataElement, span: Span) extends SyntaxElement

  final case class SAddrSpace(addrSpace: rt.AddressSpace, span: Span) extends SyntaxElement

  sealed trait RiseKind //Todo: Scoping einbauen, also Kind nennen und Token explizit immer hinzufügen

  final case class RData() extends RiseKind

  final case class RNat() extends RiseKind

  final case class RAddrSpace() extends RiseKind

  //Todo: if I have Identifier, I have to get the right Span and the Span is differntly each time
  type MapFkt = mutable.HashMap[String, Either[rd.ToBeTyped[r.Expr], r.types.Type]]
  type MapDepL = mutable.HashMap[String, RiseKind]
  type BracesSpan = Option[List[Span]]
  type OutputEPState = (Either[ParseState, PreAndErrorSynElems], ErrorList)
  type InputEPState = (ParseState, ErrorList)

  final case class ParseState(tokenStream: List[Token], parsedSynElems: List[SyntaxElement],
                              mapDepL: Option[MapDepL], spanList: BracesSpan)

  implicit class ParseStatePipe(val OutputEPState: OutputEPState) extends AnyVal {
    def |>(f: InputEPState => OutputEPState): OutputEPState = {
      val (ps, errorList) = OutputEPState
      //println("|> : " + ps)
      ps match {
        case Left(p) => f((p,errorList))
        case Right(e) => (Right(e),errorList.add(e))
      }
    }
  }

  implicit class ParseStateElse(val leftF: InputEPState => OutputEPState) extends AnyVal {
    def ||(
            rightF: InputEPState => OutputEPState
          ): InputEPState => OutputEPState = {
      oep =>
        val (ps, errorList)=oep
        leftF((ps,errorList)) match {
          case (Right(e),newEL) => {
            //println("|| : " + ps)
            rightF((ps,newEL.add(e)))
          }
          case (Left(resPs),newEL) => (Left(resPs),newEL)
        }
    }
  }


  //_________________________________________________________Lambda


  def parseBackslash(iep:InputEPState): OutputEPState = {
    val (parseState,errorList) = iep
    val ParseState(tokens, parsedExprs, mapDepL, spanList) = parseState
    if (tokens.isEmpty) {
      return (Right(TokListIsEmpty(SpanPlaceholder, "Backslash")),errorList)
    }
    val nextToken :: remainderTokens = tokens

    val spanL = nextToken match {
      case Backslash(span) => spanList match {
        case None => span :: Nil
        case Some(l) => span :: l
      }
      case tok => {
        println("failed parseBacklash: " + parseState)
        return (Right(NotCorrectToken(tok, "Backslash", "Backslash")),errorList)
      }
    }

    (Left(ParseState(remainderTokens, parsedExprs,mapDepL, Some(spanL))),errorList)
  }


  def matchPrimitiveOrIdentifier(name: String, span: Span): SyntaxElement = {
    require(name.matches("[a-z][a-zA-Z0-9_]*"), "'" + name + "' has not the preffered structure")
    //Todo: delete calcAcc and update, because they are only for testing added. These foreign function should be declared in the rise-File itself
    val calcAcc = ForeignFunction(
      ForeignFunction.Decl(name, Some(ForeignFunction.Def(Seq("p1", "p2", "deltaT", "espSqr", "acc"),
        """{
          |  float4 r;
          |  r.xyz = p2.xyz - p1.xyz;
          |  float distSqr = r.x*r.x + r.y*r.y + r.z*r.z;
          |  float invDist = 1.0f / sqrt(distSqr + espSqr);
          |  float invDistCube = invDist * invDist * invDist;
          |  float s = invDistCube * p2.w;
          |  float4 res;
          |  res.xyz = acc.xyz + s * r.xyz;
          |  return res;
          |}
          |""".stripMargin)))
    )(vec(4, f32) ->: vec(4, f32) ->: f32 ->: f32 ->: vec(4, f32) ->: vec(4, f32), Some(span))
    val update = ForeignFunction(
      ForeignFunction.Decl(name, Some(ForeignFunction.Def(Seq("pos", "vel", "deltaT", "acceleration"),
        """{
          |  float4 newPos;
          |  newPos.xyz = pos.xyz + vel.xyz * deltaT + 0.5f * acceleration.xyz * deltaT * deltaT;
          |  newPos.w = pos.w;
          |  float4 newVel;
          |  newVel.xyz = vel.xyz + acceleration.xyz * deltaT;
          |  newVel.w = vel.w;
          |  return (struct Record_float4_float4){ newPos, newVel };
          |}""".stripMargin)))
    )(vec(4, f32) ->: vec(4, f32) ->: f32 ->: vec(4, f32) ->: PairType(vec(4, f32), vec(4, f32)), Some(span))
    name match {
      //Todo: this functions please delete, because they should be declared via CFunction in the rise-File
      case "update" => SExpr(update)
      case "calcAcc" => SExpr(calcAcc)

      //openCL/primitives
      case "mapGlobal" => SIntToExpr(AltMapGlobal(), span)
      case "mapLocal" => SIntToExpr(AltMapLocal(), span)
      case "mapWorkGroup" => SIntToExpr(AltMapWorkGroup(), span)
      case "oclReduceSeq" => SExpr(op.oclReduceSeq(Some(span)))
      case "oclReduceSeqUnroll" => SExpr(op.oclReduceSeqUnroll(Some(span)))
      case "oclIterate" => SExpr(op.oclIterate(Some(span)))
      case "oclCircularBuffer" => SExpr(op.oclCircularBuffer(Some(span)))
      case "oclRotateValues" => SExpr(op.oclRotateValues(Some(span)))

      //openCL/TypedDSL //Todo: not sure if this with .toExpr is working. Test with nBody
      case "toGlobal" => SExpr(r.DepApp[rt.AddressSpaceKind](op.oclToMem(Some(span)),
        rt.AddressSpace.Global
      )(rt.TypePlaceholder, Some(span)))
      case "toLocal" => SExpr(r.DepApp[rt.AddressSpaceKind](op.oclToMem(Some(span)),
        rt.AddressSpace.Local
      )(rt.TypePlaceholder, Some(span)))
      case "toPrivate" => SExpr(r.DepApp[rt.AddressSpaceKind](op.oclToMem(Some(span)),
        rt.AddressSpace.Private
      )(rt.TypePlaceholder, Some(span)))

      //core/primitives
      case "makeArray" => SIntToExpr(AltMakeArray(), span)
      case "cast" => SExpr(rp.cast(Some(span)))
      case "depJoin" => SExpr(rp.depJoin(Some(span)))
      case "depMapSeq" => SExpr(rp.depMapSeq(Some(span)))
      case "depZip" => SExpr(rp.depZip(Some(span)))
      case "drop" => SExpr(rp.drop(Some(span)))
      case "fst" => SExpr(rp.fst(Some(span)))
      case "gather" => SExpr(rp.gather(Some(span)))
      case "generate" => SExpr(rp.generate(Some(span)))
      case "idx" => SExpr(rp.idx(Some(span)))
      case "id" => SExpr(rp.id(Some(span)))
      case "indexAsNat" => SExpr(rp.indexAsNat(Some(span)))
      case "iterate" => SExpr(rp.iterate(Some(span)))
      case "join" => SExpr(rp.join(Some(span)))
      case "concat" => SExpr(rp.concat(Some(span)))
      case "let" => SLet(span)
      case "map" => SExpr(rp.map(Some(span)))
      case "mapFst" => SExpr(rp.mapFst(Some(span)))
      case "mapSnd" => SExpr(rp.mapSnd(Some(span)))
      case "mapSeq" => SExpr(rp.mapSeq(Some(span)))
      case "mapStream" => SExpr(rp.mapStream(Some(span)))
      case "iterateStream" => SExpr(rp.iterateStream(Some(span)))
      case "mapSeqUnroll" => SExpr(rp.mapSeqUnroll(Some(span)))
      case "toMem" => SExpr(rp.toMem(Some(span)))
      case "natAsIndex" => SExpr(rp.natAsIndex(Some(span)))
      case "padEmpty" => SExpr(rp.padEmpty(Some(span)))
      case "padClamp" => SExpr(rp.padClamp(Some(span)))
      case "partition" => SExpr(rp.partition(Some(span)))
      case "makePair" => SExpr(rp.makePair(Some(span)))
      case "reduce" => SExpr(rp.reduce(Some(span)))
      case "reduceSeq" => SExpr(rp.reduceSeq(Some(span)))
      case "reduceSeqUnroll" => SExpr(rp.reduceSeqUnroll(Some(span)))
      case "reorder" => SExpr(rp.reorder(Some(span)))
      case "scanSeq" => SExpr(rp.scanSeq(Some(span)))
      case "slide" => SExpr(rp.slide(Some(span)))
      case "circularBuffer" => SExpr(rp.circularBuffer(Some(span)))
      case "rotateValues" => SExpr(rp.rotateValues(Some(span)))
      case "snd" => SExpr(rp.snd(Some(span)))
      case "split" => SExpr(rp.split(Some(span)))
      case "take" => SExpr(rp.take(Some(span)))
      case "transpose" => SExpr(rp.transpose(Some(span)))
      case "select" => SExpr(rp.select(Some(span)))
      case "zip" => SExpr(rp.zip(Some(span)))
      case "unzip" => SExpr(rp.unzip(Some(span)))
      case "neg" => SExpr(rp.neg(Some(span)))
      case "not" => SExpr(rp.not(Some(span)))
      case "add" => SExpr(rp.add(Some(span)))
      case "sub" => SExpr(rp.sub(Some(span)))
      case "mul" => SExpr(rp.mul(Some(span)))
      case "div" => SExpr(rp.div(Some(span)))
      case "mod" => SExpr(rp.mod(Some(span)))
      case "gt" => SExpr(rp.gt(Some(span)))
      case "lt" => SExpr(rp.lt(Some(span)))
      case "equal" => SExpr(rp.equal(Some(span)))
      case "asVectorAligned" => SExpr(rp.asVectorAligned(Some(span)))
      case "asVector" => SExpr(rp.asVector(Some(span)))
      case "asScalar" => SExpr(rp.asScalar(Some(span)))
      case "vectorFromScalar" => SExpr(rp.vectorFromScalar(Some(span)))
      case "printType" => SExpr(rp.printType("", Some(span)).primitive) //Todo: I was forced to delete span in printType and typeHole because of the error with wrong number of arguments
      case "typeHole" => SExpr(rp.typeHole("", Some(span)).primitive)
      case _ => SExpr(r.Identifier(name)(rt.TypePlaceholder, Some(span)))
    }
  }

  private def getIdInIdentNoDec(map: MapFkt, name: String, span: Span): SyntaxElement = {
    val id = map.get(name) match {
      case None => throw new IllegalArgumentException("Variable is not declared: " + name + " in " + span)
      case Some(Right(_)) => throw new IllegalArgumentException("Variable is only declared but has no definition: " + name + " in " + span) //Todo: Proper Error for Function has declaration but not definition yet
      case Some(Left(rd.ToBeTyped(e))) => e match {
        case r.Identifier(_) => SExpr(r.Identifier(name)(rt.TypePlaceholder, Some(span)))
        case _ => SExpr(e)
      }
    }
    id
  }

  private def getIdInIdentWithDec(map: MapFkt, name: String, span: Span): SyntaxElement = {
    val id = map.get(name) match {
      case None => SExpr(r.Identifier(name)(rt.TypePlaceholder, Some(span)))
      case Some(Right(_)) => SExpr(r.Identifier(name)(rt.TypePlaceholder, Some(span))) //throw new IllegalArgumentException("Variable is only declared but has no definition: "+ name)//Todo: Proper Error for Function has declaration but not definition yet
      case Some(Left(rd.ToBeTyped(e))) => e match {
        case r.Identifier(_) => SExpr(r.Identifier(name)(rt.TypePlaceholder, Some(span)))
        case _ => throw new IllegalArgumentException("The name already exists with an definition: " + name + " in " + span)
      }
    }
    id
  }

  def parseIdent(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val ParseState(tokens, parsedSynElems, mapDepL, spanList) = parseState
    if (tokens.isEmpty) {
      return (Right(TokListIsEmpty(SpanPlaceholder, "Ident")),errorList)
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Identifier(name, span) => {
        matchPrimitiveOrIdentifier(name, span) match {
          case SLet(span) => (Left(ParseState(remainderTokens, SLet(span) :: parsedSynElems,
            mapDepL, spanList)),errorList)
          case SIntToExpr(name, span) => (Left(ParseState(remainderTokens, SIntToExpr(name, span) :: parsedSynElems,
            mapDepL, spanList)),errorList)
          case SExpr(r.Identifier(_)) => {
            val ident = SExpr(r.Identifier(name)(rt.TypePlaceholder, Some(span)))
            (Left(ParseState(remainderTokens, ident :: parsedSynElems,mapDepL, spanList)),errorList)
          }
          case SExpr(prim) => {
            if (prim.span.isEmpty) {
              throw new IllegalStateException("The span of '" + prim + "' is empty")
            }
            (Left(ParseState(remainderTokens, SExpr(prim) :: parsedSynElems,mapDepL, spanList)),errorList)
          }
          case otherSyntaxElement => throw new IllegalStateException("The Syntax Element '" + otherSyntaxElement +
            "' was not expected from matchPrimitiveOrIdentifer")
        }
      }
      case tok => {
        println("Abbruch parseIdent: " + tok + " : " + parseState)
        (Right(NotCorrectToken(tok, "Ident", "Ident")),errorList)
      }
    }
  }

  def parseTypeIdentToCorrectForm(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    println("parseTypeIdentToCorrectForm: " + parseState)
    parseState.tokenStream.head match {
      case TypeIdentifier(name, span) => parseState.mapDepL.get.get(name) match {
        case None => (Right(NoKindWithThisName(TypeIdentifier(name, span), "TypeIdent")),errorList)
        case Some(RNat()) =>
          (Left(ParseState(parseState.tokenStream.tail, SNat(NIdentifier(
            rt.NatIdentifier(name)), span) :: parseState.parsedSynElems, parseState.mapDepL, parseState.spanList)),errorList)
        case Some(RData()) =>
          (Left(ParseState(parseState.tokenStream.tail, SData(DIdentifier(
            rt.DataTypeIdentifier(name)), span) :: parseState.parsedSynElems, 
            parseState.mapDepL, parseState.spanList)),errorList)
        case Some(RAddrSpace()) => throw new IllegalStateException("DepAddrSpace is not implemented yet")
      }
      case t => (Right(NotCorrectToken(t, "TypeIdent", "TypeIdent")),errorList)
    }
  }

  def parseTypeIdent(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    println("parseTypeIdent: " + parseState)
    val ParseState(tokens, parsedSynElems, mapDepL, spanList) = parseState
    if (tokens.isEmpty) {
      return (Right(TokListIsEmpty(SpanPlaceholder, "TypeIdent")),errorList)
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case TypeIdentifier(name, spanId) => (Left(ParseState(remainderTokens, SType(rt.TypeIdentifier(name), spanId) :: parsedSynElems,
       mapDepL, spanList)),errorList)
      case tok => {
        println("Abbruch parseTypeIdent: " + tok + " : " + parseState)
        (Right(NotCorrectToken(tok, "TypeIdent", "TypeIdent")),errorList)
      }
    }
  }

  private def getScalarType(typ: ConcreteType): Option[rt.DataType] = typ match {
    case ShortTyp() => Some(rt.i8)
    case IntTyp() => Some(rt.i32)
    case FloatTyp() => Some(rt.f32)
    case DoubleType() => Some(rt.f64)
    case BoolType() => Some(rt.bool)
    case notAtype => {
      println("                        This is not an accepted Type: " + notAtype)
      None
    }
  }

  def parseMaybeTypeAnnotation(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val ParseState(tokens, parsedSynElems,  mapDepL, spanList) = parseState
    val colonToken :: typeTokenWithremainderTokens = tokens

    colonToken match {
      case Colon(_) => {
        //if a type Annotation exist, we set the type new of the Identifier
        parseType((ParseState(typeTokenWithremainderTokens, Nil, mapDepL, spanList), errorList)) match {
          case (Right(e),errorL) => (Right(e),errorL)
          case (Left(newPS),errorL) => if (newPS.parsedSynElems.length == 1) {
            return (Left(ParseState(newPS.tokenStream, newPS.parsedSynElems.head :: parsedSynElems,
              newPS.mapDepL, spanList)),errorL)
          } else {
            throw new IllegalStateException(
              "It should only be one argument in the end of the computing of the Type exist")
          }
        }
      }
      case _ => (Left(parseState),errorList)
    }
  }

  def parseTypeAnnotation(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val ParseState(tokens, _,  mapDepL, spanList) = parseState
    val colonToken :: typeToken :: remainderTokens = tokens

    colonToken match {
      case Colon(_) => {
        //if a type Annotation exist, we set the type new of the Identifier
        typeToken match {
          case ScalarType(typ, sp) => {
            //Todo: Complex Alg for Types Parsing
            val t = getScalarType(typ)
            t match {
              case None => (Right(NotAcceptedScalarType(sp, typ,"TypeAnnotation")),errorList)
              case Some(parsedType) => (Left(ParseState(remainderTokens, SType(parsedType, sp) :: parseState.parsedSynElems,
                 mapDepL, spanList)),errorList)
            }
          }
          case notAtype => (Right(NotCorrectToken(typeToken, "ScalarType", "TypeAnnotation")),errorList)
        }
      }
      case notAColon => (Right(NotCorrectToken(notAColon, "Colon", "TypeAnnotation")),errorList)
    }
  }

  def parseVecType(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val ParseState(tokens, _, mapDepL, spanList) = parseState
    val inputType :: remainderTokens = tokens

    println("parseVecType: " + parseState)
    inputType match {
      case VectorType(len, concreteType, sp) => {
        println("ConcreteType was in parseVecType: " + concreteType + " , with length: " + len)
        //Todo: Complex Alg for Type Parsing
        val parsedInType = getScalarType(concreteType)
        val inT = parsedInType.getOrElse(return (Right(NotAcceptedScalarType(sp, concreteType,"VecType")),errorList))
        (Left(ParseState(remainderTokens, SType(rt.VectorType(len, inT), sp) :: parseState.parsedSynElems,
          mapDepL, spanList)),errorList)
      }
      case notAtype => (Right(NotCorrectToken(notAtype, "VectorType", "VecType")),errorList)
    }
  }

  def parseScalarType(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val ParseState(tokens, _, mapDepL, spanList) = parseState
    val inputType :: remainderTokens = tokens

    println("parseTypeWithoutArrow: " + parseState)
    inputType match {
      case ScalarType(typ, spanTyp) => {
        println("Type was in parseTypeWithoutArrow parsed: " + typ + " ; " + spanTyp.end)
        //Todo: Complex Alg for Type Parsing
        val parsedInType = getScalarType(typ)
        val inT = parsedInType.getOrElse(return (Right(NotAcceptedScalarType(spanTyp, typ,"ScalarType")), errorList))
        (Left(ParseState(remainderTokens, SType(inT, spanTyp) :: parseState.parsedSynElems,  mapDepL, spanList)), errorList)
      }
      case notAtype => (Right(NotCorrectToken(notAtype, "ScalarType", "ScalarType")), errorList)
    }
  }

  def parseKindWithDepArrowAfterForDepLambda(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    println("parseKind: " + parseState)
    val (nameOfIdentifier, spanType, spanList) = parseState.parsedSynElems.head match {
      case SType(rt.TypeIdentifier(name), sp) => parseState.spanList match {
        case Some(Nil) => throw new IllegalArgumentException("this should not happen to be Nil")
        case None => throw new IllegalArgumentException("this should not happen to be None")
        case Some(span :: Nil) => (name, sp + span, None)
        case Some(span :: l) => (name, sp + span, Some(l))
      }
      case t => throw new IllegalStateException("Here should be an TypeIdentifier and not '" + t + "'") //Todo: make PreAndErrorSynElem and test for this
    }
    val newPS = parseState.parsedSynElems.tail

    val tokens = parseState.tokenStream
    tokens.head match {
      case Kind(concreteKind, sp) => tokens.tail.head match {
        case DepArrow(_) => {
          val ki = concreteKind match { //Todo: einfach Span direkt reingeben!!! Auch bei Lambda DepLambda etc.
            case Data() => RData()
            case Nat() => RNat()
            case AddrSpace() => RAddrSpace()
            case ki => return (Right(NotCorrectKind(sp, ki, "KindWithDepArrow")), errorList)
          }
          parseState.mapDepL match {
            case None => throw new IllegalStateException("mapDepL is None")
            case Some(mL)=> mL.update(nameOfIdentifier, ki)
          }
          println("Kind was in parseDepFunctionType parsed: " + concreteKind + " , mapDepL: "+ parseState.mapDepL)
          parseMaybeAppExpr((ParseState(tokens.tail.tail, Nil, parseState.mapDepL, spanList), errorList)) match {
            case (Right(e),errorL) => (Right(e),errorL)
            case (Left(pS),errorL) => {
              println("In the middle of parseDepFunctionType: " + pS)
              if (pS.parsedSynElems.tail.nonEmpty) throw new IllegalStateException("ParsedSynElems.tail has to be empty!") //Todo: write test
              val depLam = pS.parsedSynElems.head match {
                case SExpr(outT) => {
                  val span = outT.span match {
                    case None => throw new IllegalStateException("Span should not be None in DepLambdafkt")
                    case Some(sp) => {
                      val spanZW = spanType + sp
                      println("Span of spanType123: (" + spanZW.begin + "," + spanZW.end + ") = (" + sp.begin + "," + sp.end + ") + (" + spanType.begin + ")" + spanType.end + ") ; " + nameOfIdentifier)
                      spanZW
                    }
                  }
                  ki match { //Todo: einfach Span direkt reingeben!!! Auch bei Lambda DepLambda etc.
                    case RData() => SExpr(r.DepLambda[rt.DataKind](rt.DataTypeIdentifier(nameOfIdentifier),
                      outT)(rt.TypePlaceholder, Some(span)))
                    case RNat() => SExpr(r.DepLambda[rt.NatKind](rt.NatIdentifier(nameOfIdentifier),
                      outT)(rt.TypePlaceholder, Some(span)))
                    case RAddrSpace() => SExpr(r.DepLambda[rt.AddressSpaceKind](
                      rt.AddressSpaceIdentifier(nameOfIdentifier), outT)(rt.TypePlaceholder, Some(span)))
                  }
                }
                case wrongSynElem => return (Right(NotCorrectSynElem(wrongSynElem, "SExpr", "KindWithDepArrow")),errorL)
              }

              (Left(ParseState(pS.tokenStream, depLam :: newPS, pS.mapDepL , spanList)),errorL)
            }
          }
        }
        case notAnDepArrow => (Right(NotCorrectToken(notAnDepArrow, "DepArrow", "KindWithDepArrow")),errorList)
      }
      case t => (Right(NotCorrectToken(t, "Kind", "KindWithDepArrow")),errorList)
    }
  }

//Todo: combine with function above
  def parseKindWithDepArrowAfterForDepFunctionType(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    println("parseKind: " + parseState)
    val nameOfIdentifier: String = parseState.parsedSynElems.head match {
      case SType(rt.TypeIdentifier(name), _) => name
      case t => throw new IllegalStateException("Here should be an TypeIdentifier and not '" + t + "'")
    }
    val newPS = parseState.parsedSynElems.tail

    val tokens = parseState.tokenStream
    tokens.head match {
      case Kind(concreteKind, span) => tokens.tail.head match {
        case DepArrow(_) => {
          if (parseState.mapDepL.contains(nameOfIdentifier)) {
            throw new IllegalArgumentException("It exists already an DepLambda with this Name: " + nameOfIdentifier)
          }
          concreteKind match {
            case Data() => parseState.mapDepL match {
              case None => throw new IllegalStateException("mapDepL is None")
              case Some(mL)=> mL.update(nameOfIdentifier, RData())
            }
            case Nat() => parseState.mapDepL match {
              case None => throw new IllegalStateException("mapDepL is None")
              case Some(mL)=> mL.update(nameOfIdentifier, RNat())
            }
            case AddrSpace() => parseState.mapDepL match {
              case None => throw new IllegalStateException("mapDepL is None")
              case Some(mL)=> mL.update(nameOfIdentifier, RAddrSpace())
            }
            case ki => return (Right(NotCorrectKind(span, ki, "KindWithDepArrow")),errorList)
          }
          println("Kind was in parseDepFunctionType parsed: " + concreteKind)
          parseType((ParseState(tokens.tail.tail, Nil,  parseState.mapDepL, parseState.spanList),errorList)) match {
            case (Right(e),errorL) => (Right(e),errorL)
            case (Left(pS),errorL) => {
              if (pS.parsedSynElems.tail.nonEmpty) throw new IllegalStateException("ParsedSynElems.tail has to be empty!") //Todo: write test
              val depFun: SType = pS.parsedSynElems.head match {
                case SType(outT, sp) => {
                  concreteKind match {
                    case Data() => SType(rt.DepFunType[rt.DataKind, rt.Type](
                      rt.DataTypeIdentifier(nameOfIdentifier), outT), sp)
                    case Nat() => SType(rt.DepFunType[rt.NatKind, rt.Type](
                      rt.NatIdentifier(nameOfIdentifier), outT), sp)
                    case AddrSpace() => SType(rt.DepFunType[rt.AddressSpaceKind, rt.Type](
                      rt.AddressSpaceIdentifier(nameOfIdentifier), outT), sp)
                    case ki => return (Right(NotCorrectKind(sp, ki, "KindWithDepArrow")),errorL)
                  }
                }
                case wrongSynEl => return (Right(NotCorrectSynElem(wrongSynEl, "SType", "KindWithDepArrow")),errorL)
              }
              (Left(ParseState(pS.tokenStream, depFun :: newPS,  pS.mapDepL, pS.spanList)),errorL)
            }
          }
        }
        case notAnDepArrow => (Right(NotCorrectToken(notAnDepArrow, "DepArrow", "KindWithDepArrow")),errorList)
      }
      case t => (Right(NotCorrectToken(t, "Kind", "KindWithDepArrow")),errorList)
    }
  }


  private def isMinLen(l:List[Token], minLen: Int, whatToParse:String):Option[PreAndErrorSynElems]={
    if (l.length < minLen) {
      if(l.isEmpty){
        return Some(TokListIsEmpty(SpanPlaceholder, whatToParse))
      }
      return Some(TokListTooSmall(l, minLen, whatToParse))
    }
    return None
  }

  def parseDepFunctionType(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    println("parseDepFunctionType: " + parseState)
    val ParseState(tokens, _,  _, _) = parseState

    isMinLen(tokens, 3, "DepFunctionType") match{
      case Some(e) => return (Right(e),errorList)
      case None => None
    }

    val psOld =
      (Left(parseState),errorList) |>
        parseTypeIdent |>
        parseColon |>
        parseKindWithDepArrowAfterForDepFunctionType
    psOld
  }

  def parseDepLambda(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    println("parseDepFunctionType: " + parseState)
    val ParseState(tokens, _,  _, _) = parseState
    isMinLen(tokens, 3, "DepLambda") match{
      case Some(e) => return (Right(e),errorList)
      case None => None
    }

    val psOld =
      (Left(parseState),errorList) |>
        parseBackslash |>
        parseTypeIdent |>
        parseColon |>
        parseKindWithDepArrowAfterForDepLambda
    psOld
  }

  def parseArrow(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val ParseState(tokens, parsedExprs, mapDepL, spanList) = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Arrow(_) =>
      case tok => Right(NotCorrectToken(tok, "Arrow", "Arrow"))
    }

    (Left(ParseState(remainderTokens, parsedExprs, mapDepL, spanList)),errorList)
  }

  private def combineTypes(typesList: List[r.types.Type]): r.types.Type = {
    if (typesList.isEmpty) {
      throw new IllegalArgumentException("the ElemList is empty!")
    }
    if (typesList.tail.isEmpty) {
      typesList.head
    } else {
      rt.FunType(typesList.head, combineTypes(typesList.tail))
    }
  }

  private def combineSynElemList(leftSynElemList: List[SyntaxElement], rightSynElemList: List[SyntaxElement]):
  List[SyntaxElement] = {
    var lS = leftSynElemList
    var rS = rightSynElemList
    var l: List[SyntaxElement] = Nil
    while (rS.nonEmpty) {
      l = rS.head :: l
      rS = rS.tail
    }
    while (lS.nonEmpty) {
      l = lS.head :: l
      lS = lS.tail
    }
    l
  }

  private def createSIntToExpr(name: SIntToExprAlternatives, n: Int, span: Span): r.Expr = {
    val expr = name match {
      case AltMapGlobal() => op.mapGlobal(n, Some(span)).toExpr
      case AltMapLocal() => op.mapLocal(n, Some(span)).toExpr
      case AltMapWorkGroup() => op.mapWorkGroup(n, Some(span)).toExpr
      case AltMakeArray() => rp.makeArray(n, Some(span)).toExpr
    }
    expr
  }

  private def combineExpressionDependentFirsExpr(synElemList: List[SyntaxElement], mapDepL: MapDepL): (r.Expr, List[SyntaxElement]) = {
    if (synElemList.isEmpty) {
      throw new IllegalArgumentException("the ElemList is empty!")
    }
    var synE = synElemList.reverse
    var e: r.Expr = synE.head match {
      case SLet(sp) => {
        val (ex1, sp1) = synE.tail match {
          case SExpr(expr1) :: l => (expr1, expr1.span.get)
          case SExprClutched(expr1, sp1L) :: l => (expr1, sp1L)
          case r => throw new IllegalArgumentException("It is an 1.Expr expected: " + r)
        }
        val (ex2, sp2) = synE.tail.tail match {
          case SExpr(expr1) :: l => (expr1, expr1.span.get)
          case SExprClutched(expr1, sp1L) :: l => (expr1, sp1L)
          case r => throw new IllegalArgumentException("It is an 2.Expr expected: " + r)
        }
        synE = synE.tail.tail.tail
        val span = sp + sp1 + sp2
        val e_let = r.App(rp.let(ex1, sp), ex2)(rt.TypePlaceholder, Some(span))
        require(e_let.span != None)
        e_let
      }
      case SExprClutched(expr, spanClutch) => {
        synE = synE.tail
        if (synE.isEmpty) {
          expr
        } else {
          val (exprComb, l) = combineExpressionsDependentOneStep(expr, synE, mapDepL, Some(spanClutch))
          synE = l
          exprComb
        }
      }
      case SExpr(expr) => {
        if (expr.span.isEmpty) {
          throw new IllegalStateException("Span of the first Expr '" + expr + "' is None in combineExpressionsDependent")
        }
        synE = synE.tail
        expr
      }
      case SIntToExpr(name, span) => {
        if (synE.tail.isEmpty) {
          throw new IllegalStateException("For this Primitive '" + name + "' we expect to see an lenght in Int")
        }
        val (n, spanOfN) = synE.tail.head match {
          case SExpr(r.Literal(rS.IntData(len), spanOfLen)) => (len, spanOfLen)
          case _ => throw new IllegalStateException("For this Primitive '" + name + "' we expect to see an lenght in Int")
        }
        synE = synE.tail.tail
        spanOfN match {
          case None => throw new IllegalStateException("Span of N should not be None")
          case Some(spa2) => createSIntToExpr(name, n, span + spa2)
        }
      }
      case SAddrSpace(addrSpace, _) => throw new RuntimeException(
        "List should't have AddrSpaceTypes at this beginning position! " + addrSpace)
      case SType(t, _) => throw new RuntimeException("List should't have Types at this beginning position! " + t)
      case SData(t, _) => throw new RuntimeException("List should't have any Data at this position! " + t)
      case SNat(t, _) => throw new RuntimeException("List should't have any Nats at this position! " + t)
    }
    (e, synE)
  }

  private def combineExpressionsDependentOneStep(exp: r.Expr, synElemList: List[SyntaxElement],
                                                 mapDepL: MapDepL, spanOfClutchedExprBefore: Option[Span]): (r.Expr, List[SyntaxElement]) = {
    var synE = synElemList
    var e = exp

    val span_e = if (spanOfClutchedExprBefore == None) {
      e.span match {
        case Some(span) => span
        case None => throw new IllegalStateException("Span of combined Expr is None in combineExpressionsDependent: " + e)
      }
    } else {
      spanOfClutchedExprBefore.get
    }

    synE.head match {
      case SLet(sp) => {
        val (ex1, sp1) = synE.tail match {
          case SExpr(expr1) :: l => (expr1, expr1.span.get)
          case SExprClutched(expr1, sp1L) :: l => (expr1, sp1L)
          case r => throw new IllegalArgumentException("It is an 1.Expr expected: " + r)
        }
        val (ex2, sp2) = synE.tail.tail match {
          case SExpr(expr1) :: l => (expr1, expr1.span.get)
          case SExprClutched(expr1, sp1L) :: l => (expr1, sp1L)
          case r => throw new IllegalArgumentException("It is an 2.Expr expected: " + r)
        }
        synE = synE.tail.tail.tail
        val span = sp + sp1 + sp2
        val e_let = r.App(rp.let(ex1, sp), ex2)(rt.TypePlaceholder, Some(span))
        e = r.App(e, e_let)(rt.TypePlaceholder, Some(span_e + span))
      }
      case SExprClutched(expr1, spanClutch) => {
        val span = span_e + spanClutch
        println("\n\nspan in SExprClutched: " + span)
        e = r.App(e, expr1)(rt.TypePlaceholder, Some(span))
        synE = synE.tail
      }
      case SExpr(expr1) => {
        val span_expr1 = expr1.span match {
          case Some(span) => span
          case None => throw new IllegalStateException("Span of the next Expr '" + expr1 + "'is None in combineExpressionsDependent")
        }
        val span = span_e + span_expr1
        e = r.App(e, expr1)(rt.TypePlaceholder, Some(span))
        synE = synE.tail
      }
      case SData(DIdentifier(data@rt.DataTypeIdentifier(name, _)), dataSpan) => {
        val span = dataSpan + span_e

        mapDepL.get(name) match {
          case None => {
            //Todo: Bessere Fehlermeldung!!!
            throw new IllegalArgumentException("The DataTypeIdentifier '" + name + "' is unknown!")
          }
          case Some(k) => k match {
            case RData() => e = r.DepApp[rt.DataKind](e, rt.DataTypeIdentifier(name, true))(rt.TypePlaceholder, Some(span))
            case RNat() => e = r.DepApp[rt.NatKind](e, rt.NatIdentifier(name, true))(rt.TypePlaceholder, Some(span))
            case RAddrSpace() => e = r.DepApp[rt.AddressSpaceKind](e,
              rt.AddressSpaceIdentifier(name, true))(rt.TypePlaceholder, Some(span))
          }
        }
        synE = synE.tail
      }
      case SIntToExpr(name, span1) => {
        if (synE.tail.isEmpty) {
          throw new IllegalStateException("For this Primitive '" + name + "' we expect to see an lenght in Int")
        }
        val (n, spanOfN) = synE.tail.head match {
          case SExpr(r.Literal(rS.IntData(len), spanInt)) => spanInt match {
            case None => throw new IllegalStateException("Span should not be None")
            case Some(spanI) => (len, spanI)
          }
          case _ => throw new IllegalStateException("For this Primitive '" + name + "' we expect to see an lenght in Int")
        }
        val span = span1 + spanOfN + span_e
        synE = synE.tail.tail
        createSIntToExpr(name, n, span)
      }
      case SAddrSpace(addrSpace, spanAddr) => {
        val span = span_e
        e = r.DepApp[rt.AddressSpaceKind](e, addrSpace)(rt.TypePlaceholder, Some(span))
        synE = synE.tail
      }
      case SType(t, spanType) => {
        val span = span_e + spanType
        //        println("spanType: "+ spanType.end + " of Type: " + t)
        if (t.isInstanceOf[rt.DataTypeIdentifier]) {
          t match {
            case rt.DataTypeIdentifier(name, _) => mapDepL.get(name) match {
              case None => {
                //Todo: Bessere Fehlermeldung!!!
                throw new IllegalArgumentException("The DataTypeIdentifier '" + name + "' is unknown!")
              }
              case Some(k) => {
                //                print("Type: "+ t + "; "+ span.end)
                k match {
                  case RData() => e = r.DepApp[rt.DataKind](e, rt.DataTypeIdentifier(name, true))(rt.TypePlaceholder, Some(span))
                  case RNat() => e = r.DepApp[rt.NatKind](e, rt.NatIdentifier(name, true))(rt.TypePlaceholder, Some(span))
                  case RAddrSpace() => e = r.DepApp[rt.AddressSpaceKind](e,
                    rt.AddressSpaceIdentifier(name, true))(rt.TypePlaceholder, Some(span))
                }
              }
            }
            case _ => throw new IllegalStateException(
              "This should not be happening in the combining of the Dependent Expressions")
          }
        } else {
          e = r.DepApp[rt.TypeKind](e, t)(rt.TypePlaceholder, Some(span))
        }
        synE = synE.tail
      }
      case SData(dataElem, dataSpan) => {
        val span = span_e
        dataElem match {
          case DIdentifier(data) => e = r.DepApp[rt.DataKind](e, data)(rt.TypePlaceholder, Some(span))
          case DType(data) => e = r.DepApp[rt.DataKind](e, data)(rt.TypePlaceholder, Some(span))
        }
        synE = synE.tail
      }
      case SNat(natElem, spanNat) => {
        val span = span_e
        natElem match {
          case NIdentifier(nat) => e = r.DepApp[rt.NatKind](e, nat)(rt.TypePlaceholder, Some(span))
          case NNumber(nat) => e = r.DepApp[rt.NatKind](e, nat)(rt.TypePlaceholder, Some(span))
        }
        synE = synE.tail
      }
    }

    (e, synE)
  }

  private def combineExpressionsDependent(synElemList: List[SyntaxElement], mapDepL: MapDepL): r.Expr = {
    var (e, synE) = combineExpressionDependentFirsExpr(synElemList, mapDepL)
    println("I will combine Expressions in combineExpressionsDependent: " + synE + " <::> " + e)
    while (!synE.isEmpty) {
      val r = combineExpressionsDependentOneStep(e, synE, mapDepL, None)
      e = r._1
      synE = r._2
    }
    println("I have combined the Expressions in combineExpressionsDependent: " + e + " ; " + e.span.get.end)
    e
  }

  def parseTypAnnotatedIdentAndThenNamedExprAndOtherTypAnnotatedIdens(parseState: ParseState):
  Either[MapFkt, ErrorList] = {
    val mapFkt = new MapFkt
    if (parseState.tokenStream.isEmpty) {
      throw new IllegalArgumentException("TokenStream is empty")
    }
    if (!parseState.parsedSynElems.isEmpty) {
      throw new IllegalArgumentException("parsedSynElemnts has to be empty: " + parseState.parsedSynElems)
    }

    var tokenList = parseState.tokenStream match {
      case BeginTypAnnotatedIdent(_) :: remainderTokens => {
        val ps: ParseState = ParseState(remainderTokens, Nil, Some(new MapDepL), parseState.spanList)
        val psNew = parseTypAnnotatedIdent(ps, mapFkt)
        //println("MapFkt after first TypeAnn: "+ mapFkt)
        psNew match {
          case Left(tokens) => if (!tokens.isEmpty) {
            tokens
          } else {
            throw new IllegalStateException("We need an NamedExpr too, because of that it " +
              "should not be possible only to have an TypAnnotatedIdent")
          }
          case Right(errorL) => return Right(errorL)
        }
      }
      case BeginNamedExpr(_) :: remainderTokens => {
        throw new IllegalArgumentException("You aren't allowed to start with an NamedExpr")
      }
      case a => throw new IllegalArgumentException("You have started with something different: " + a)
    }

    while (!tokenList.isEmpty) {
      println("tokens: " + tokenList + " ,MapFkt: " + mapFkt)
      tokenList match {
        case BeginTypAnnotatedIdent(_) :: remainderTokens => {
          val p: ParseState = ParseState(remainderTokens, Nil, Some(new MapDepL), parseState.spanList)
          val psNew = parseTypAnnotatedIdent(p, mapFkt)
          psNew match {
            case Left(tokens) => {
              tokenList = tokens
            }
            case Right(e) => return Right(e)
          }
        }
        case BeginNamedExpr(_) :: remainderTokens => {
          val p: ParseState = ParseState(remainderTokens, Nil, Some(new MapDepL), parseState.spanList)
          println("p: " + p)
          val psNew = parseNamedExpr(p, mapFkt)
          println("psNew: " + psNew)
          psNew match {
            case Left(tokens) => {
              tokenList = tokens
            }
            case Right(errorL) => return Right(errorL)
          }
        }
        case a => throw new IllegalArgumentException("You have started with something different: " + a)
      }
    }

    Left(mapFkt)
  }

  /*
  top level Lambda expects that the type of the Identifier is defined!

  only use as top level!
   */
  def parseNamedExpr(parseState: ParseState, mapFkt: MapFkt): Either[List[Token], ErrorList] = {
    require(parseState.parsedSynElems.isEmpty, "parseState is not empty, but nothing should be in it yet")
    val (psLambdaOld,errorList) =
      (Left(parseState),ErrorList()) |>
        parseIdent

    val (ps, identifierFkt, typeOfFkt): (ParseState, r.Identifier, r.types.Type) = psLambdaOld match {
      case Right(e) => return Right(errorList.add(e))
      case Left(p) => {
        p.parsedSynElems.head match {
          case SExpr(id@r.Identifier(n)) =>
            mapFkt.get(n) match {
              case None => {
                println("Identifier doesn't exist: " + n + " , " + psLambdaOld)
                throw new IllegalStateException("We want to parse an NamedExpr for " + n +
                  " but this Identifier is not declared yet!")
              }
              case Some(Left(e)) => throw new IllegalStateException("The Lambda-Fkt should't be initiated yet!: " + e)
              case Some(Right(typeFkt)) =>
                (ParseState(p.tokenStream, parseState.parsedSynElems, p.mapDepL, p.spanList),
                  id.setType(typeFkt), typeFkt)
            }
          case SLet(sp) =>
            throw new IllegalStateException("it is an Identifier expected not let: " + sp)
          case SExprClutched(expr, spanClutch) =>
            throw new IllegalStateException("it is an Identifier expected not expr with spanClutch: " + expr + " ; " + spanClutch)
          case SExpr(expr) => throw new IllegalStateException("it is an Identifier expected: " + expr)
          case SType(t, _) => throw new IllegalStateException(
            "it is an Identifier expected but an Type is completely false: " + t)
          case SIntToExpr(prim, _) => throw new IllegalStateException("it is an Identifier expected: " + prim)
          case SData(t, _) => throw new RuntimeException("List should't have any Data at this position! " + t)
          case SNat(t, _) => throw new RuntimeException("List should't have any Nats at this position! " + t)
          case SAddrSpace(addrSpace, _) => throw new RuntimeException(
            "List should't have AddrSpaceTypes at this beginning position! " + addrSpace)
        }
      }
    }

    val (psNamedExprBefore,newEL) = {
      (Left(ps),errorList) |>
        parseEqualsSign |>
        parseMaybeAppExpr
    }


    val (tokenList,synElemList, mapDepL) = psNamedExprBefore match {
      case Right(e) => return Right(newEL.add(e))
      case Left(p) => {
        p.tokenStream match {

          case EndNamedExpr(_) :: remainderTokens => {
            mapFkt.get(identifierFkt.name) match {
              case None => throw new IllegalStateException("Identifier seems not to be in the Map: " +
                identifierFkt.name + " , " + mapFkt)
              case Some(Left(e)) => throw new IllegalStateException("The Lambda-Fkt should't be initiated yet!: " + e)
              case Some(Right(l)) =>
                //Todo: I have to add Types in the Identifiers in Lambda and delete it after it, after this this if-clause makes sense
                //                if(!l.isEmpty){
                //                throw new IllegalStateException("The List should be empty! But it isn't. " +
                //                  "Probably we have one or more Types in the " +
                //                  "TypAnnotationIdent declared than the NamedExpr really has. Types left: " + l + "\nTypes defined: " + typesDefined + "\nNamedExpr: " + p.parsedSynElems)
                //              }else{
                //Todo: We have to give the Identifier (identifierFkt/p.map.get(n)) now a Type

                (remainderTokens, p.parsedSynElems, p.mapDepL.get)
              //              }
            }
          }
          case _ => {
            throw new IllegalStateException("NewExpr ends with an EndNamedExpr, but we have no EndNamedExpr at the end")
          }
        }
      }
    }

    println("\n\n\n Before combining the Expr in parseNamedExpr \n\n\n")
    var expr = combineExpressionsDependent(synElemList, mapDepL)
    expr = expr.setType(typeOfFkt)

    println("expr finished: " + expr + " with type: " + expr.t + "   (should have Type: " + typeOfFkt + " ) ")

    require(expr.span != None, "expr is None!")
    rd.ToBeTyped(expr) match {
      case rd.ToBeTyped(e) => require(e.span != None, "expr is now in ToBeTyped without infer None")
      case _ => throw new IllegalStateException("this should not be happening")
    }
    println("before requre MapFkt: '"+ mapFkt + "' for the Expr: '"+ expr + "' with Type: '"+ expr.t + "'")
    require(rd.ToBeTyped(expr).toExpr.span != None, "expr is now with ToBeType.toExpr None!")

    mapFkt.update(identifierFkt.name, Left(rd.ToBeTyped(expr)))
    println("map updated: " + mapFkt + "\nRemainderTokens: " + tokenList)
    Left(tokenList)
  }

  /*
  mapFkt is by reference, and does not to be returned
   */
  def parseTypAnnotatedIdent(parseState: ParseState, mapFkt:MapFkt): Either[List[Token], ErrorList] = {
    require(parseState.parsedSynElems.isEmpty, "parseState is not empty, but nothing should be in it yet")
    val psLambdaOld =
      (Left(parseState),ErrorList()) |>
        parseIdent

    val (ps, identifierFkt, eL): (ParseState, r.Identifier, ErrorList) = psLambdaOld match {
      case (Right(e),errorL) => return Right(errorL.add(e))
      case (Left(p),errorL) => {
        p.parsedSynElems.head match {
          case SExpr(id@r.Identifier(n)) => if (mapFkt.contains(n)) {
            println("Identifier does already exist: " + n + " , " + psLambdaOld)
            throw new IllegalStateException("We want to parse an TypAnnotatedIdent for " + n
              + " but this Identifier is already declared!")
          } else {
            (p, id, errorL)
          }
          case SLet(sp) =>
            throw new IllegalStateException("it is an Identifier expected not let: " + sp)
          case SExprClutched(expr, spanClutch) =>
            throw new IllegalStateException("it is an Identifier expected not expr with spanClutch: " + expr + " ; " + spanClutch)
          case SExpr(expr) => throw new IllegalStateException("it is an Identifier expected: " + expr)
          case SType(t, _) => throw new IllegalStateException(
            "it is an Identifier expected but an Type is completely false: " + t)
          case SIntToExpr(prim, _) => throw new IllegalStateException("it is an Identifier expected: " + prim)
          case SData(t, _) => throw new RuntimeException("List should't have any Data at this position! " + t)
          case SNat(t, _) => throw new RuntimeException("List should't have any Nats at this position! " + t)
          case SAddrSpace(addrSpace, _) => throw new RuntimeException(
            "List should't have AddrSpaceTypes at this beginning position! " + addrSpace)
        }
      }
    }

    val psNamedExprBefore = {
      (Left(ParseState(ps.tokenStream, Nil, ps.mapDepL, ps.spanList)),eL) |>
        parseDoubleColons |>
        parseType
    }


    val (psNew,newErrorList) = psNamedExprBefore match {
      case (Right(e),erL) => return Right(erL.add(e))
      case (Left(p),erL) => (p,erL)
    }

    println("in the middle of TypAnnotatedIden: " + psNew)
    psNew.tokenStream match {
      case EndTypAnnotatedIdent(_) :: remainderTokens => {
        val (typesList, _) = getTypesInList(psNew.parsedSynElems, None)
        if (typesList.length != 1) {
          throw new IllegalStateException("The TypesList should have lenght 1: " + typesList)
        }
        mapFkt.put(identifierFkt.name, Right(typesList.head))
        println("return TypAnnotatedIdent: " + remainderTokens + " <<<<>>>> " + mapFkt)
        Left(remainderTokens)
      }
      case EndNamedExpr(_) :: remainderTokens => {
        throw new IllegalStateException("TypAnnotatedIdent ends with an EndTypAnnotatedIdent, but end with EndNamedExpr")
      }
      case a => {
        throw new IllegalStateException(
          "TypAnnotatedIdent ends with an EndTypAnnotatedIdent, but we have no EndTypAnnotatedIdent at the end: " + a)
      }
    }
  }

  def getTypesInList(synElems: List[SyntaxElement], spanOp: Option[Span]): (List[r.types.Type], Span) = {
    if (!synElems.isEmpty) {
      synElems.head match {
        case SType(typ, sp) => {
          val span = spanOp match {
            case None => sp
            case Some(s) => s + sp
          }
          val (l, newSpan) = getTypesInList(synElems.tail, Some(span))
          (typ :: l, newSpan)
        }
        case SLet(sp) =>
          throw new IllegalStateException("it is an Identifier expected not let: " + sp)
        case SExprClutched(expr, spanClutch) =>
          throw new IllegalStateException("in getTypesInList we have as head a not Type:" + expr + " ; " + spanClutch)
        case SExpr(e) => throw new IllegalArgumentException("in getTypesInList we have as head a not Type: " + e)
        case SIntToExpr(e, _) => throw new IllegalArgumentException(
          "in getTypesInList we have as head a not Type: " + e)
        case SData(t, sp) => {
          val span = spanOp match {
            case None => sp
            case Some(s) => s + sp
          }
          t match {
            case DIdentifier(data) => {
              val (l, newSpan) = getTypesInList(synElems.tail, Some(span))
              (data :: l, newSpan)
            }
            case DType(data) => {
              val (l, newSpan) = getTypesInList(synElems.tail, Some(span))
              (data :: l, newSpan)
            }
          }
        }
        case SNat(t, _) => throw new RuntimeException("List should't have any Nats at this position! " + t)
        case SAddrSpace(addrSpace, _) => throw new RuntimeException(
          "List should't have AddrSpaceTypes at this beginning position! " + addrSpace)
      }
    } else {
      spanOp match {
        case None => throw new IllegalStateException("spanOp should not be None")
        case Some(sp) => (Nil, sp)
      }
    }
  }

  def parseType(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    println("parseType: " + parseState)
    val ps: OutputEPState =
      (Left(parseState),errorList) |>
        (parseDepOrNormalFunctionType _ || parseSimpleType)
    ps
  }

  def parseDepOrNormalFunctionType(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    println("parseType: " + parseState)
    val ps: OutputEPState =
      (Left(parseState),errorList) |>
        (parseDepFunctionType _ || parseFunctionType)
    ps
  }

  def parseSimpleType(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    println("parseType: " + parseState)
    val ps: OutputEPState =
      (Left(parseState),errorList) |>
        (parseBracesExprType _ ||
          parseTupleType || parseIndexType || parseArrayType || parseVecType ||
          parseScalarType || parseData) //Todo: The Function parseTypeIdentToCorrectForm is not good, because it is not clear what we should parse. I have an Function for parseData, but I don't need a function for parseNat, because Nat should not be returned. For ArrayType i am also unsure.
    ps
  }

  def parseFunctionType(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val ps = parseSimpleType((parseState,errorList))
    ps match {
      case (Right(e),eL) => (Right(e),eL)
      case (Left(p),eL) => if (p.tokenStream.isEmpty) {
        throw new IllegalStateException("Tokens are empty: " + p)
      } else {
        p.tokenStream.head match {
          case Arrow(_) => {
            val (newParse,errorListNew) = parseFunctionType((ParseState(p.tokenStream.tail, Nil,  p.mapDepL, p.spanList),eL)) match {
              case (Left(pars),erL) => (pars,erL)
              case (Right(e),erL) => return (Right(e),erL)
            }
            val (list1, spanList1) = getTypesInList(p.parsedSynElems, None)
            val (list2, spanList2) = getTypesInList(newParse.parsedSynElems, None)
            val typesList: List[r.types.Type] = combineTypes(list1) ::
              combineTypes(list2) :: Nil
            val newType: r.types.Type = combineTypes(typesList)
            (Left(ParseState(newParse.tokenStream, SType(newType, spanList1 + spanList2) :: Nil, p.mapDepL, p.spanList)),errorListNew)
          }
          //Todo: Maybe remove already here the EndTypAnnotatedIdent or RBrace from the TokenList
          case EndTypAnnotatedIdent(_) => (Left(p),eL)
          case RParentheses(_) => (Left(p),eL)
          case a => (Right(NotCorrectToken(a, "Arrow, EndTypAnn or RParentheses", "FunctionType")),eL)
        }
      }
    }
  }

  private def parseCompLoop(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    println("parseCompLoop: "+ parseState.tokenStream)
    val (p,errL) = (Left(parseState),errorList)|> parseDot |> parseNoAppExpr
    p match{
      case Right(e) => (Right(e),errL)
      case Left(ps) => if(ps.tokenStream.head.isInstanceOf[Dot]){
        (Left(ps),errL) |> parseCompLoop
      }else{
        (Left(ps),errL)
      }
    }
  }

  private def getExprAndSpan(syntaxElement: SyntaxElement, parseState: ParseState):Option[(r.Expr, Span)]={
    syntaxElement match {
      case SExpr(expr) => Some((expr, expr.span.get))
      case SExprClutched(expr, sp) => Some((expr, sp))
      case _ => {
        println("Abort; getExprAndSpan because not accepted beggining types: " + parseState)
        None
      }
    }
  }

  //Todo: Here I have to expect the result of getComposition, so I have to add SComp in the SyntaxElement
  //Todo: and with that I can return this SyntaxElement so that SComp has to look something like this:
  //Todo: SComp = (e1:r.Expr, s2:SyntaxElement, span:Span) and with that I have to create an Lambda like this
  //Todo: r.Lambda(r.freshname(e), r.App(e1,createLambda(s2))(r.TypePlaceholder, Some(span))
  private def parseComp(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    println("parseComp: " + parseState)
//    val cpyMapFkt = parseState.mapFkt.clone()
    val (psOld,eL) = (Left(ParseState(parseState.tokenStream, parseState.parsedSynElems.head::Nil,  parseState.mapDepL, parseState.spanList)),errorList) |>
      parseCompLoop //|> parseNoAppExpr
    println("Yuhuuuasdf: "+psOld)
    psOld match {
      case Right(e) => {
//        parseState.mapFkt.clear()
//        cpyMapFkt.foreach{case (n,t)=>parseState.mapFkt.update(n,t)}
        (Right(e),eL)
      }
      case Left(ps) => {
        //val (lastElem, lastElemSpan) = getExprAndSpan(ps.parsedSynElems.head, ps).get
        //var synElems = ps.parsedSynElems.tail.reverse
        var synElems = ps.parsedSynElems

        val (e1, sp1) = getExprAndSpan(synElems.head,parseState).get
        if(synElems.tail.isEmpty) return (Right(SynListIsEmpty(sp1, "Composition")),eL)
        val (e2, sp2) = getExprAndSpan(synElems.tail.head,parseState).get
        val span = sp1 + sp2 //+ lastElemSpan
        val name = r.freshName("e")
        //var app = r.App(e2, r.App(e1, lastElem)(rt.TypePlaceholder, Some(span)))(rt.TypePlaceholder, Some(span))
        //println("app of Comp: "+ app + " e1: '" + e1 + "' e2: '"+ e2 + "' lastElem: '" + lastElem + "'")
        var app = r.App(e2, r.App(e1, r.Identifier(name)(rt.TypePlaceholder, Some(span)))(rt.TypePlaceholder, Some(span)))(rt.TypePlaceholder, Some(span))
        println("app of Comp: "+ app + " e1: '" + e1 + "' e2: '"+ e2 + "'")

        synElems = synElems.tail.tail
        while(synElems.nonEmpty){
          val (exN, _) = getExprAndSpan(synElems.head,parseState).get
          synElems = synElems.tail
          app = r.App(exN, app)(rt.TypePlaceholder,Some(span))
          println("app of Comp in Loop: "+ app)
        }
        val lam = r.Lambda(r.Identifier(name)(rt.TypePlaceholder, Some(span)), app)(rt.TypePlaceholder, Some(span))
        println("lam in parseComp:"+lam + "\n")
        if(end(ps.tokenStream)){
          (Left(ParseState(ps.tokenStream, SExpr(lam) :: parseState.parsedSynElems.tail, ps.mapDepL, ps.spanList)),eL)
        }else{
          val (pP,newErrorList) = (Left(ParseState(ps.tokenStream, Nil, ps.mapDepL, ps.spanList)) ,eL)|> parseNoAppExpr
          pP match{
            case Right(e)=> return (Right(e),newErrorList)
            case Left(pL) => {
              val (expr, expr_span) = getExprAndSpan(pL.parsedSynElems.head, pL).get
              val resApp = r.App(lam,expr)(rt.TypePlaceholder, Some(span+expr_span))
              println("resApp in parseComp:"+resApp + "\n")
              (Left(ParseState(pL.tokenStream, SExpr(resApp) :: parseState.parsedSynElems.tail, pL.mapDepL, pL.spanList)),newErrorList)
            }
          }
        }
      }
    }
  }


  /*
is the whole Syntax-Tree.
the syntax-Tree has on top an Lambda-Expression
 */
  def parseLambda(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    if(parseState.tokenStream.isEmpty){
      println("Abbruch; parseLambda: "+ parseState)
      return (Left(parseState),errorList)
    }
    println("parseLambda: " +parseState)
    val (psOld,errorL) =
      (Left(ParseState(parseState.tokenStream, Nil,  parseState.mapDepL, parseState.spanList)),errorList) |>
        parseBackslash |>
        parseIdent     |>
        parseMaybeTypeAnnotation |>
        parseArrow

    val ((psOrErr, spanBackslash),idName) = psOld match {
      case Left(p) => {
        val synElemListExpr = p.parsedSynElems
        val (maybeTypedIdent, synElemListMaybeTIdent):(r.Expr, List[SyntaxElement]) = {
          synElemListExpr.head match {
            case SType(t, _) =>
              synElemListExpr.tail.head match {
                case SExpr(i) => (i.setType(t), synElemListExpr.tail.tail)
                case a => throw new RuntimeException("Here is an Expression expected, but " + a +" is not an Expression!")
              }
            case SLet(sp) =>
              throw new IllegalStateException("it is an Identifier expected not let: " + sp)
            case SExprClutched(_,_) => throw new IllegalStateException("SExprClutched is not expected here")
            case SExpr(i) => (i, synElemListExpr.tail)
            case SIntToExpr(prim, _) => throw new RuntimeException("Here is an Expression expected, but " + prim +
              " is not an Expression!")
            case SData(t,_) => t match {
              case DIdentifier(data) => synElemListExpr.tail.head match {
                case SExpr(i) => (i.setType(data), synElemListExpr.tail.tail)
                case a => throw new RuntimeException("Here is an Expression expected, but " + a +" is not an Expression!")
              }
              case DType(data) => synElemListExpr.tail.head match {
                case SExpr(i) => (i.setType(data), synElemListExpr.tail.tail)
                case a => throw new RuntimeException("Here is an Expression expected, but " + a +" is not an Expression!")
              }
            }
            case SNat(t,_) => throw new RuntimeException("List should't have any Nats at this position! " + t)
            case SAddrSpace(addrSpace,_) => throw new RuntimeException(
              "List should't have AddrSpaceTypes at this beginning position! " + addrSpace)
          }
        }
        require(synElemListMaybeTIdent.isEmpty, "the List should be empty")
        val identifierName = maybeTypedIdent.asInstanceOf[r.Identifier]
        print("WhereAre: "+p)
        //local variables are in the list, so that not two same localVariables are declared
//        if (map.contains(identifierName.name)) {//Todo: Better error
//              throw new IllegalArgumentException("parseLambda: A variable or function with the exact same name '"+ identifierName.name +
//                "' is already declared! <- " + map.get(identifierName.name))
//        }
//        map.update(identifierName.name, Left(rd.ToBeTyped(identifierName)))
        val ret = p.spanList match {
          case Some(Nil) => throw new IllegalArgumentException("this should not happen to be Nil")
          case None => throw new IllegalArgumentException("this should not happen to be None")
          case Some(span::Nil) => (parseMaybeAppExpr((ParseState(p.tokenStream,Nil, p.mapDepL, None),errorL)) , span )
          case Some(span::l)=> ( parseMaybeAppExpr((ParseState(p.tokenStream,Nil, p.mapDepL, Some(l)),errorL) ), span)
        }
        (ret, identifierName)
      }
      case Right(e) => {
        println("endLambda: "+ e)
        return (Right(e),errorL.add(e))
      }
    }

    val (toks, expr,  mapDepL, spanList, newEL) = psOrErr match {
      case (Left(psNew),errorList2) => {
        println("combine in Lambda: "+ psNew)
        val expr = combineExpressionsDependent(psNew.parsedSynElems, psNew.mapDepL.get)
        //println("\n\n\nSpanIsHere"+ expr.expr +" : "+ expr.expr.span+ "\n\n\n")
        (psNew.tokenStream,expr, psNew.mapDepL, psNew.spanList, errorList2)
      }
      case (Right(e),errorList2) => return (Right(e),errorList2)
    }
    val spanOfBackslash = parseState.tokenStream.head.s
    val span = Span(spanOfBackslash.file,spanOfBackslash.begin, expr.span.head.end)
    val lambda = r.Lambda(idName, expr)(rt.TypePlaceholder, Some(span))
    val myNewParseState = ParseState(toks, SExpr(lambda) :: parseState.parsedSynElems, mapDepL, spanList)
    println("myNewParseState: "+ myNewParseState)
    (Left(myNewParseState),newEL)
  }


  //_________________________________________________________Lambda
  //_________________________________________________________Expres

  def parseNoAppExpr(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    println("parseLowExpression: " + parseState)
    (Left(parseState),errorList) |>
      (parseLambda _ || parseDepLambda || parseBracesExpr ||
        parseUnOperator || parseBinOperator || parseIdent ||
        parseNumber || parseAddrSpaceType || parseTypeinNoAppExpr||
        parseNat
        )

  }

  //Todo:Maybe not needed any more
//  def ParseTypesUntilRBracket(inputEPState: InputEPState): OutputEPState = {
//    if(parseState.tokenStream.isEmpty||parseState.tokenStream.head.isInstanceOf[RBracket]){
//      println("Abbruch; endlessPossibleParseType: "+ parseState)
//      return (Left(parseState),errorList)
//    }
//    val p =
//      Left(ParseState(parseState.tokenStream,Nil, parseState.mapFkt, parseState.mapDepL, spanList) )  |>
//        parseType |> //Todo: I can't express FunctionTypes yet
//        ParseTypesUntilRBracket
//
//    p match {
//      case Left(newPS) => {
//        val synList = combineSynElemList(parseState.parsedSynElems, newPS.parsedSynElems).reverse
////        println("SynList in parseTypesUntilRBracket: " + synList + " , newPS: " + newPS.parsedSynElems + " , parseStateOld: " + parseState.parsedSynElems)
//        Left(ParseState(newPS.tokenStream, synList, newPS.mapFkt, newPS.mapDepL, spanList) )
//      }
//      case Right(e) => Right(e)
//    }
//  }

  def parseDepOrNormalFunctionTypeInNoAppExpr(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val (p,eL) = //Todo: change so that no Brackets are needed, only parentheses for Dep- and FunctionType
      (Left(parseState),errorList) |>
        parseLeftParentheses |>
        parseDepOrNormalFunctionType |>
        parseRightParentheses
    p match {
      case Right(e) => (Right(e),eL)
      case Left(pa) => (Left(ParseState(pa.tokenStream, pa.parsedSynElems,  pa.mapDepL, parseState.spanList)),eL)
    }
  }

  def parseTypeinNoAppExpr(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    println("parseTypeinNoAppExpr: "+ parseState)
    if(parseState.tokenStream.isEmpty){
      println("Abbruch; parseTypeinNoAppExpr: "+ parseState)
      return (Left(parseState),errorList)
    }
    val (p,eL) = //Todo: change so that no Brackets are needed, only parentheses for Dep- and FunctionType
      (Left(ParseState(parseState.tokenStream,Nil, parseState.mapDepL, parseState.spanList)),errorList)  |>
        (parseDepOrNormalFunctionTypeInNoAppExpr _ || parseSimpleType)

    println("after parseTypeinNoAppExpr: "+ p)
    p match {
      case Left(newPS) => {
        val synList = combineSynElemList(parseState.parsedSynElems, newPS.parsedSynElems).reverse
//        println("SynList2 in parseTypeinNoAppExpr: " + synList + " , newPS: " + newPS.parsedSynElems + " , parseStateOld: " + parseState.parsedSynElems)
        (Left(ParseState(newPS.tokenStream, synList,  newPS.mapDepL, newPS.spanList) ),eL)
      }
      case Right(e) => (Right(e),eL)
    }
  }

  def parseBracesExpr(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    println("parseBracesExpr: "+ parseState)
    if(parseState.tokenStream.isEmpty){
      println("Abbruch; parseBracesExpr: "+ parseState)
      return (Left(parseState),errorList)
    }
    val (p,eL) =
      (Left(ParseState(parseState.tokenStream,Nil,  parseState.mapDepL, parseState.spanList)),errorList)  |>
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
        val spanClutch = pState.spanList match {
          case None => throw new IllegalStateException("Here should SpanList not be NONE")
          case Some(sp1::sp2::Nil) => sp1+sp2
          case Some(l) => throw new IllegalStateException("The List should have two Spans: "+ l)
        }
        val synElem = if(pState.parsedSynElems.length==1){
          pState.parsedSynElems.head match {
            case SLet(sp) => SLet(spanClutch)
            case SExpr(expr) => SExprClutched(expr, spanClutch)
            case SExprClutched(expr, _) => SExprClutched(expr, spanClutch)
            case SAddrSpace(addrSpace, _) => SAddrSpace(addrSpace, spanClutch)
            case SData(data, _) => SData(data, spanClutch)
            case SIntToExpr(name, _) => SIntToExpr(name, spanClutch)
            case SNat(nat, _) => SNat(nat, spanClutch)
            case SType(t, _) => SType(t, spanClutch)
          }
        }else {
          val newExpr = combineExpressionsDependent(pState.parsedSynElems, pState.mapDepL.get)
          SExprClutched(newExpr, spanClutch)
        }
        //println("synElem: " + synElem)
        val newL = synElem :: Nil
        val li:List[SyntaxElement] = parseState.parsedSynElems.reverse ++ newL
        val l = li.reverse
        val newParse:ParseState = ParseState(pState.tokenStream, l,  pState.mapDepL, parseState.spanList)
        (Left(newParse),eL)
      }
      case Right(e) => (Right(e),eL)
    }
  }

  def parseArrayType(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    //println("parseIndexType: " + parseState)
    val (p,eL) =
      (Left(ParseState(parseState.tokenStream,Nil, parseState.mapDepL, parseState.spanList)),errorList)|>
        parseNat |>
        parseDot |>
        parseSimpleType

    p match {
      case Left(pState) => {
        require(pState.parsedSynElems.length==2, "It should exactly be 1 Nat and one Identifer for IndexType in the list!")
        val ty = pState.parsedSynElems.reverse match {
          case SNat(nat,spanNat)::SData(data,spanData) :: Nil => {
            val span = spanNat+spanData
            nat match {
              case NNumber(n) => data match {
                case DIdentifier(d) => SType(rt.ArrayType(n, d), span)
                case DType(d) => SType(rt.ArrayType(n,d), span)
              }
              case NIdentifier(n) => data match {
                case DIdentifier(d) => SType(rt.ArrayType(n, d), span)
                case DType(d) => SType(rt.ArrayType(n,d), span)
              }
            }
          }
          case SNat(nat,spanNat)::SType(t, spanType) :: Nil => {
            val span = spanNat+spanType
            nat match {
              case NNumber(n) => SType(rt.ArrayType(n, t.asInstanceOf[rt.DataType]), span)
              case NIdentifier(n) => SType(rt.ArrayType(n, t.asInstanceOf[rt.DataType]), span)
            }
          }
          case a => throw new RuntimeException("List should't have only Nat at this position! " + a)
        }
        val newL = ty :: Nil
        val li:List[SyntaxElement] = parseState.parsedSynElems.reverse ++ newL
        val l = li.reverse
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.mapDepL, pState.spanList)
        (Left(newParse),eL)
      }
      case Right(e) => (Right(e),eL)
    }
  }

  def parseIndexType(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    //println("parseIndexType: " + parseState)
    val (p,eL) =
      (Left(ParseState(parseState.tokenStream,Nil, parseState.mapDepL, parseState.spanList)),errorList)  |>
        parseTypeIdent |>
        parseLeftBracket |>
        parseNat |>
        parseRightBracket

    p match {
      case Left(pState) => {
        require(pState.parsedSynElems.length==2,
          "It should exactly be 1 Nat and one Identifer for IndexType in the list!")
        val ty = pState.parsedSynElems.reverse match {
          case SType(rt.TypeIdentifier("Idx"), spanType) :: SNat(nat,spanNat)::Nil => {
            val span = spanType+spanNat
            nat match {
              case NNumber(nat) => SType(rt.IndexType(nat), span)
              case NIdentifier(nat) => SType(rt.IndexType(nat), span)
            }
          }
          case a => throw new RuntimeException("List should't have only Identifier at this position! " + a)
        }
        val newL = ty :: Nil
        val li:List[SyntaxElement] = parseState.parsedSynElems.reverse ++ newL
        val l = li.reverse
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.mapDepL, pState.spanList)
        (Left(newParse),eL)
      }
      case Right(e) => (Right(e),eL)
    }
  }
  def parseTupleType(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    //println("parseTupleType: "+ parseState)
    val (p,eL) =
      (Left(ParseState(parseState.tokenStream,Nil, parseState.mapDepL, parseState.spanList)),errorList)  |>
        parseLeftParentheses  |>
        parseType |>
        parseComma |>
        parseType |>
        parseRightParentheses

    p match {
      case Left(pState) => {
        if(pState.parsedSynElems.isEmpty){
          val rBraceIndex = parseState.tokenStream.indexWhere(p=> p.isInstanceOf[RParentheses])
          throw new RuntimeException("There was no Expression in Braces at posstion (" + 0 + " , " + rBraceIndex +
            " : "+ parseState.tokenStream.toString())
        }
        val (tL, tLSpan) = getTypesInList(pState.parsedSynElems, None)
        val typesList = tL.reverse
        require(typesList.length==2, "It should exactly be two Types for PairType in the list!")
        val ty = SType(rt.PairType(typesList.head.asInstanceOf[rt.DataType],
          typesList.tail.head.asInstanceOf[rt.DataType]), tLSpan)
        val newL = ty :: Nil
        val li:List[SyntaxElement] = parseState.parsedSynElems.reverse ++ newL
        val l = li.reverse
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.mapDepL, parseState.spanList)
        (Left(newParse),eL)
      }
      case Right(e) => (Right(e),eL)
    }
  }

  def parseBracesExprType(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    //println("parseBracesExprType: "+ parseState)
    val (p,eL) =
      (Left(ParseState(parseState.tokenStream,Nil, parseState.mapDepL, parseState.spanList)),errorList)  |>
        parseLeftParentheses  |>
        parseType |>
        parseRightParentheses

    p match {
      case Left(pState) => {
        if(pState.parsedSynElems.isEmpty){
          val rBraceIndex = parseState.tokenStream.indexWhere(p=> p.isInstanceOf[RParentheses])
          throw new RuntimeException("There was no Expression in Braces at posstion (" + 0 + " , " + rBraceIndex +
            " : "+ parseState.tokenStream.toString())
        }
        val (tL, spanTL) = getTypesInList(pState.parsedSynElems, None)
        val ty = SType(combineTypes(tL), spanTL)
        val newL = ty :: Nil
        val li:List[SyntaxElement] = parseState.parsedSynElems.reverse ++ newL
        val l = li.reverse
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.mapDepL, parseState.spanList)
        (Left(newParse),eL)
      }
      case Right(e) => (Right(e),eL)
    }
  }

  private def end(tokenStream:List[Token]):Boolean={
    tokenStream.isEmpty||tokenStream.head.isInstanceOf[EndNamedExpr]
  }

  def parseMaybeAppExpr(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    //println("parseMaybeAppExpr: " + parseState)
    if(parseState.tokenStream.head.isInstanceOf[RParentheses]){
      println("L" +
        "RBrace is at the beginning of parseApp: " + parseState)
      return (Left(parseState),errorList)
    }else if(parseState.tokenStream.head.isInstanceOf[EndNamedExpr]){
      println("EndNamedExpr sighted in ParseMaybeAppExpr: "+ parseState)
      return (Left(parseState),errorList)
    }
    val (parseStateOrError,eL) =
      (Left(parseState),errorList)  |> parseNoAppExpr
    //println("parseApp after parseLowExpression: "+ parseStateOrError)
    parseStateOrError match {
      case Right(e) => (Right(e),eL.add(e))
      case Left(ps)=> if(end(ps.tokenStream)){
        //println("parseApp End, because TokenList is empty: "+ ps)
                              val expr = combineExpressionsDependent(ps.parsedSynElems, ps.mapDepL.get)
                              (Left(ParseState(ps.tokenStream, SExpr(expr)::Nil,  ps.mapDepL, ps.spanList)), eL)
                            }else{
                              if(ps.parsedSynElems.isEmpty) throw new IllegalStateException("ps is Empty: "+ ps)
                              val (p, newEL) = if(ps.tokenStream.head.isInstanceOf[Dot]){
                                (Left(ps),eL)|> parseComp
                              }else{
                                (Left(ps),eL)|> parseMaybeAppExpr
                              }

                              p match {
                                case Right(e) => (Right(e),newEL.add(e))
                                case Left(newPS) => {
                                  val synElem = if(newPS.parsedSynElems.length==1){
                                    newPS.parsedSynElems.head
                                  }else {
                                    if(newPS.parsedSynElems.isEmpty){
                                      val e = SynListIsEmpty(newPS.tokenStream.head.s, "MaybeAppExpr")
                                      return (Right(e), newEL.add(e))
                                    }
                                    val expr = combineExpressionsDependent(newPS.parsedSynElems, newPS.mapDepL.get)
                                    //println("\n\n\nSpanIsHere"+ expr +" : "+ expr.span+ "\n\n\n")
                                    SExpr(expr)
                                  }
                                  (Left(ParseState(newPS.tokenStream, synElem::Nil, newPS.mapDepL, newPS.spanList)), newEL)
                                }
                              }
                            }
    }
  }

  def parseEqualsSign(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val ParseState(tokens, parsedExprs, mapDepL, spanList) = parseState
    if (tokens.isEmpty) {
      return (Right(TokListIsEmpty(SpanPlaceholder, "EqualsSign")),errorList)
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case EqualsSign(_) =>
      case tok => {
        return (Right(NotCorrectToken(tok, "EqualsSign", "EqualsSign")),errorList)
      }
    }

    (Left(ParseState(remainderTokens, parsedExprs, mapDepL, spanList)),errorList)
  }

  def parseDoubleColons(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val ParseState(tokens, parsedExprs, mapDepL, spanList) = parseState
    if (tokens.isEmpty) {
      return (Right(TokListIsEmpty(SpanPlaceholder, "DoubleColons")),errorList)
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case DoubleColons(_) =>
      case tok => {
        return (Right(NotCorrectToken(tok, "DoubleColons", "DoubleColons" )),errorList)
      }
    }

    (Left(ParseState(remainderTokens, parsedExprs, mapDepL, spanList)),errorList)
  }


  def parseAddrSpaceType(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    if (parseState.tokenStream.isEmpty) {
      return (Right(TokListIsEmpty(SpanPlaceholder, "AddrSpacerType")),errorList)
    }
    val nextToken :: remainderTokens = parseState.tokenStream

    nextToken match {
      case AddrSpaceType(addrSpace, spanAddr) => {
        val p:rt.AddressSpace = addrSpace match {
          case "Local" => rt.AddressSpace.Local
          case "Global" => rt.AddressSpace.Global
          case "Private" => rt.AddressSpace.Private
          case "Constant" => rt.AddressSpace.Constant
        }
        (Left(ParseState(remainderTokens,
          SAddrSpace(p,spanAddr) :: parseState.parsedSynElems, parseState.mapDepL, parseState.spanList) ),errorList)
      }
      case tok => {
        println("AddrSpaceTypeWasExpected: "+ tok + ": " + remainderTokens)
        (Right(NotCorrectToken(tok, "addrSpaceType", "AddrSpaceType")),errorList)
      }
    }
  }

  def parseUnOperator(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val nextToken :: remainderTokens = parseState.tokenStream

    val p = nextToken match {
      case UnOp(un, span) => un match {
        case OpType.UnaryOpType.NEG => Left(ParseState(remainderTokens,
          SExpr(r.primitives.neg(Some(span))) :: parseState.parsedSynElems, parseState.mapDepL, parseState.spanList) )
        case OpType.UnaryOpType.NOT => Left(ParseState(remainderTokens,
          SExpr(r.primitives.not(Some(span))) :: parseState.parsedSynElems, parseState.mapDepL, parseState.spanList) )
      }
      case tok => {
        //println("UnaryOperatorWasExpected: "+ tok + ": " + remainderTokens)
        Right(NotCorrectToken(tok, "UnOp", "UnOperator"))
      }
    }
    (p,errorList)
  }

  def parseNumber(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    //println("ParseNumber: " + parseState)
    val ParseState(tokens, parsedSynElems, mapDepL, spanList)  = parseState
    if(tokens.isEmpty){
      return (Right(TokListIsEmpty(SpanPlaceholder, "Number")),errorList)
    }
    val nextToken :: remainderTokens = tokens

    val p = nextToken match {
      case I8(number, span) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.IntData(number), Some(span))) :: parsedSynElems, mapDepL, spanList) )
      case I32(number, span) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.IntData(number),Some(span))) :: parsedSynElems, mapDepL, spanList) )
      case F32(number, span) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.FloatData(number),Some(span))) :: parsedSynElems, mapDepL, spanList) )
      case F64(number, span) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.DoubleData(number),Some(span))) :: parsedSynElems, mapDepL, spanList) )
      case tok => Right(NotCorrectToken(tok, "Integer of Float", "Number"))
    }
    (p,errorList)
  }


  def parseBinOperator(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    //println("parseBinOperator: "+ parseState)
    val ParseState(tokens, parsedSynElems, mapDepL, spanList)  = parseState
    val nextToken :: remainderTokens = tokens

    val p = nextToken match {
      case BinOp(op, span) => op match {
        case OpType.BinOpType.ADD =>{
          //println("\n\n Span of add: "+ span + " , add: "+ r.primitives.add(Some(span)) + "  ,SExpr(add): "+ SExpr(r.primitives.add(Some(span))).expr ) //es wird vergessen von Scala, dass es auch vom Type ToBeTyped ist
          //println("span of add: " + r.primitives.add(Some(span)).span + "span of add: " + r.primitives.add(Some(span)).toUntypedExpr.span+ " , span of SExpr(add): "+ SExpr(r.primitives.add(Some(span))).expr.span)
          //println("span of makeArray: "+ r.primitives.makeArray(5, Some(span)).span)
          Left(ParseState(remainderTokens, SExpr(r.primitives.add(Some(span))) :: parsedSynElems, mapDepL, spanList) )
        }

        case OpType.BinOpType.DIV =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.div(Some(span))) :: parsedSynElems, mapDepL, spanList) )
        case OpType.BinOpType.EQ =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.equal(Some(span))) :: parsedSynElems, mapDepL, spanList) )
        case OpType.BinOpType.GT =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.gt(Some(span))) :: parsedSynElems, mapDepL, spanList) )
        case OpType.BinOpType.LT =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.lt(Some(span))) :: parsedSynElems, mapDepL, spanList) )
        case OpType.BinOpType.MOD => Left(ParseState(remainderTokens, SExpr(
          r.primitives.mod(Some(span))) :: parsedSynElems, mapDepL, spanList) )
        case OpType.BinOpType.MUL => Left(ParseState(remainderTokens, SExpr(
          r.primitives.mul(Some(span))) :: parsedSynElems, mapDepL, spanList) )
        case OpType.BinOpType.SUB => Left(ParseState(remainderTokens, SExpr(
          r.primitives.sub(Some(span))) :: parsedSynElems, mapDepL, spanList) )
        case tok => {
          //println("Das hier kann nicht sein, weil alle Operatoren müsste ich abgedeckt haben. BinOp: '" + tok +
          //  "' is no BinOperator!")
          Right(NotCorrectToken(BinOp(op, span), "BinOp", "BinOp"))
        }
      }
      case tok => {
        println("BinOp: '" + tok + "' is no BinOperator!")
        Right(NotCorrectToken(tok, "BinOp", "BinOp"))
      }
    }
    (p,errorList)
  }


  def parseComma(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    //println("parseComma: " + parseState)
    val ParseState(tokens, parsedSynElems, mapDepL, spanList)  = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Comma(_) => (Left(ParseState(remainderTokens, parsedSynElems, mapDepL, spanList) ),errorList)
      case tok => (Right(NotCorrectToken(tok, "Comma", "Comma")),errorList)
    }
  }



  def parseDepArrow(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val ParseState(tokens, parsedSynElems, mapDepL, spanList)  = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case DepArrow(_) => (Left(ParseState(remainderTokens, parsedSynElems, mapDepL, spanList) ),errorList)
      case tok => (Right(NotCorrectToken(tok, "DepArrow", "DepArrow")),errorList)
    }
  }

  def parseColon(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val ParseState(tokens, parsedSynElems, mapDepL, spanList)  = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Colon(_) => (Left(ParseState(remainderTokens, parsedSynElems, mapDepL, spanList) ),errorList)
      case tok => (Right(NotCorrectToken(tok, "Colon", "Colon")),errorList)
    }
  }

  def parseDot(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val ParseState(tokens, parsedSynElems, mapDepL, spanList)  = parseState
    isMinLen(tokens, 2, "Dot") match{
      case Some(e) => return (Right(e), errorList)
      case None => None
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Dot(_) => (Left(ParseState(remainderTokens, parsedSynElems, mapDepL, spanList) ),errorList)
      case tok => (Right(NotCorrectToken(tok, "Dot", "Dot")),errorList)
    }
  }

  def parseNat(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val ParseState(tokens, parsedSynElems, mapDepL, spanList)  = parseState
    val nextToken :: remainderTokens = tokens

    val p = nextToken match {
      case NatNumber(number, spanNat) => Left(ParseState(remainderTokens, SNat(NNumber(number:rt.Nat),spanNat)::parsedSynElems,
        mapDepL, spanList) )
      case TypeIdentifier(name, spanTypeIdentifier) =>Left(ParseState(remainderTokens,
        SNat(NIdentifier(rt.NatIdentifier(name)), spanTypeIdentifier)::parsedSynElems, mapDepL, spanList) )
      case tok => Right(NotCorrectToken(tok, "NatNumber or TypeIdent", "Nat"))
    }
    (p,errorList)
  }

  def parseData(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val ParseState(tokens, parsedSynElems, mapDepL, spanList)  = parseState
    val nextToken :: remainderTokens = tokens

    val p = nextToken match {
      case TypeIdentifier(name, sp) =>Left(ParseState(remainderTokens,
        SData(DIdentifier(rt.DataTypeIdentifier(name)), sp)::parsedSynElems, mapDepL, spanList) )
      case a => Right(NotCorrectToken(a, "TypeIdent", "Data"))
    }
    (p,errorList)
  }

  def parseLeftBracket(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val ParseState(tokens, parsedSynElems, mapDepL, spanList)  = parseState
    isMinLen(tokens, 2, "LeftBracket") match{
      case Some(e) => return (Right(e),errorList)
      case None => None
    }
    val nextToken :: remainderTokens = tokens

    val p = nextToken match {
      case LBracket(_) => Left(ParseState(remainderTokens, parsedSynElems, mapDepL, spanList) )
      case tok => Right(NotCorrectToken(tok, "LBracket", "LBracket"))
    }
    (p,errorList)
  }

  def parseRightBracket(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val ParseState(tokens, parsedSynElems, mapDepL, spanList)  = parseState
    val nextToken :: remainderTokens = tokens

    val p = nextToken match {
      case RBracket(_) => Left(ParseState(remainderTokens, parsedSynElems, mapDepL, spanList) )
      case tok => Right(NotCorrectToken(tok, "RBracket", "RBracket"))
    }
    (p,errorList)
  }

  def parseLeftParentheses(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val ParseState(tokens, parsedSynElems, mapDepL, _)  = parseState
    val nextToken :: remainderTokens = tokens

    val p  =nextToken match {
      case LParentheses(sp) => Left(ParseState(remainderTokens, parsedSynElems, mapDepL, Some(sp::Nil)))
      case tok => Right(NotCorrectToken(tok, "LParentheses", "LParentheses"))
    }
    (p,errorList)
  }

  def parseRightParentheses(inputEPState: InputEPState): OutputEPState = {
    val (parseState,errorList) = inputEPState
    val ParseState(tokens, parsedSynElems, mapDepL, spanList)  = parseState
    val nextToken :: remainderTokens = tokens

    val spanL = spanList match{
      case None => throw new IllegalStateException("The spanList should not be None")
      case Some(l) => l match {
        case spLeftPar :: Nil => spLeftPar
        case _ => throw new IllegalStateException("The list in parseRightParentheses: '" + l + "' has not the correct form")
      }
    }

    val p = nextToken match {
      case RParentheses(sp) => Left(ParseState(remainderTokens, parsedSynElems, mapDepL, Some(sp::spanL ::Nil)) )
      case tok => Right(NotCorrectToken(tok, "RParentheses", "RParentheses"))
    }
    (p,errorList)
  }


  //_________________________________________________________Expres
}