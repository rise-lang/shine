package parser //old branch 17. Dezember 2020

import rise.{core => r, openCL => o}
import r.{DSL => rd, primitives => rp, semantics => rS, types => rt}
import o.{primitives => op}
import parser.ErrorMessage.{ErrorList, NoKindWithThisName, NotAcceptedScalarType, NotCorrectKind, NotCorrectSynElem, NotCorrectToken, PreAndErrorSynElems, SynListIsEmpty, TokListIsEmpty, TokListTooSmall, UsedOrFailedRule, isFailed, isMatched, isParsing}
import rise.core.DSL.ToBeTyped
import rise.core.DSL.Type.TypeConstructors
import rise.core.ForeignFunction
import rise.core.types.{DataType, DepFunType, PairType, TypePlaceholder, f32, vec}

import scala.collection.mutable

object parse {

  val SpanPlaceholder = Span(FileReader("src/test/scala/parser/readFiles/Placeholder.rise"), Range(Location(0,0), Location(0,0)))


  def giveEveryExpr(map:MapFkt):MapExpr={
    val m= new MapExpr
    map.foreach{
      case (name, elems) => elems match {
        case HMExpr(e) => m.update(name, e.toExprWithMapFkt(map))
        case HMType(t) => { //error: Every fkt should be initialised yet
          throw new RuntimeException("The function '"+name+"' is not implemented: "+ t)
        }
        case HMNat(n) => //ignore
      }
    }
    m
  }
  /*
   * Precondition: Only valid Tokens in tokenList.
   */
  def apply(tokenList: List[Token]): MapExpr = {
    val parseState: ParseState = ParseState(tokenList, Nil, None, None, Nil)
    val shineLambda: MapFkt = parseTypAnnotatedIdentAndThenNamedExprAndOtherTypAnnotatedIdens(parseState) match {
      case Left(map) => map
      case Right(errorOrState) => {
        println(errorOrState)
        throw new RuntimeException("failed parsing : " + errorOrState)
      }
    }
    println("parse: " + shineLambda)
    val res = giveEveryExpr(shineLambda)
    println("everyExpr: " + res)
    res
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

  final case class SSeq(seq:mutable.Seq[String], span:Span) extends SyntaxElement

  sealed trait RiseKind //Todo: Scoping einbauen, also Kind nennen und Token explizit immer hinzufügen

  final case class RData() extends RiseKind

  final case class RNat() extends RiseKind

  final case class RAddrSpace() extends RiseKind

  sealed trait HashMapElems
  final case class HMExpr(e:rd.ToBeTyped[r.Expr]) extends HashMapElems
  final case class HMType(t:r.types.Type) extends HashMapElems
  final case class HMNat(n:SNat) extends HashMapElems

  //Todo: if I have Identifier, I have to get the right Span and the Span is differntly each time
  type MapExpr = mutable.HashMap[String, r.Expr]
  type MapFkt = mutable.HashMap[String, HashMapElems]
  type MapDepL = mutable.HashMap[String, RiseKind]
  type BracesSpan = Option[List[Span]]
  type OutputEPState = (Either[ParseState, PreAndErrorSynElems], ErrorList)
  type InputEPState = (ParseState, ErrorList)

  final case class ParseState(tokenStream: List[Token], parsedSynElems: List[SyntaxElement],
                              mapDepL: Option[MapDepL], spanList: BracesSpan, argumentsTypes:List[rt.Type])

  implicit class ParseStatePipe(val OutputEPState: OutputEPState) extends AnyVal {
    def |>(f: InputEPState => OutputEPState): OutputEPState = {
      val (ps, errorList) = OutputEPState
      //println("|> : " + ps)
      ps match {
        case Left(p) => f((p,errorList))
        case Right(e) => (Right(e),errorList)//(Right(e),errorList.add(e))
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
            rightF((ps,newEL))//rightF((ps,newEL.add(e)))
          }
          case (Left(resPs),newEL) => (Left(resPs),newEL)
        }
    }
  }


  //_________________________________________________________Lambda


  def parseBackslash(inputEPState:InputEPState): OutputEPState = {
    val whatToParse = "Backslash"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, parsedExprs, mapDepL, spanList, argumentsTypes) = parseState
    if (tokens.isEmpty) {
      val e = TokListIsEmpty(SpanPlaceholder, "Backslash")
      return (Right(e),errorList.add(e))
    }
    val nextToken :: remainderTokens = tokens

    val spanL = nextToken match {
      case Backslash(span) => spanList match {
        case None => span :: Nil
        case Some(l) => span :: l
      }
      case tok => {
        val e = NotCorrectToken(tok, "Backslash", "Backslash")
        return (Right(e),errorList.add(e))
      }
    }
    (Left(ParseState(remainderTokens, parsedExprs,mapDepL, Some(spanL), argumentsTypes)),
      errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
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
      case "updateP" => SExpr(update)
      case "calcAccP" => SExpr(calcAcc)

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

  def parseIdent(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "Ident"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, parsedSynElems, mapDepL, spanList, argumentsTypes) = parseState
    if (tokens.isEmpty) {
      val e = ErrorMessage.TokListIsEmpty(SpanPlaceholder, "Ident")
      return (Right(e),errorList.add(e))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Identifier(name, span) => {
        matchPrimitiveOrIdentifier(name, span) match {
          case SLet(span) => (Left(ParseState(remainderTokens, SLet(span) :: parsedSynElems,
            mapDepL, spanList, argumentsTypes)),errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
          case SIntToExpr(name, span) => (Left(ParseState(remainderTokens, SIntToExpr(name, span) :: parsedSynElems,
            mapDepL, spanList, argumentsTypes)),errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
          case SExpr(r.Identifier(_)) => {
            val ident = SExpr(r.Identifier(name)(rt.TypePlaceholder, Some(span)))
            (Left(ParseState(remainderTokens, ident :: parsedSynElems,mapDepL, spanList, argumentsTypes)),
              errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
          }
          case SExpr(prim) => {
            if (prim.span.isEmpty) {
              throw new IllegalStateException("The span of '" + prim + "' is empty")
            }
            (Left(ParseState(remainderTokens, SExpr(prim) :: parsedSynElems,mapDepL, spanList, argumentsTypes)),
              errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
          }
          case otherSyntaxElement => throw new IllegalStateException("The Syntax Element '" + otherSyntaxElement +
            "' was not expected from matchPrimitiveOrIdentifer")
        }
      }
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, "Ident", "Ident")
        (Right(e),errorList.add(e))
      }
    }
  }

  def parseTypeIdentToCorrectForm(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "TypeIdent"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseTypeIdentToCorrectForm: " + parseState)
    parseState.tokenStream.head match {
      case TypeIdentifier(name, span) => parseState.mapDepL.get.get(name) match {
        case None => {
          val e = NoKindWithThisName(TypeIdentifier(name, span), "TypeIdent")
          (Right(e),errorList.add(e))
        }
        case Some(RNat()) =>
          (Left(ParseState(parseState.tokenStream.tail, SNat(NIdentifier(
            rt.NatIdentifier(name)), span) :: parseState.parsedSynElems, parseState.mapDepL, parseState.spanList, parseState.argumentsTypes)),
            errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
        case Some(RData()) =>
          (Left(ParseState(parseState.tokenStream.tail, SData(DIdentifier(
            rt.DataTypeIdentifier(name)), span) :: parseState.parsedSynElems, 
            parseState.mapDepL, parseState.spanList, parseState.argumentsTypes)),errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
        case Some(RAddrSpace()) => throw new IllegalStateException("DepAddrSpace is not implemented yet")
      }
      case t => {
        val e = ErrorMessage.NotCorrectToken(t, "TypeIdent", "TypeIdent")
        (Right(e),errorList.add(e))
      }
    }
  }

  def parseTypeIdent(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "TypeIdent"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseTypeIdent: " + parseState)
    val ParseState(tokens, parsedSynElems, mapDepL, spanList, argumentsTypes) = parseState
    if (tokens.isEmpty) {
      val e = ErrorMessage.TokListIsEmpty(SpanPlaceholder, "TypeIdent")
      return (Right(e),errorList.add(e))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case TypeIdentifier(name, spanId) => (Left(ParseState(remainderTokens, SType(rt.TypeIdentifier(name), spanId) :: parsedSynElems,
       mapDepL, spanList, argumentsTypes)),errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
      case tok => {
        val e =ErrorMessage.NotCorrectToken(tok, "TypeIdent", "TypeIdent")
        (Right(e),errorList.add(e))
      }
    }
  }

  private def getScalarType(typ: ConcreteType): Option[rt.DataType] = typ match {
    case ShortTyp() => Some(rt.i8)
    case IntTyp() => Some(rt.i32)
    case FloatTyp() => Some(rt.f32)
    case DoubleType() => Some(rt.f64)
    case BoolType() => Some(rt.bool)
    case NatTyp() => Some(rt.NatType)
    case notAtype => {
      //println("                        This is not an accepted Type: " + notAtype)
      None
    }
  }

  def parseMaybeTypeAnnotation(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "MaybeTypeAnnotation"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, parsedSynElems,  mapDepL, spanList, argumentsTypes) = parseState
    val colonToken :: typeTokenWithremainderTokens = tokens

    colonToken match {
      case Colon(_) => {
        //if a type Annotation exist, we set the type new of the Identifier
        parseType((ParseState(typeTokenWithremainderTokens, Nil, mapDepL, spanList, argumentsTypes), errorList)) match {
          case (Right(e),errorL) => (Right(e),errorL.add(UsedOrFailedRule(isFailed(), whatToParse)))
          case (Left(newPS),errorL) => if (newPS.parsedSynElems.length == 1) {
            return (Left(ParseState(newPS.tokenStream, newPS.parsedSynElems.head :: parsedSynElems,
              newPS.mapDepL, spanList, argumentsTypes)),errorL.add(UsedOrFailedRule(isMatched(), whatToParse)))
          } else {
            throw new IllegalStateException(
              "It should only be one argument in the end of the computing of the Type exist")
          }
        }
      }
      case _ => (Left(parseState),errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
    }
  }

  def parseTypeAnnotation(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "TypeAnnotation"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, _,  mapDepL, spanList, argumentsTypes) = parseState
    val colonToken :: typeToken :: remainderTokens = tokens

    colonToken match {
      case Colon(_) => {
        //if a type Annotation exist, we set the type new of the Identifier
        typeToken match {
          case ScalarType(typ, sp) => {
            //Todo: Complex Alg for Types Parsing
            val t = getScalarType(typ)
            t match {
              case None => {
                val e = NotAcceptedScalarType(sp, typ,"TypeAnnotation")
                (Right(e),errorList.add(e))
              }
              case Some(parsedType) => (Left(ParseState(remainderTokens, SType(parsedType, sp) :: parseState.parsedSynElems,
                 mapDepL, spanList, argumentsTypes)),errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
            }
          }
          case notAtype => {
            val e = ErrorMessage.NotCorrectToken(typeToken, "ScalarType", "TypeAnnotation")
            (Right(e),errorList.add(e))
          }
        }
      }
      case notAColon => {
        val e = ErrorMessage.NotCorrectToken(notAColon, "Colon", "TypeAnnotation")
        (Right(e),errorList.add(e))
      }
    }
  }

  def parseVecType(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "VecType"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, _, mapDepL, spanList, argumentsTypes) = parseState
    val inputType :: remainderTokens = tokens

    //println("parseVecType: " + parseState)
    inputType match {
      case VectorType(len, concreteType, sp) => {
        //println("ConcreteType was in parseVecType: " + concreteType + " , with length: " + len)
        //Todo: Complex Alg for Type Parsing
        val parsedInType = getScalarType(concreteType)
        val inT = parsedInType match {
          case None => {
            val e = ErrorMessage.NotAcceptedScalarType(sp, concreteType,"VecType")
            return (Right(e),errorList.add(e))
          }
          case Some(value) => value
        }
        (Left(ParseState(remainderTokens, SType(rt.VectorType(len, inT), sp) :: parseState.parsedSynElems,
          mapDepL, spanList, argumentsTypes)),errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
      }
      case notAtype => {
        val e = ErrorMessage.NotCorrectToken(notAtype, "VectorType", "VecType")
        (Right(e),errorList.add(e))
      }
    }
  }

  def parseScalarType(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "ScalarType"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, _, mapDepL, spanList, argumentsTypes) = parseState
    val inputType :: remainderTokens = tokens

    //println("parseTypeWithoutArrow: " + parseState)
    inputType match {
      case ScalarType(typ, spanTyp) => {
        //println("Type was in parseTypeWithoutArrow parsed: " + typ + " ; " + spanTyp.end)
        //Todo: Complex Alg for Type Parsing
        val parsedInType = getScalarType(typ)
        val inT = parsedInType match {
          case None => {
            val e = ErrorMessage.NotAcceptedScalarType(spanTyp, typ,"ScalarType")
            return (Right(e), errorList.add(e))
          }
          case Some(value) => value
        }
        (Left(ParseState(remainderTokens, SType(inT, spanTyp) :: parseState.parsedSynElems,  mapDepL, spanList, argumentsTypes)),
          errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
      }
      case notAtype => {
        val e = ErrorMessage.NotCorrectToken(notAtype, "ScalarType", "ScalarType")
        (Right(e), errorList.add(e))
      }
    }
  }

  def parseKindWithDepArrowAfterForDepLambda(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "KindWithDepArrow"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseKind: " + parseState)
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
            case ki => {
              val e = NotCorrectKind(sp, ki, "KindWithDepArrow")
              return (Right(e), errorList.add(e))
            }
          }
          parseState.mapDepL match {
            case None => throw new IllegalStateException("mapDepL is None")
            case Some(mL)=> mL.update(nameOfIdentifier, ki)
          }
          //println("Kind was in parseDepFunctionType parsed: " + concreteKind + " , mapDepL: "+ parseState.mapDepL)
          parseMaybeAppExpr((ParseState(tokens.tail.tail, Nil, parseState.mapDepL, spanList, parseState.argumentsTypes), errorList)) match {
            case (Right(e),errorL) => (Right(e),errorL.add(UsedOrFailedRule(isFailed(), whatToParse)))
            case (Left(pS),errorL) => {
              //println("In the middle of parseDepFunctionType: " + pS)
              if (pS.parsedSynElems.tail.nonEmpty) throw new IllegalStateException("ParsedSynElems.tail has to be empty!") //Todo: write test
              val depLam = pS.parsedSynElems.head match {
                case SExpr(outT) => {
                  val span = outT.span match {
                    case None => throw new IllegalStateException("Span should not be None in DepLambdafkt")
                    case Some(sp) => {
                      val spanZW = spanType + sp
                      //println("Span of spanType123: (" + spanZW.begin + "," + spanZW.end + ") = (" + sp.begin + "," + sp.end + ") + (" + spanType.begin + ")" + spanType.end + ") ; " + nameOfIdentifier)
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
                case wrongSynElem => {
                  val e = NotCorrectSynElem(wrongSynElem, "SExpr", "KindWithDepArrow")
                  return (Right(e),errorL.add(e))
                }
              }

              (Left(ParseState(pS.tokenStream, depLam :: newPS, pS.mapDepL , spanList, pS.argumentsTypes)),
                errorL.add(UsedOrFailedRule(isMatched(), whatToParse)))
            }
          }
        }
        case notAnDepArrow => {
          val e = ErrorMessage.NotCorrectToken(notAnDepArrow, "DepArrow", "KindWithDepArrow")
          (Right(e),errorList.add(e))
        }
      }
      case t => {
        val e = ErrorMessage.NotCorrectToken(t, "Kind", "KindWithDepArrow")
        (Right(e),errorList.add(e))
      }
    }
  }

//Todo: combine with function above
  def parseKindWithDepArrowAfterForDepFunctionType(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "KindWithDepArrow"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseKind: " + parseState)
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
            case ki => {
              val e = ErrorMessage.NotCorrectKind(span, ki, "KindWithDepArrow")
              return (Right(e),errorList.add(e))
            }
          }
          //println("Kind was in parseDepFunctionType parsed: " + concreteKind)
          parseType((ParseState(tokens.tail.tail, Nil,  parseState.mapDepL, parseState.spanList, parseState.argumentsTypes),errorList)) match {
            case (Right(e),errorL) => (Right(e),errorL.add(UsedOrFailedRule(isFailed(), whatToParse)))
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
                    case ki => {
                      val e = ErrorMessage.NotCorrectKind(sp, ki, "KindWithDepArrow")
                      return (Right(e),errorL.add(e))
                    }
                  }
                }
                case wrongSynEl => {
                  val e = NotCorrectSynElem(wrongSynEl, "SType", "KindWithDepArrow")
                  return (Right(e),errorL.add(e))
                }
              }
              (Left(ParseState(pS.tokenStream, depFun :: newPS,  pS.mapDepL, pS.spanList, pS.argumentsTypes)),
                errorL.add(UsedOrFailedRule(isMatched(), whatToParse)))
            }
          }
        }
        case notAnDepArrow => {
          val e = ErrorMessage.NotCorrectToken(notAnDepArrow, "DepArrow", "KindWithDepArrow")
          (Right(e),errorList.add(e))
        }
      }
      case t => {
        val e = ErrorMessage.NotCorrectToken(t, "Kind", "KindWithDepArrow")
        (Right(e),errorList.add(e))
      }
    }
  }


  private def isMinLen(l:List[Token], minLen: Int, whatToParse:String):Option[PreAndErrorSynElems]={
    if (l.length < minLen) {
      if(l.isEmpty){
        return Some(ErrorMessage.TokListIsEmpty(SpanPlaceholder, whatToParse))
      }
      return Some(TokListTooSmall(l, minLen, whatToParse))
    }
    return None
  }

  def parseDepFunctionType(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "DepFunctionType"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseDepFunctionType: " + parseState)
    val ParseState(tokens, _,  _, _, _) = parseState

    isMinLen(tokens, 3, "DepFunctionType") match{
      case Some(e) => return (Right(e),errorList.add(e))
      case None => None
    }

    val (p, eL) =
      (Left(parseState),errorList) |>
        parseTypeIdent |>
        parseColon |>
        parseKindWithDepArrowAfterForDepFunctionType
    p match {
      case Right(e) => (Right(e),eL.add(UsedOrFailedRule(isFailed(), whatToParse)))
      case Left(pa) => (Left(ParseState(pa.tokenStream, pa.parsedSynElems,  pa.mapDepL, parseState.spanList,pa.argumentsTypes)),
        eL.add(UsedOrFailedRule(isMatched(), whatToParse)))
    }
  }

  def parseDepLambda(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "DepLambda"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseDepFunctionType: " + parseState)
    val ParseState(tokens, _,  _, _, _) = parseState
    isMinLen(tokens, 3, "DepLambda") match{
      case Some(e) => return (Right(e),errorList.add(e))
      case None => None
    }

    val (p, eL) =
      (Left(parseState),errorList) |>
        parseBackslash |>
        parseTypeIdent |>
        parseColon |>
        parseKindWithDepArrowAfterForDepLambda
    p match {
      case Right(e) => (Right(e),eL.add(UsedOrFailedRule(isFailed(), whatToParse)))
      case Left(pa) => (Left(ParseState(pa.tokenStream, pa.parsedSynElems,  pa.mapDepL, parseState.spanList, pa.argumentsTypes)),
        eL.add(UsedOrFailedRule(isMatched(), whatToParse)))
    }
  }

  def parseArrow(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "Arrow"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, parsedExprs, mapDepL, spanList, argumentsTypes) = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Arrow(_) =>
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, "Arrow", "Arrow")
        return (Right(e),errorList.add(e))
      }
    }

    (Left(ParseState(remainderTokens, parsedExprs, mapDepL, spanList, argumentsTypes)),
      errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
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
      case SSeq(seq, _) => throw new RuntimeException("List should't have any Seq at this position! " + seq)
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
        //println("\n\nspan in SExprClutched: " + span)
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
      case SSeq(seq, _) => throw new RuntimeException("List should't have any Seq at this position! " + seq)
    }

    (e, synE)
  }

  private def combineExpressionsDependent(synElemList: List[SyntaxElement], mapDepL: MapDepL): r.Expr = {
    var (e, synE) = combineExpressionDependentFirsExpr(synElemList, mapDepL)
    //println("I will combine Expressions in combineExpressionsDependent: " + synE + " <::> " + e)
    while (!synE.isEmpty) {
      val r = combineExpressionsDependentOneStep(e, synE, mapDepL, None)
      e = r._1
      synE = r._2
    }
    //println("I have combined the Expressions in combineExpressionsDependent: " + e + " ; " + e.span.get.end)
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
        val ps: ParseState = ParseState(remainderTokens, Nil, Some(new MapDepL), parseState.spanList, parseState.argumentsTypes)
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
      //println("tokens: " + tokenList + " ,MapFkt: " + mapFkt)
      tokenList match {
        case BeginTypAnnotatedIdent(_) :: remainderTokens => {
          val p: ParseState = ParseState(remainderTokens, Nil, Some(new MapDepL), parseState.spanList, parseState.argumentsTypes)
          val psNew = parseTypAnnotatedIdent(p, mapFkt)
          psNew match {
            case Left(tokens) => {
              tokenList = tokens
            }
            case Right(e) => return Right(e)
          }
        }
        case BeginNamedExpr(_) :: remainderTokens => {
          val p: ParseState = ParseState(remainderTokens, Nil, Some(new MapDepL), parseState.spanList, parseState.argumentsTypes)
          //println("p: " + p)
          val psNew = parseNamedExpr(p, mapFkt)
          //println("psNew: " + psNew)
          psNew match {
            case Left(tokens) => {
              tokenList = tokens
            }
            case Right(errorL) => return Right(errorL)
          }
        }
        case BeginForeignFct(_) :: remainderTokens => {
          val p: ParseState = ParseState(remainderTokens, Nil, Some(new MapDepL), parseState.spanList, parseState.argumentsTypes)
          val psNew = parseForeignFct(p, mapFkt)
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

  def parseNamedExprWithNormalExpr(inputEPState: InputEPState):OutputEPState ={
    val (ps, errorList) = inputEPState
    (Left(ps),errorList) |>
      parseEqualsSign |>
      parseMaybeAppExpr
  }
//  def parseNamedExprWithOnlyOneNumber(inputEPState: InputEPState):OutputEPState ={
//    val (ps, errorList) = inputEPState
//    (Left(ps),errorList) |>
//      parseEqualsSign |>
//      parseNumber
//  }

  def parseNamedExprWithOnlyOneNat(inputEPState: InputEPState):OutputEPState ={
    val (ps, errorList) = inputEPState
    (Left(ps),errorList) |>
      parseEqualsSign |>
      parseNat
  }

  def iterateForeignFctBodyLines(inputEPState: InputEPState):OutputEPState = {
    val whatToParse = "iterateForeignFctBodyLines"
    val (parseState, errorList) = inputEPState
    if(parseState.tokenStream.head.isInstanceOf[RBraces]){
      return (Left(parseState), errorList)
    }
    (Left(parseState), errorList) |>
      parseBodyLine |> iterateForeignFctBodyLines
  }

  def iterateForeignFctParameterList(inputEPState: InputEPState):OutputEPState = {
    val whatToParse = "iterateForeignFctParameterList"
    val (parseState, errorList) = inputEPState
    if(parseState.tokenStream.head.isInstanceOf[RParentheses]){
      return (Left(parseState), errorList)
    }
    (Left(parseState), errorList) |>
      parseComma |> parseIdent |> iterateForeignFctParameterList
  }
private def subGetSequenceStrings(seq:mutable.Seq[String], parsedSynElems:List[SyntaxElement]):Either[(mutable.Seq[String], Span), PreAndErrorSynElems]={
  val whatToParse = "SequenceOfNames"
  parsedSynElems match {
    case SExpr(id@r.Identifier(name))::list => {
      val s  = seq.appended(name +"\n")
      println("SequenzString: "+ s)
      subGetSequenceStrings(s,list) match{
        case Right(e) => if(e.isInstanceOf[SynListIsEmpty]){
          println("SequenzStringEnd: "+ s)
          Left(s, id.span.get)
        }else{
          Right(e)
        }
        case Left((seqN, spanN)) => Left(seqN, id.span.get + spanN)
      }
    }
    case e :: list => {
      Right(NotCorrectSynElem(e, "Identifier", whatToParse))
    }
    case Nil => Right(ErrorMessage.SynListIsEmpty(SpanPlaceholder, whatToParse))
  }
}

  private def getSequenceOfStrings(seq:mutable.Seq[String], parsedSynElems:List[SyntaxElement]):Either[(mutable.Seq[String], Span), PreAndErrorSynElems]={
    subGetSequenceStrings(seq, parsedSynElems.reverse)
  }

  def parseForeignFctBodyLinesLoop(inputEPState: InputEPState):OutputEPState  = {
    val whatToParse = "iterateForeignFctBodyLines"
    val (parseState, errorList) = inputEPState
    val seq = mutable.Seq[String]()
    if(parseState.tokenStream.head.isInstanceOf[RBraces]){
      return (Left(ParseState(parseState.tokenStream, SSeq(seq,
        parseState.tokenStream.head.s)::parseState.parsedSynElems,
        parseState.mapDepL, parseState.spanList, parseState.argumentsTypes)), errorList)
    }
    val newPS = (Left(ParseState(parseState.tokenStream, Nil, parseState.mapDepL, parseState.spanList, parseState.argumentsTypes)),
      errorList.add(UsedOrFailedRule(isParsing(), whatToParse))) |>
      iterateForeignFctBodyLines

    newPS match {
      case (Right(e), errorList) => (Right(e), errorList.add(e))
      case (Left(p), errorList) => {
        val (seqN, seqSpan) = getSequenceOfStrings(seq, p.parsedSynElems) match {
          case Left(value) => {
            errorList.add(UsedOrFailedRule(isMatched(), whatToParse))
            value
          }
          case Right(e) => return (Right(e), errorList.add(e))
        }
        (Left(ParseState(p.tokenStream, SSeq(seqN,
          seqSpan)::parseState.parsedSynElems,
          p.mapDepL, p.spanList,p.argumentsTypes)), errorList)
      }
    }
  }


  def parseForeignFctParameterIterationLoop(inputEPState: InputEPState):OutputEPState  = {
    val whatToParse = "ForeignFctParameterIterationLoop"
    val (parseState, errorList) = inputEPState
    val seq = mutable.Seq[String]()
    if(parseState.tokenStream.head.isInstanceOf[RParentheses]){
      return (Left(ParseState(parseState.tokenStream, SSeq(seq,
        parseState.tokenStream.head.s)::parseState.parsedSynElems,
        parseState.mapDepL, parseState.spanList, parseState.argumentsTypes)), errorList)
    }
//    println("Hey, here is intern FctParamList\n")
    val newPS = (Left(ParseState(parseState.tokenStream, Nil, parseState.mapDepL, parseState.spanList, parseState.argumentsTypes)),
      errorList.add(UsedOrFailedRule(isParsing(), whatToParse))) |>
      parseIdent |> iterateForeignFctParameterList

    newPS match {
      case (Right(e), errorList) => (Right(e), errorList)
      case (Left(p), errorList) => {
        val (seqN,seqSpan) = getSequenceOfStrings(seq, p.parsedSynElems) match {
          case Left(value) => value
          case Right(e) => return (Right(e), errorList.add(e))
        }
        (Left(ParseState(p.tokenStream, SSeq(seqN,
          seqSpan)::parseState.parsedSynElems,
          p.mapDepL, p.spanList, p.argumentsTypes)), errorList.add(UsedOrFailedRule(isMatched(),whatToParse)))
      }
    }
  }

  def parseForeignFctParameterList(inputEPState: InputEPState):OutputEPState  = {
    val whatToParse = "ForeignFctParameterList"
    //Seq("p1", "p2", "deltaT", "espSqr", "acc")
    val (parseState, errorList) = inputEPState
    val ps = ParseState(parseState.tokenStream, Nil , parseState.mapDepL, parseState.spanList,parseState.argumentsTypes)
    val newPS = (Left(ps), errorList.add(UsedOrFailedRule(isParsing(), whatToParse))) |>
      parseLeftParentheses |> parseForeignFctParameterIterationLoop |> parseRightParentheses

    newPS match{
      case (Right(e), errorList) => (Right(e), errorList.add(UsedOrFailedRule(isFailed(), whatToParse)))
      case (Left(p), errorList) => (Left(p), errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
    }
  }

  def parseForeignFctBody(inputEPState: InputEPState):OutputEPState  = {
    val whatToParse = "ForeignFctParameterBody"
    //Seq("p1", "p2", "deltaT", "espSqr", "acc")
    val (parseState, errorList) = inputEPState
    val newPS = (Left(parseState), errorList.add(UsedOrFailedRule(isParsing(), whatToParse))) |>
      parseLeftBraces |>
      parseForeignFctBodyLinesLoop |>
      parseRightBraces

    newPS match{
      case (Right(e), errorList) => (Right(e), errorList.add(UsedOrFailedRule(isFailed(), whatToParse)))
      case (Left(p), errorList) => (Left(p), errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
    }
  }

  private def giveAsOneString(seq: Seq[String]):String={
    var body:String = ""
    for(col <- seq){
      body = body + col
    }
    body
  }

  def parseForeignFct(parseState: ParseState, mapFkt: MapFkt): Either[List[Token], ErrorList] = {
    require(parseState.parsedSynElems.isEmpty, "parseState is not empty, but nothing should be in it yet")
    val whatToParse = "ForeignFct"
    val (psLambdaOld, errorList) =
      (Left(parseState), ErrorList().add(UsedOrFailedRule(isParsing(), whatToParse))) |>
        parseForeignFctKeyWord |>
        parseIdent
    val (ps, identifierFkt, typeOfFkt): (ParseState, r.Identifier, r.types.Type) = psLambdaOld match {
      case Right(e) => return Right(errorList.add(UsedOrFailedRule(isFailed(), whatToParse)))
      case Left(p) => {
        p.parsedSynElems.head match {
          case SExpr(id@r.Identifier(n)) =>
            mapFkt.get(n) match {
              case None => {
                //println("Identifier doesn't exist: " + n + " , " + psLambdaOld)
                throw new IllegalStateException("We want to parse an NamedExpr for " + n +
                  " but this Identifier is not declared yet!")
              }
              case Some(HMExpr(e)) => throw new IllegalStateException("The Lambda-Fkt should't be initiated yet!: " + e)
              case Some(HMNat(n)) => throw new IllegalStateException("Name of a Nat: " + n)
              case Some(HMType(typeFkt)) =>
                (ParseState(p.tokenStream, parseState.parsedSynElems, p.mapDepL, p.spanList, p.argumentsTypes),
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
          case SSeq(seq, _) => throw new RuntimeException("List should't have any Seq at this position! " + seq)
        }
      }
    }

    val (psNamedExprBefore, newEL) = {
      (Left(ps), errorList) |> parseForeignFctParameterList |> parseForeignFctBody
    }
    val (tokenList, synElemList, mapDepL) = psNamedExprBefore match {
      case Right(e) => {
        return Right(newEL.add(UsedOrFailedRule(isFailed(), whatToParse)))
      }
      case Left(p) => {
        p.tokenStream match {

          case EndForeignFct(span) :: remainderTokens => {
            mapFkt.get(identifierFkt.name) match {
              case None => throw new IllegalStateException("Identifier seems not to be in the Map: " +
                identifierFkt.name + " , " + mapFkt)
              case Some(HMNat(n)) => throw new IllegalStateException("Name of a Nat: " + n)
              case Some(HMExpr(e)) => throw new IllegalStateException("The Lambda-Fkt should't be initiated yet!: " + e)
              case Some(HMType(t)) =>
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
            throw new IllegalStateException("ForeignFct ends with an EndForeignFct, but we have no EndForeignFct at the end")
          }
        }
      }
    }
    val expr = synElemList match{
      case SSeq(seqBody,spBody)::SSeq(seqParam, spParam)::list => {
        val body = giveAsOneString(seqBody.toSeq)
        val bodyWithBraces ="{\n"+body+"}"
        println("body: '"+ bodyWithBraces+"'" + " ; '"+ seqBody.isEmpty+"'")
        r.ForeignFunction(r.ForeignFunction.Decl(identifierFkt.name,
          Some(ForeignFunction.Def(seqParam.toSeq, bodyWithBraces))))(typeOfFkt, Some(spBody+spParam))
      }
      case SSeq(_,_)::x::list => {
        val e = NotCorrectSynElem(x, "SSeq (second position)", whatToParse)
        return Right(newEL.add(e).add(UsedOrFailedRule(isFailed(), whatToParse)))
      }
      case SSeq(_,sp)::Nil => {
        val e = ErrorMessage.SynListIsEmpty(sp, whatToParse)
        return Right(newEL.add(e).add(UsedOrFailedRule(isFailed(), whatToParse)))
      }
      case x::list => {
        val e = NotCorrectSynElem(x, "SSeq (first position)", whatToParse)
        return Right(newEL.add(e).add(UsedOrFailedRule(isFailed(), whatToParse)))
      }
      case Nil =>{
        val e = ErrorMessage.SynListIsEmpty(SpanPlaceholder, whatToParse)
        return Right(newEL.add(e).add(UsedOrFailedRule(isFailed(), whatToParse)))
      }
    }
    mapFkt.update(identifierFkt.name, HMExpr(rd.ToBeTyped(expr)))
    //println("map updated: " + mapFkt + "\nRemainderTokens: " + tokenList)
    errorList.add(UsedOrFailedRule(isMatched(), whatToParse))
    Left(tokenList)
  }


  def getArgumentTypes(typeOfFkt:rt.Type): List[rt.Type]={
    typeOfFkt match {
      //case rt.FunType(inT1, rt.FunType(inT2, outT)) => inT1::Nil
      case rt.FunType(inT, outT) => inT::getArgumentTypes(outT)
      case DepFunType(x, t) => getArgumentTypes(t)
      case t => Nil //last Element is expected Type and should be ignored
    }
  }
  /*
  top level Lambda expects that the type of the Identifier is defined!

  only use as top level!
   */
  def parseNamedExpr(parseState: ParseState, mapFkt: MapFkt): Either[List[Token], ErrorList] = {
    require(parseState.parsedSynElems.isEmpty, "parseState is not empty, but nothing should be in it yet")
    val whatToParse = "NamedExpr"
    val (psLambdaOld,errorList) =
      (Left(parseState),ErrorList().add(UsedOrFailedRule(isParsing(), whatToParse))) |>
        parseIdent

    val (ps, identifierFkt, typeOfFkt): (ParseState, r.Identifier, r.types.Type) = psLambdaOld match {
      case Right(e) => return Right(errorList.add(UsedOrFailedRule(isParsing(), whatToParse)))
      case Left(p) => {
        p.parsedSynElems.head match {
          case SExpr(id@r.Identifier(n)) =>
            mapFkt.get(n) match {
              case None => {
                //println("Identifier doesn't exist: " + n + " , " + psLambdaOld)
                throw new IllegalStateException("We want to parse an NamedExpr for " + n +
                  " but this Identifier is not declared yet!")
              }
              case Some(HMExpr(e)) => throw new IllegalStateException("The Lambda-Fkt should't be initiated yet!: " + e)
              case Some(HMNat(n)) => throw new IllegalStateException("Name of a Nat: "+ n)
              case Some(HMType(typeFkt)) =>
                (ParseState(p.tokenStream, parseState.parsedSynElems, p.mapDepL, p.spanList, p.argumentsTypes),
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
          case SSeq(seq, _) => throw new RuntimeException("List should't have any Seq at this position! " + seq)
        }
      }
    }
    val argumentList = getArgumentTypes(typeOfFkt)
    val psWithNewArgumentList = ParseState(ps.tokenStream, ps.parsedSynElems, ps.mapDepL, ps.spanList, argumentList)
    val (psNamedExprBefore,newEL) = {
      (Left(psWithNewArgumentList),errorList) |>(parseNamedExprWithOnlyOneNat _||parseNamedExprWithNormalExpr)
    }


    val (tokenList,synElemList, mapDepL) = psNamedExprBefore match {
      case Right(e) => {
        return Right(newEL.add(UsedOrFailedRule(isFailed(), whatToParse)))
      }
      case Left(p) => {
        p.tokenStream match {

          case EndNamedExpr(_) :: remainderTokens => {
            mapFkt.get(identifierFkt.name) match {
              case None => throw new IllegalStateException("Identifier seems not to be in the Map: " +
                identifierFkt.name + " , " + mapFkt)
              case Some(HMNat(n)) => throw new IllegalStateException("Name of a Nat: "+ n)
              case Some(HMExpr(e)) => throw new IllegalStateException("The Lambda-Fkt should't be initiated yet!: " + e)
              case Some(HMType(t)) =>
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

      synElemList.head match {
        case SExprClutched(expr, spanClutch) =>
        case SExpr(expr) =>
        case SLet(span) =>
        case SType(t, span) =>
        case SIntToExpr(name, span) =>
        case SNat(nat, span) => {
          mapFkt.update(identifierFkt.name, HMNat(SNat(nat, span)))
          //println("map updated: " + mapFkt + "\nRemainderTokens: " + tokenList)
          return Left(tokenList)
        }
        case SData(data, span) =>
        case SAddrSpace(addrSpace, span) =>
        case SSeq(seq, _) => throw new RuntimeException("List should't have any Seq at this position! " + seq)
      }
    //println("\n\n\n Before combining the Expr in parseNamedExpr \n\n\n")
    var expr = combineExpressionsDependent(synElemList, mapDepL)
    expr = expr.setType(typeOfFkt)

    //println("expr finished: " + expr + " with type: " + expr.t + "   (should have Type: " + typeOfFkt + " ) ")

    require(expr.span != None, "expr is None!")
//    rd.ToBeTyped(expr) match {
//      case rd.ToBeTyped(e) => require(e.span != None, "expr is now in ToBeTyped without infer None")
//      case _ => throw new IllegalStateException("this should not be happening")
//    }
//    //println("before requre MapFkt: '"+ mapFkt + "' for the Expr: '"+ expr + "' with Type: '"+ expr.t + "'")
//    require(rd.ToBeTyped(expr).toExpr.span != None, "expr is now with ToBeType.toExpr None!")

    mapFkt.update(identifierFkt.name, HMExpr(rd.ToBeTyped(expr)))
    //println("map updated: " + mapFkt + "\nRemainderTokens: " + tokenList)
    Left(tokenList)
  }

  /*
  mapFkt is by reference, and does not to be returned
   */
  def parseTypAnnotatedIdent(parseState: ParseState, mapFkt:MapFkt): Either[List[Token], ErrorList] = {
    require(parseState.parsedSynElems.isEmpty, "parseState is not empty, but nothing should be in it yet")
    val whatToParse = "TypAnnotatedIdent"
    val psLambdaOld =
      (Left(parseState),ErrorList().add(UsedOrFailedRule(isParsing(), whatToParse))) |>
        parseIdent

    val (ps, identifierFkt, eL): (ParseState, r.Identifier, ErrorList) = psLambdaOld match {
      case (Right(e),errorL) => return Right(errorL.add(UsedOrFailedRule(isFailed(), whatToParse)))
      case (Left(p),errorL) => {
        p.parsedSynElems.head match {
          case SExpr(id@r.Identifier(n)) => if (mapFkt.contains(n)) {
            //println("Identifier does already exist: " + n + " , " + psLambdaOld)
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
          case SSeq(seq, _) => throw new RuntimeException("List should't have any Seq at this position! " + seq)
        }
      }
    }

    val psNamedExprBefore = {
      (Left(ParseState(ps.tokenStream, Nil, ps.mapDepL, ps.spanList, ps.argumentsTypes)),eL) |>
        parseDoubleColons |>
        parseType
    }


    val (psNew,_) = psNamedExprBefore match {
      case (Right(e),erL) => return Right(erL.add(UsedOrFailedRule(isFailed(), whatToParse)))
      case (Left(p),erL) => (p,erL.add(UsedOrFailedRule(isMatched(), whatToParse)))
    }

    //println("in the middle of TypAnnotatedIden: " + psNew)
    psNew.tokenStream match {
      case EndTypAnnotatedIdent(_) :: remainderTokens => {
        val (typesList, _) = getTypesInList(psNew.parsedSynElems, None)
        if (typesList.length != 1) {
          throw new IllegalStateException("The TypesList should have lenght 1: " + typesList)
        }
        mapFkt.put(identifierFkt.name, HMType(typesList.head))
        //println("return TypAnnotatedIdent: " + remainderTokens + " <<<<>>>> " + mapFkt)
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
        case SSeq(seq, _) => throw new RuntimeException("List should't have any Seq at this position! " + seq)
      }
    } else {
      spanOp match {
        case None => throw new IllegalStateException("spanOp should not be None")
        case Some(sp) => (Nil, sp)
      }
    }
  }

  def parseType(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "Type"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseType: " + parseState)
    val (p, eL) =
      (Left(parseState),errorList) |>
        (parseDepOrNormalFunctionType _ || parseSimpleType)
    p match {
      case Right(e) => (Right(e),eL.add(UsedOrFailedRule(isFailed(), whatToParse)))
      case Left(pa) => (Left(ParseState(pa.tokenStream, pa.parsedSynElems,  pa.mapDepL, parseState.spanList, pa.argumentsTypes)),
        eL.add(UsedOrFailedRule(isMatched(), whatToParse)))
    }
  }

  def parseDepOrNormalFunctionType(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "DepOrNormalFunctionType"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseType: " + parseState)
    val (p,eL) =
      (Left(parseState),errorList) |>
        (parseDepFunctionType _ || parseFunctionType)
    p match {
      case Right(e) => (Right(e),eL.add(UsedOrFailedRule(isFailed(), whatToParse)))
      case Left(pa) => (Left(ParseState(pa.tokenStream, pa.parsedSynElems,  pa.mapDepL, parseState.spanList, pa.argumentsTypes)),
        eL.add(UsedOrFailedRule(isMatched(), whatToParse)))
    }
  }

  def parseSimpleType(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "SimpleType"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseType: " + parseState)
    val (p, eL) : OutputEPState =
      (Left(parseState),errorList) |>
        (parseBracesExprType _ ||
          parseTupleType || parseIndexType || parseArrayType || parseVecType ||
          parseScalarType || parseData) //Todo: The Function parseTypeIdentToCorrectForm is not good, because it is not clear what we should parse. I have an Function for parseData, but I don't need a function for parseNat, because Nat should not be returned. For ArrayType i am also unsure.
    p match {
      case Right(e) => (Right(e),eL.add(UsedOrFailedRule(isFailed(), whatToParse)))
      case Left(pa) => (Left(ParseState(pa.tokenStream, pa.parsedSynElems,  pa.mapDepL, parseState.spanList, pa.argumentsTypes)),
        eL.add(UsedOrFailedRule(isMatched(), whatToParse)))
    }
  }

  def parseFunctionType(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "FunctionType"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ps = parseSimpleType((parseState,errorList))
    ps match {
      case (Right(e),eL) => (Right(e),eL.add(UsedOrFailedRule(isFailed(), whatToParse)))
      case (Left(p),eL) => if (p.tokenStream.isEmpty) {
        throw new IllegalStateException("Tokens are empty: " + p)
      } else {
        p.tokenStream.head match {
          case Arrow(_) => {
            val (newParse,errorListNew) = parseFunctionType((ParseState(p.tokenStream.tail, Nil,  p.mapDepL, p.spanList, p.argumentsTypes),eL)) match {
              case (Left(pars),erL) => (pars,erL)
              case (Right(e),erL) => return (Right(e),erL.add(UsedOrFailedRule(isFailed(), whatToParse)))
            }
            val (list1, spanList1) = getTypesInList(p.parsedSynElems, None)
            val (list2, spanList2) = getTypesInList(newParse.parsedSynElems, None)
            val typesList: List[r.types.Type] = combineTypes(list1) ::
              combineTypes(list2) :: Nil
            val newType: r.types.Type = combineTypes(typesList)
            (Left(ParseState(newParse.tokenStream, SType(newType, spanList1 + spanList2) :: Nil, p.mapDepL, p.spanList, p.argumentsTypes)),
              errorListNew.add(UsedOrFailedRule(isMatched(), whatToParse)))
          }
          //Todo: Maybe remove already here the EndTypAnnotatedIdent or RBrace from the TokenList
          case EndTypAnnotatedIdent(_) => (Left(p),eL.add(UsedOrFailedRule(isMatched(), whatToParse)))
          case RParentheses(_) => (Left(p),eL.add(UsedOrFailedRule(isMatched(), whatToParse)))
          case a => {
            val e = ErrorMessage.NotCorrectToken(a, "Arrow, EndTypAnn or RParentheses", "FunctionType")
            (Right(e),eL.add(e))
          }
        }
      }
    }
  }

  private def parseCompLoop(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "CompLoop"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseCompLoop: "+ parseState.tokenStream)
    val (p,errL) = (Left(parseState),errorList)|> parseDot |> parseNoAppExpr
    p match{
      case Right(e) => (Right(e),errL.add(UsedOrFailedRule(isFailed(), whatToParse)))
      case Left(ps) => if(ps.tokenStream.head.isInstanceOf[Dot]){
        val (newPS, eL) = (Left(ps),errL) |> parseCompLoop
        (newPS, eL.add(UsedOrFailedRule(isMatched(), whatToParse)))
      }else{
        (Left(ps),errL.add(UsedOrFailedRule(isMatched(), whatToParse)))
      }
    }
  }

  private def getExprAndSpan(syntaxElement: SyntaxElement, parseState: ParseState):Option[(r.Expr, Span)]={
    syntaxElement match {
      case SExpr(expr) => Some((expr, expr.span.get))
      case SExprClutched(expr, sp) => Some((expr, sp))
      case _ => {
        //println("Abort; getExprAndSpan because not accepted beggining types: " + parseState)
        None
      }
    }
  }

  //Todo: Here I have to expect the result of getComposition, so I have to add SComp in the SyntaxElement
  //Todo: and with that I can return this SyntaxElement so that SComp has to look something like this:
  //Todo: SComp = (e1:r.Expr, s2:SyntaxElement, span:Span) and with that I have to create an Lambda like this
  //Todo: r.Lambda(r.freshname(e), r.App(e1,createLambda(s2))(r.TypePlaceholder, Some(span))
  private def parseComp(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "Comp"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseComp: " + parseState)
//    val cpyMapFkt = parseState.mapFkt.clone()
    val (psOld,eL) = (Left(ParseState(parseState.tokenStream, parseState.parsedSynElems.head::Nil,
  parseState.mapDepL, parseState.spanList, parseState.argumentsTypes)),errorList) |>
      parseCompLoop //|> parseNoAppExpr
    //println("Yuhuuuasdf: "+psOld)
    psOld match {
      case Right(e) => {
//        parseState.mapFkt.clear()
//        cpyMapFkt.foreach{case (n,t)=>parseState.mapFkt.update(n,t)}
        (Right(e),eL.add(UsedOrFailedRule(isFailed(), whatToParse)))
      }
      case Left(ps) => {
        //val (lastElem, lastElemSpan) = getExprAndSpan(ps.parsedSynElems.head, ps).get
        //var synElems = ps.parsedSynElems.tail.reverse
        var synElems = ps.parsedSynElems

        val (e1, sp1) = getExprAndSpan(synElems.head,parseState).get
        if(synElems.tail.isEmpty) {
          val e = ErrorMessage.SynListIsEmpty(sp1, "Composition")
          return (Right(e),eL.add(e))
        }
        val (e2, sp2) = getExprAndSpan(synElems.tail.head,parseState).get
        val span = sp1 + sp2 //+ lastElemSpan
        val name = r.freshName("e")
        //var app = r.App(e2, r.App(e1, lastElem)(rt.TypePlaceholder, Some(span)))(rt.TypePlaceholder, Some(span))
        ////println("app of Comp: "+ app + " e1: '" + e1 + "' e2: '"+ e2 + "' lastElem: '" + lastElem + "'")
        var app = r.App(e2, r.App(e1, r.Identifier(name)(rt.TypePlaceholder, Some(span)))(rt.TypePlaceholder, Some(span)))(rt.TypePlaceholder, Some(span))
        //println("app of Comp: "+ app + " e1: '" + e1 + "' e2: '"+ e2 + "'")

        synElems = synElems.tail.tail
        while(synElems.nonEmpty){
          val (exN, _) = getExprAndSpan(synElems.head,parseState).get
          synElems = synElems.tail
          app = r.App(exN, app)(rt.TypePlaceholder,Some(span))
          //println("app of Comp in Loop: "+ app)
        }
        val lam = r.Lambda(r.Identifier(name)(rt.TypePlaceholder, Some(span)), app)(rt.TypePlaceholder, Some(span))
        //println("lam in parseComp:"+lam + "\n")
        if(end(ps.tokenStream)){
          (Left(ParseState(ps.tokenStream, SExpr(lam) :: parseState.parsedSynElems.tail, ps.mapDepL, ps.spanList, ps.argumentsTypes)),
            eL.add(UsedOrFailedRule(isMatched(), whatToParse)))
        }else{
          val (pP,newErrorList) = (Left(ParseState(ps.tokenStream, Nil, ps.mapDepL, ps.spanList,ps.argumentsTypes)) ,eL)|> parseNoAppExpr
          pP match{
            case Right(e)=> {
              (Left(ParseState(ps.tokenStream, SExpr(lam) :: parseState.parsedSynElems.tail, ps.mapDepL, ps.spanList, ps.argumentsTypes)),
                newErrorList.add(UsedOrFailedRule(isMatched(), whatToParse)))//If NoAppExpr does not suceed, we simply use lam
            }
            case Left(pL) => {
              val (expr, expr_span) = getExprAndSpan(pL.parsedSynElems.head, pL).get
              val resApp = r.App(lam,expr)(rt.TypePlaceholder, Some(span+expr_span))
              //println("resApp in parseComp:"+resApp + "\n")
              (Left(ParseState(pL.tokenStream, SExpr(resApp) :: parseState.parsedSynElems.tail, pL.mapDepL, pL.spanList,pL.argumentsTypes)),
                newErrorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
            }
          }
        }
      }
    }
  }

  def giveNextLambdaIdentType(argumentTypes:List[rt.Type]):Option[(rt.Type, List[rt.Type])]={
    argumentTypes match {
      //case head :: Nil => None //Todo: I don't know why but this line fixes the nbody.rise test, if something fails with a match Error in Solution and Nats look here
      case head::next =>
        println("\ngiveNextLambdaIdenType: "+ head + "::" + next)
        Some((head, next))
      case Nil => None
    }
  }
  def giveIdentWithCorrectType(i:r.Expr, identType:rt.Type):r.Expr={
    identType match {
      case TypePlaceholder => i
      case _ => i.setType(identType)
    }
  }

  /*
is the whole Syntax-Tree.
the syntax-Tree has on top an Lambda-Expression
 */
  def parseLambda(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "Lambda"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    if(parseState.tokenStream.isEmpty){
      return (Left(parseState),errorList.add(UsedOrFailedRule(isFailed(), whatToParse)))
    }
    println("parseLambda: " +parseState)
    val (psOld,errorL) =
      (Left(ParseState(parseState.tokenStream, Nil,  parseState.mapDepL, parseState.spanList, parseState.argumentsTypes)),errorList) |>
        parseBackslash |>
        parseIdent     |>
        parseMaybeTypeAnnotation |>
        parseArrow



    val ((psOrErr, spanBackslash),idName) = psOld match {
      case Left(p) => {
        val (identType, argumentTypeListNew) = giveNextLambdaIdentType(p.argumentsTypes) match {
          case None => (rt.TypePlaceholder, Nil)
          case Some((t,l)) => (t,l)
        }
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
            case SExpr(i) => (i //Todo:giveIdentWithCorrectType(i,identType)
              , synElemListExpr.tail)
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
            case SSeq(seq, _) => throw new RuntimeException("List should't have any Seq at this position! " + seq)
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
          case Some(span::Nil) => (parseMaybeAppExpr((ParseState(p.tokenStream,Nil, p.mapDepL, None, argumentTypeListNew),errorL)) , span )
          case Some(span::l)=> ( parseMaybeAppExpr((ParseState(p.tokenStream,Nil, p.mapDepL, Some(l), argumentTypeListNew),errorL) ), span)
        }
        (ret, identifierName)
      }
      case Right(e) => {
        return (Right(e),errorL.add(UsedOrFailedRule(isFailed(), whatToParse)))
      }
    }

    val (toks, expr,  mapDepL, spanList, newEL, newArgumentTypeList) = psOrErr match {
      case (Left(psNew),errorList2) => {
        //println("combine in Lambda: "+ psNew)
        val expr = combineExpressionsDependent(psNew.parsedSynElems, psNew.mapDepL.get)
        //println("\n\n\nSpanIsHere"+ expr.expr +" : "+ expr.expr.span+ "\n\n\n")
        (psNew.tokenStream,expr, psNew.mapDepL, psNew.spanList, errorList2, psNew.argumentsTypes)
      }
      case (Right(e),errorList2) => return (Right(e),errorList2.add(UsedOrFailedRule(isFailed(), whatToParse)))
    }
    val spanOfBackslash = parseState.tokenStream.head.s
    val span = Span(spanOfBackslash.file,Range(spanOfBackslash.range.begin, expr.span.head.range.end))
    val lambda = r.Lambda(idName, expr)(rt.TypePlaceholder, Some(span))
    val myNewParseState = ParseState(toks, SExpr(lambda) :: parseState.parsedSynElems, mapDepL, spanList, newArgumentTypeList)
    //println("myNewParseState: "+ myNewParseState)
    (Left(myNewParseState),newEL.add(UsedOrFailedRule(isMatched(), whatToParse)))
  }


  //_________________________________________________________Lambda
  //_________________________________________________________Expres

  def parseNoAppExpr(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "NoAppExpr"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseLowExpression: " + parseState)
    val (p, eL) = (Left(parseState),errorList) |>
      (parseLambda _ || parseDepLambda || parseBracesExpr ||
        parseUnOperator || parseBinOperator || parseIdent ||
        parseNumber || parseAddrSpaceType || parseTypeinNoAppExpr||
        parseNat
        )
    p match {
      case Right(e) => (Right(e),eL.add(UsedOrFailedRule(isFailed(), whatToParse)))
      case Left(pa) => (Left(ParseState(pa.tokenStream, pa.parsedSynElems,  pa.mapDepL, parseState.spanList, pa.argumentsTypes)),
        eL.add(UsedOrFailedRule(isMatched(), whatToParse)))
    }
  }

  def parseDepOrNormalFunctionTypeInNoAppExpr(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "DepOrNomralFunctionTypeInNoAppExpr"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val (p,eL) = //Todo: change so that no Brackets are needed, only parentheses for Dep- and FunctionType
      (Left(parseState),errorList) |>
        parseLeftParentheses |>
        parseDepOrNormalFunctionType |>
        parseRightParentheses
    p match {
      case Right(e) => (Right(e),eL.add(UsedOrFailedRule(isFailed(), whatToParse)))
      case Left(pa) => (Left(ParseState(pa.tokenStream, pa.parsedSynElems,  pa.mapDepL, parseState.spanList, pa.argumentsTypes)),
        eL.add(UsedOrFailedRule(isMatched(), whatToParse)))
    }
  }

  def parseTypeinNoAppExpr(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "TypeInNoAppExpr"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseTypeinNoAppExpr: "+ parseState)
    if(parseState.tokenStream.isEmpty){
      return (Left(parseState),errorList.add(UsedOrFailedRule(isFailed(), whatToParse)))
    }
    val (p,eL) = //Todo: change so that no Brackets are needed, only parentheses for Dep- and FunctionType
      (Left(ParseState(parseState.tokenStream,Nil, parseState.mapDepL, parseState.spanList, parseState.argumentsTypes)),errorList)  |>
        (parseDepOrNormalFunctionTypeInNoAppExpr _ || parseSimpleType)

    //println("after parseTypeinNoAppExpr: "+ p)
    p match {
      case Left(newPS) => {
        val synList = combineSynElemList(parseState.parsedSynElems, newPS.parsedSynElems).reverse
//        println("SynList2 in parseTypeinNoAppExpr: " + synList + " , newPS: " + newPS.parsedSynElems + " , parseStateOld: " + parseState.parsedSynElems)
        (Left(ParseState(newPS.tokenStream, synList,  newPS.mapDepL, newPS.spanList, newPS.argumentsTypes) ),
          eL.add(UsedOrFailedRule(isMatched(), whatToParse)))
      }
      case Right(e) => (Right(e),eL.add(UsedOrFailedRule(isFailed(), whatToParse)))
    }
  }

  def parseBracesExpr(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "BracesExpr"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseBracesExpr: "+ parseState)
    if(parseState.tokenStream.isEmpty){
      return (Left(parseState),errorList.add(UsedOrFailedRule(isFailed(), whatToParse)))
    }
    val (p,eL) =
      (Left(ParseState(parseState.tokenStream,Nil,  parseState.mapDepL, parseState.spanList, parseState.argumentsTypes)),errorList)  |>
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
            case SSeq(seq, _) => throw new RuntimeException("List should't have any Seq at this position! " + seq)
          }
        }else {
          val newExpr = combineExpressionsDependent(pState.parsedSynElems, pState.mapDepL.get)
          SExprClutched(newExpr, spanClutch)
        }
        //println("synElem: " + synElem)
        val newL = synElem :: Nil
        val li:List[SyntaxElement] = parseState.parsedSynElems.reverse ++ newL
        val l = li.reverse
        val newParse:ParseState = ParseState(pState.tokenStream, l,  pState.mapDepL, parseState.spanList, pState.argumentsTypes)
        (Left(newParse),eL.add(UsedOrFailedRule(isMatched(), whatToParse)))
      }
      case Right(e) => (Right(e),eL.add(UsedOrFailedRule(isFailed(), whatToParse)))
    }
  }

  def parseArrayType(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "ArrayType"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseIndexType: " + parseState)
    val (p,eL) =
      (Left(ParseState(parseState.tokenStream,Nil, parseState.mapDepL, parseState.spanList, parseState.argumentsTypes)),errorList)|>
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
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.mapDepL, pState.spanList, pState.argumentsTypes)
        (Left(newParse),eL.add(UsedOrFailedRule(isMatched(), whatToParse)))
      }
      case Right(e) => (Right(e),eL.add(UsedOrFailedRule(isFailed(), whatToParse)))
    }
  }

  def parseIndexType(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "IndexType"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseIndexType: " + parseState)
    val (p,eL) =
      (Left(ParseState(parseState.tokenStream,Nil, parseState.mapDepL, parseState.spanList, parseState.argumentsTypes)),errorList)  |>
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
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.mapDepL, pState.spanList, pState.argumentsTypes)
        (Left(newParse),eL.add(UsedOrFailedRule(isMatched(), whatToParse)))
      }
      case Right(e) => (Right(e),eL.add(UsedOrFailedRule(isFailed(), whatToParse)))
    }
  }
  def parseTupleType(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "TupleType"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseTupleType: "+ parseState)
    val (p,eL) =
      (Left(ParseState(parseState.tokenStream,Nil, parseState.mapDepL, parseState.spanList, parseState.argumentsTypes)),errorList)  |>
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
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.mapDepL, parseState.spanList, pState.argumentsTypes)
        (Left(newParse),eL.add(UsedOrFailedRule(isMatched(), whatToParse)))
      }
      case Right(e) => (Right(e),eL.add(UsedOrFailedRule(isFailed(), whatToParse)))
    }
  }

  def parseBracesExprType(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "BracesExprType"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseBracesExprType: "+ parseState)
    val (p,eL) =
      (Left(ParseState(parseState.tokenStream,Nil, parseState.mapDepL, parseState.spanList, parseState.argumentsTypes)),errorList)  |>
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
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.mapDepL, parseState.spanList, pState.argumentsTypes)
        (Left(newParse),eL.add(UsedOrFailedRule(isMatched(), whatToParse)))
      }
      case Right(e) => (Right(e),eL.add(UsedOrFailedRule(isFailed(), whatToParse)))
    }
  }

  private def end(tokenStream:List[Token]):Boolean={
    tokenStream.isEmpty||tokenStream.head.isInstanceOf[EndNamedExpr]
  }

  def parseMaybeAppExpr(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "MaybeAppExpr"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseMaybeAppExpr: " + parseState)
    if(parseState.tokenStream.head.isInstanceOf[RParentheses]){
      return (Left(parseState),errorList.add(UsedOrFailedRule(isFailed(), whatToParse)))
    }else if(parseState.tokenStream.head.isInstanceOf[EndNamedExpr]){
      //println("EndNamedExpr sighted in ParseMaybeAppExpr: "+ parseState)
      return (Left(parseState),errorList.add(UsedOrFailedRule(isFailed(), whatToParse)))
    }
    val (parseStateOrError,eL) =
      (Left(parseState),errorList)  |> parseNoAppExpr
    //println("parseApp after parseLowExpression: "+ parseStateOrError)
    parseStateOrError match {
      case Right(e) => (Right(e),eL.add(UsedOrFailedRule(isFailed(), whatToParse)))//(Right(e),eL.add(e))
      case Left(ps)=> if(end(ps.tokenStream)){
        //println("parseApp End, because TokenList is empty: "+ ps)
                              val expr = combineExpressionsDependent(ps.parsedSynElems, ps.mapDepL.get)
                              (Left(ParseState(ps.tokenStream, SExpr(expr)::Nil,  ps.mapDepL, ps.spanList, ps.argumentsTypes)),
                                eL.add(UsedOrFailedRule(isMatched(), whatToParse)))
                            }else{
                              if(ps.parsedSynElems.isEmpty) throw new IllegalStateException("ps is Empty: "+ ps)
                              val (p, newEL) = if(ps.tokenStream.head.isInstanceOf[Dot]){
                                (Left(ps),eL)|> parseComp
                              }else{
                                (Left(ps),eL)|> parseMaybeAppExpr
                              }

                              p match {
                                case Right(e) => (Right(e),newEL.add(UsedOrFailedRule(isFailed(), whatToParse)))
                                case Left(newPS) => {
                                  val synElem = if(newPS.parsedSynElems.length==1){
                                    newPS.parsedSynElems.head
                                  }else {
                                    if(newPS.parsedSynElems.isEmpty){
                                      val e = ErrorMessage.SynListIsEmpty(newPS.tokenStream.head.s, "MaybeAppExpr")
                                      return (Right(e), newEL.add(e))
                                    }
                                    val expr = combineExpressionsDependent(newPS.parsedSynElems, newPS.mapDepL.get)
                                    //println("\n\n\nSpanIsHere"+ expr +" : "+ expr.span+ "\n\n\n")
                                    SExpr(expr)
                                  }
                                  (Left(ParseState(newPS.tokenStream, synElem::Nil, newPS.mapDepL, newPS.spanList, newPS.argumentsTypes)),
                                    newEL.add(UsedOrFailedRule(isMatched(), whatToParse)))
                                }
                              }
                            }
    }
  }

  def parseEqualsSign(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "EqualsSign"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, parsedExprs, mapDepL, spanList,argumentsTypes) = parseState
    if (tokens.isEmpty) {
      val e = ErrorMessage.TokListIsEmpty(SpanPlaceholder, "EqualsSign")
      return (Right(e),errorList.add(e))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case EqualsSign(_) =>
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, "EqualsSign", "EqualsSign")
        return (Right(e),errorList.add(e))
      }
    }

    (Left(ParseState(remainderTokens, parsedExprs, mapDepL, spanList, argumentsTypes)),
      errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
  }

  def parseDoubleColons(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "DoubleColons"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, parsedExprs, mapDepL, spanList,argumentsTypes) = parseState
    if (tokens.isEmpty) {
      val e = ErrorMessage.TokListIsEmpty(SpanPlaceholder, "DoubleColons")
      return (Right(e),errorList.add(e))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case DoubleColons(_) =>
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, "DoubleColons", "DoubleColons" )
        return (Right(e),errorList.add(e))
      }
    }

    (Left(ParseState(remainderTokens, parsedExprs, mapDepL, spanList,argumentsTypes)),
      errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
  }


  def parseAddrSpaceType(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "AddrSpaceType"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    if (parseState.tokenStream.isEmpty) {
      val e = ErrorMessage.TokListIsEmpty(SpanPlaceholder, "AddrSpacerType")
      return (Right(e),errorList.add(e))
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
          SAddrSpace(p,spanAddr) :: parseState.parsedSynElems, parseState.mapDepL, parseState.spanList, parseState.argumentsTypes) ),
          errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
      }
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, "addrSpaceType", "AddrSpaceType")
        (Right(e),errorList.add(e))
      }
    }
  }

  def parseUnOperator(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "UnOperator"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val nextToken :: remainderTokens = parseState.tokenStream

    val p = nextToken match {
      case UnOp(un, span) => un match {
        case OpType.UnaryOpType.NEG => Left(ParseState(remainderTokens,
          SExpr(r.primitives.neg(Some(span))) :: parseState.parsedSynElems, parseState.mapDepL, parseState.spanList, parseState.argumentsTypes) )
        case OpType.UnaryOpType.NOT => Left(ParseState(remainderTokens,
          SExpr(r.primitives.not(Some(span))) :: parseState.parsedSynElems, parseState.mapDepL, parseState.spanList, parseState.argumentsTypes) )
      }
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, "UnOp", "UnOperator")
        return (Right(e), errorList.add(e))
      }
    }
    (p,errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
  }

  def parseNumber(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "Number"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("ParseNumber: " + parseState)
    val ParseState(tokens, parsedSynElems, mapDepL, spanList,argumentsTypes)  = parseState
    if(tokens.isEmpty){
      val e = ErrorMessage.TokListIsEmpty(SpanPlaceholder, "Number")
      return (Right(e),errorList.add(e))
    }
    val nextToken :: remainderTokens = tokens

    val p = nextToken match {
      case I8(number, span) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.IntData(number), Some(span))) :: parsedSynElems, mapDepL, spanList,argumentsTypes) )
      case I32(number, span) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.IntData(number),Some(span))) :: parsedSynElems, mapDepL, spanList,argumentsTypes) )
      case F32(number, span) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.FloatData(number),Some(span))) :: parsedSynElems, mapDepL, spanList,argumentsTypes) )
      case F64(number, span) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.DoubleData(number),Some(span))) :: parsedSynElems, mapDepL, spanList,argumentsTypes) )
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, "Integer of Float", "Number")
        return (Right(e),errorList.add(e))
      }
    }
    (p,errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
  }


  def parseBinOperator(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "BinOperator"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseBinOperator: "+ parseState)
    val ParseState(tokens, parsedSynElems, mapDepL, spanList,argumentsTypes)  = parseState
    val nextToken :: remainderTokens = tokens

    val p = nextToken match {
      case BinOp(op, span) => op match {
        case OpType.BinOpType.ADD =>{
          //println("\n\n Span of add: "+ span + " , add: "+ r.primitives.add(Some(span)) + "  ,SExpr(add): "+ SExpr(r.primitives.add(Some(span))).expr ) //es wird vergessen von Scala, dass es auch vom Type ToBeTyped ist
          //println("span of add: " + r.primitives.add(Some(span)).span + "span of add: " + r.primitives.add(Some(span)).toUntypedExpr.span+ " , span of SExpr(add): "+ SExpr(r.primitives.add(Some(span))).expr.span)
          //println("span of makeArray: "+ r.primitives.makeArray(5, Some(span)).span)
          Left(ParseState(remainderTokens, SExpr(r.primitives.add(Some(span))) :: parsedSynElems, mapDepL, spanList,argumentsTypes) )
        }

        case OpType.BinOpType.DIV =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.div(Some(span))) :: parsedSynElems, mapDepL, spanList,argumentsTypes) )
        case OpType.BinOpType.EQ =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.equal(Some(span))) :: parsedSynElems, mapDepL, spanList,argumentsTypes) )
        case OpType.BinOpType.GT =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.gt(Some(span))) :: parsedSynElems, mapDepL, spanList,argumentsTypes) )
        case OpType.BinOpType.LT =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.lt(Some(span))) :: parsedSynElems, mapDepL, spanList,argumentsTypes) )
        case OpType.BinOpType.MOD => Left(ParseState(remainderTokens, SExpr(
          r.primitives.mod(Some(span))) :: parsedSynElems, mapDepL, spanList,argumentsTypes) )
        case OpType.BinOpType.MUL => Left(ParseState(remainderTokens, SExpr(
          r.primitives.mul(Some(span))) :: parsedSynElems, mapDepL, spanList,argumentsTypes) )
        case OpType.BinOpType.SUB => Left(ParseState(remainderTokens, SExpr(
          r.primitives.sub(Some(span))) :: parsedSynElems, mapDepL, spanList,argumentsTypes) )
        case tok => {
          //println("Das hier kann nicht sein, weil alle Operatoren müsste ich abgedeckt haben. BinOp: '" + tok +
          //  "' is no BinOperator!")
          val e = ErrorMessage.NotCorrectToken(BinOp(op, span), "BinOp", "BinOp")
          return (Right(e),errorList.add(e))
        }
      }
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, "BinOp", "BinOp")
        return (Right(e),errorList.add(e))
      }
    }
    (p,errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
  }

  def parseBodyLine(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "BodyLine"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseComma: " + parseState)
    val ParseState(tokens, parsedSynElems, mapDepL, spanList,argumentsTypes)  = parseState
    val nextToken :: remainderTokens = tokens
    //println("BodyLine: '"+nextToken+"'")
    nextToken match {
      case ForeignFctBodyColumn(body, span) => (Left(ParseState(remainderTokens,
        SExpr(r.Identifier(body)(rt.TypePlaceholder, Some(span)))::parsedSynElems, mapDepL, spanList,argumentsTypes) ),
        errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, "ForeignFctBodyColumn", whatToParse)
        (Right(e),errorList.add(e))
      }
    }
  }

  def parseComma(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "Comma"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    //println("parseComma: " + parseState)
    val ParseState(tokens, parsedSynElems, mapDepL, spanList,argumentsTypes)  = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Comma(_) => (Left(ParseState(remainderTokens, parsedSynElems, mapDepL, spanList,argumentsTypes) ),
        errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, "Comma", "Comma")
        (Right(e),errorList.add(e))
      }
    }
  }



  def parseDepArrow(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "DepArrow"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, parsedSynElems, mapDepL, spanList,argumentsTypes)  = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case DepArrow(_) => (Left(ParseState(remainderTokens, parsedSynElems, mapDepL, spanList,argumentsTypes) ),
        errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, "DepArrow", "DepArrow")
        (Right(e),errorList.add(e))
      }
    }
  }

  def parseColon(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "Colon"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, parsedSynElems, mapDepL, spanList,argumentsTypes)  = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Colon(_) => (Left(ParseState(remainderTokens, parsedSynElems, mapDepL, spanList,argumentsTypes) ),
        errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, "Colon", "Colon")
        (Right(e),errorList.add(e))
      }
    }
  }

  def parseDot(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "Dot"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, parsedSynElems, mapDepL, spanList,argumentsTypes)  = parseState
    isMinLen(tokens, 2, "Dot") match{
      case Some(e) => return (Right(e), errorList.add(e))
      case None => None
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Dot(_) => (Left(ParseState(remainderTokens, parsedSynElems, mapDepL, spanList,argumentsTypes) ),
        errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, "Dot", "Dot")
        (Right(e),errorList.add(e))
      }
    }
  }

    def parseForeignFctKeyWord(inputEPState: InputEPState): OutputEPState = {
      val whatToParse = "ForeignFctKeyWord"
      val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
      val ParseState(tokens, parsedSynElems, mapDepL, spanList,argumentsTypes)  = parseState
      val nextToken :: remainderTokens = tokens

      val p = nextToken match {
        case ForeignKeyword(_) => Left(ParseState(remainderTokens, parsedSynElems,
          mapDepL, spanList,argumentsTypes) )
        case tok => {
          val e = ErrorMessage.NotCorrectToken(tok, whatToParse, whatToParse)
          return (Right(e),errorList.add(e))
        }
      }
      (p,errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
    }

  def parseNat(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "Nat"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, parsedSynElems, mapDepL, spanList,argumentsTypes)  = parseState
    val nextToken :: remainderTokens = tokens

    val p = nextToken match {
      case NatNumber(number, spanNat) => Left(ParseState(remainderTokens, SNat(NNumber(number:rt.Nat),spanNat)::parsedSynElems,
        mapDepL, spanList,argumentsTypes) )
      case TypeIdentifier(name, spanTypeIdentifier) =>Left(ParseState(remainderTokens,
        SNat(NIdentifier(rt.NatIdentifier(name)), spanTypeIdentifier)::parsedSynElems, mapDepL, spanList,argumentsTypes) )
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, "NatNumber or TypeIdent", "Nat")
        return (Right(e),errorList.add(e))
      }
    }
    (p,errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
  }

  def parseData(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "Data"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, parsedSynElems, mapDepL, spanList,argumentsTypes)  = parseState
    val nextToken :: remainderTokens = tokens

    val p = nextToken match {
      case TypeIdentifier(name, sp) =>Left(ParseState(remainderTokens,
        SData(DIdentifier(rt.DataTypeIdentifier(name)), sp)::parsedSynElems, mapDepL, spanList,argumentsTypes) )
      case a => {
        val e = ErrorMessage.NotCorrectToken(a, "TypeIdent", "Data")
        return (Right(e),errorList.add(e))
      }
    }
    (p,errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
  }

  def parseLeftBracket(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "LeftBracket"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, parsedSynElems, mapDepL, spanList,argumentsTypes)  = parseState
    isMinLen(tokens, 2, "LeftBracket") match{
      case Some(e) => return (Right(e),errorList.add(e))
      case None => None
    }
    val nextToken :: remainderTokens = tokens

    val p = nextToken match {
      case LBracket(_) => Left(ParseState(remainderTokens, parsedSynElems, mapDepL, spanList,argumentsTypes) )
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, "LBracket", "LBracket")
        return (Right(e),errorList.add(e))
      }
    }
    (p,errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
  }

  def parseRightBracket(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "RightBracket"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, parsedSynElems, mapDepL, spanList,argumentsTypes)  = parseState
    val nextToken :: remainderTokens = tokens

    val p = nextToken match {
      case RBracket(_) => Left(ParseState(remainderTokens, parsedSynElems, mapDepL, spanList,argumentsTypes) )
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, "RBracket", "RBracket")
        return (Right(e),errorList.add(e))
      }
    }
    (p,errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
  }

  def parseLeftBraces(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "LeftBraces"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, parsedSynElems, mapDepL, _,argumentsTypes)  = parseState
    val nextToken :: remainderTokens = tokens

    val p  =nextToken match {
      case LBraces(sp) => Left(ParseState(remainderTokens, parsedSynElems, mapDepL, Some(sp::Nil),argumentsTypes))
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, whatToParse, whatToParse)
        return (Right(e),errorList.add(e))
      }
    }
    (p,errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
  }
  def parseRightBraces(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "RightBraces"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, parsedSynElems, mapDepL, _,argumentsTypes)  = parseState
    val nextToken :: remainderTokens = tokens

    val p  =nextToken match {
      case RBraces(sp) => Left(ParseState(remainderTokens, parsedSynElems, mapDepL, Some(sp::Nil),argumentsTypes))
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, whatToParse, whatToParse)
        return (Right(e),errorList.add(e))
      }
    }
    (p,errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
  }

  def parseLeftParentheses(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "LeftParentheses"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, parsedSynElems, mapDepL, _,argumentsTypes)  = parseState
    val nextToken :: remainderTokens = tokens

    val p  =nextToken match {
      case LParentheses(sp) => Left(ParseState(remainderTokens, parsedSynElems, mapDepL, Some(sp::Nil),argumentsTypes))
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, "LParentheses", "LParentheses")
        return (Right(e),errorList.add(e))
      }
    }
    (p,errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
  }

  def parseRightParentheses(inputEPState: InputEPState): OutputEPState = {
    val whatToParse = "RightParentheses"
    val (parseState,errorList) = (inputEPState._1, inputEPState._2.add(UsedOrFailedRule(isParsing(), whatToParse)))
    val ParseState(tokens, parsedSynElems, mapDepL, spanList,argumentsTypes)  = parseState
    val nextToken :: remainderTokens = tokens

    val spanL = spanList match{
      case None => throw new IllegalStateException("The spanList should not be None")
      case Some(l) => l match {
        case spLeftPar :: Nil => spLeftPar
        case _ => throw new IllegalStateException("The list in parseRightParentheses: '" + l + "' has not the correct form")
      }
    }

    val p = nextToken match {
      case RParentheses(sp) => Left(ParseState(remainderTokens, parsedSynElems, mapDepL, Some(sp::spanL ::Nil),argumentsTypes) )
      case tok => {
        val e = ErrorMessage.NotCorrectToken(tok, "RParentheses", "RParentheses")
        return (Right(e),errorList.add(e))
      }
    }
    (p,errorList.add(UsedOrFailedRule(isMatched(), whatToParse)))
  }


  //_________________________________________________________Expres
}