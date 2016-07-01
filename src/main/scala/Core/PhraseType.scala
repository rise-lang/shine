package Core

import apart.arithmetic.{ArithExpr, NamedVar}

sealed trait PhraseType

abstract class BasePhraseTypes extends PhraseType

final case class ExpType(dataType: DataType) extends BasePhraseTypes {
  override def toString = s"exp[$dataType]"
}

final case class AccType(dataType: DataType) extends BasePhraseTypes {
  override def toString = s"acc[$dataType]"
}

sealed case class CommandType() extends PhraseType {
  override def toString = "comm"
}

object comm extends CommandType

final case class PairType[T1 <: PhraseType, T2 <: PhraseType](t1: T1, t2: T2) extends PhraseType {
  override def toString = s"$t1 x $t2"
}

final case class FunctionType[T1 <: PhraseType, T2 <: PhraseType](inT: T1, outT: T2) extends PhraseType {
  override def toString = s"$inT -> $outT"
}

final case class PassiveFunctionType[T1 <: PhraseType, T2 <: PhraseType](inT: T1, outT: T2)
  extends PhraseType

final case class NatDependentFunctionType[T <: PhraseType](x: NamedVar, t: T) extends PhraseType {
  override def toString = s"($x : Nat) -> $t"
}

object PhraseType {

  def substitute[T <: PhraseType](ae: ArithExpr,
                                  `for`: NamedVar,
                                  in: Phrase[T]): Phrase[T] = {

    case class fun() extends VisitAndRebuild.fun {
      override def apply(e: ArithExpr) = substitute(ae, `for`, e)

      override def apply[DT <: DataType](dt: DT) = substitute(ae, `for`, dt)
    }

    val p = VisitAndRebuild(in, fun())
    p.typeCheck()
    p

  }

  def substitute(ae: ArithExpr, `for`: NamedVar, in: PhraseType): PhraseType = {
    in match {
      case b: BasePhraseTypes => b match {
        case e: ExpType => ExpType(substitute(ae, `for`, e.dataType))
        case a: AccType => AccType(substitute(ae, `for`, a.dataType))
      }
      case c: CommandType => c
      case p: PairType[_, _] =>
        PairType(substitute(ae, `for`, p.t1), substitute(ae, `for`, p.t2))
      case f: FunctionType[_, _] =>
        FunctionType(substitute(ae, `for`, f.inT), substitute(ae, `for`, f.outT))
      case pf: PassiveFunctionType[_, _] =>
        PassiveFunctionType(substitute(ae, `for`, pf.inT), substitute(ae, `for`, pf.outT))
      case nf: NatDependentFunctionType[_] =>
        NatDependentFunctionType(nf.x, substitute(ae, `for`, nf.t))
    }
  }

  def substitute[T <: DataType](ae: ArithExpr, `for`: NamedVar, in: T): T = {
    (in match {
      case b: BasicType => b
      case a: ArrayType =>
        ArrayType(ArithExpr.substitute(a.size, Map((`for`, ae))),
          substitute(ae, `for`, a.elemType))
      case r: RecordType =>
        RecordType(substitute(ae, `for`, r.fst), substitute(ae, `for`, r.snd))
    }).asInstanceOf[T]
  }

  def substitute(ae: ArithExpr, `for`: NamedVar, in: ArithExpr): ArithExpr = {
    ArithExpr.substitute(in, Map((`for`, ae)))
  }

  class PhraseTypeParser(val string: String,
                         var strings: Seq[String],
                         var values: Iterator[Any]) {

    val tokens = Seq("exp", "acc", "comm", "var", "nat", "[", "]", "(", ")", ".", "x", "->", ":")

    def hasToken: Boolean = strings.nonEmpty

    def trim(s: String): String = s.trim

    def nextToken: String = {
      if (!hasToken) error
      val head = trim(strings.head)
      tokens.foreach(token => {
        if (head.startsWith(token)) {
          val (_, tail) = head.splitAt(token.length)
          if (tail.isEmpty)  strings = strings.tail
          else strings = tail +: strings.tail
          return token
        }
      })
      error
    }

    def peakToken: String = {
      if (!hasToken) error
      val head = trim(strings.head)
      tokens.foreach(token => if (head.startsWith(token)) return token)
      error
    }

    def error: Nothing = throw new Exception(s"Could not parse `$string' into a PhraseType")

    def check(cond: Boolean): Unit = {
      if (!cond) error
    }

    def parseArrayType(n: ArithExpr): ArrayType = {
      nextToken match {
        case "." => ArrayType(n, parseDataType)
        case _ => error
      }

    }

    def parseRecordOrBaseType(dt: DataType): DataType = {
      peakToken match {
        case "x" => nextToken; RecordType(dt, parseDataType)
        case "]" => dt
        case ")" => nextToken; parseRecordOrBaseType(dt)
        case _ => error
      }
    }

    def parseArrayOrRecordOrBaseType(`null`: Any): DataType = {
      peakToken match {
        case "x" => nextToken; RecordType(`null`.asInstanceOf[DataType], parseDataType)
        case "]" => `null`.asInstanceOf[DataType]
        case ")" => nextToken; parseRecordOrBaseType(`null`.asInstanceOf[DataType])
        case "." => nextToken; ArrayType(`null`.asInstanceOf[ArithExpr], parseDataType)
        case _ => error
      }
    }

    def parseDataType: DataType = {
      peakToken match {
        case "(" => nextToken
        case _ =>
      }
      if (values.hasNext) {
        values.next match {
          case n: ArithExpr => parseArrayType(n)
          case dt: DataType => parseRecordOrBaseType(dt)
          case null => parseArrayOrRecordOrBaseType(null)
          case _ => error
        }
      } else error
    }

    def parseNatDependentFunctionType: PhraseType = {
      if (values.hasNext) {
        values.next match {
          case l: NamedVar =>
            peakToken match {
              case ":" => nextToken
                peakToken match {
                  case "nat" => nextToken
                    peakToken match {
                      case ")" => nextToken
                        peakToken match {
                          case "->" => nextToken
                            NatDependentFunctionType(l, parsePhraseType)
                          case _ => error
                        }
                      case _ => error
                    }
                  case _ => error
                }
              case _ => error
            }
          case _ => error
        }
      } else error
    }

    def parseBasePhraseType: PhraseType = {
      nextToken match {
        case "exp" => parseExpType
        case "acc" => parseAccType
        case "var" => parseVarType
        case "comm" => comm
        case "(" => parseNatDependentFunctionType
        case _ => error
      }
    }

    def parseExpType: ExpType = {
      nextToken match {
        case "[" =>
          val dt = parseDataType
          nextToken match {
            case "]" => ExpType(dt)
            case _ => error
          }
        case _ => error
      }
    }

    def parseAccType: AccType = {
      nextToken match {
        case "[" =>
          val dt = parseDataType
          nextToken match {
            case "]" => AccType(dt)
            case _ => error
          }
        case _ => error
      }
    }

    def parseVarType: VarType = {
      nextToken match {
        case "[" =>
          val dt = parseDataType
          nextToken match {
            case "]" => ExpType(dt) x AccType(dt)
            case _ => error
          }
        case _ => error
      }
    }

    def parsePhraseType: PhraseType = {
      val pt1 = parseBasePhraseType
      if (!hasToken) return pt1
      nextToken match {
        case "x" => PairType(pt1, parsePhraseType)
        case "->" => FunctionType(pt1, parsePhraseType)
        case _ => error
      }
    }
  }

}
