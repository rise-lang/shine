package idealised.DPIA.Types

import idealised.DPIA._

class PhraseTypeParser(val string: String,
                       var strings: Seq[String],
                       var values: Iterator[Any]) {

  val tokens: Seq[String] = Seq("exp", "acc", "comm", "var", "nat", "idx", "[", "]", "(", ")", ".", "x", "->", ":")

  def hasToken: Boolean = strings.nonEmpty

  def trim(s: String): String = s.trim

  def nextToken: String = {
    if (!hasToken) error
    val head = trim(strings.head)
    tokens.foreach(token => {
      if (head.startsWith(token)) {
        val (_, tail) = head.splitAt(token.length)
        if (tail.isEmpty) strings = strings.tail
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

  def parseArrayOrIdxType(n: Nat): DataType = {
    peakToken match {
      case "." => parseArrayType(n)
      case "idx" => parseIdxType(n)
      case _ => error
    }
  }

  def parseArrayType(n: Nat): DataType = {
    nextToken match {
      case "." =>
        parseDataTypeOrNatDataTypeFunction match {
          case Left(dt) => ArrayType(n, dt)
          case Right(ft) => DepArrayType(n, ft)
        }
      case _ => error
    }
  }

  def parseIdxType(n: Nat): IndexType = {
    nextToken match {
      case "idx" =>
        nextToken match {
          case "(" =>
            nextToken match {
              case ")" => IndexType(n)
              case _ => error
            }
          case _ => error
        }
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

//  def parseArrayOrIdxOrRecordOrBaseType(`null`: Any): DataType = {
//    peakToken match {
//      case "x" => nextToken; RecordType(`null`.asInstanceOf[DataType], parseDataType)
//      case "]" => `null`.asInstanceOf[DataType]
//      case ")" => nextToken; parseRecordOrBaseType(`null`.asInstanceOf[DataType])
//      case "." => nextToken; ArrayType(`null`.asInstanceOf[Nat], parseDataType)
//      case "idx" => parseIdxType(`null`.asInstanceOf[Nat])
//      case _ => error
//    }
//  }

  def parseDataTypeOrNatDataTypeFunction:Either[DataType, NatToDataLambda] = {
    if (peakToken == "(") {
      nextToken
    }

    if (values.hasNext) {
      values.next match {
        case n: Nat => Left(parseArrayOrIdxType(n))
        case dt: DataType => Left(parseRecordOrBaseType(dt))
        case ndt:NatToDataLambda =>
          Right(ndt)
        //case null => parseArrayOrIdxOrRecordOrBaseType(null)
        case _ => error
      }
    } else error
  }

  def parseDataType: DataType = {
    parseDataTypeOrNatDataTypeFunction match {
      case Left(dt) => dt
      case Right(_) => error
    }
  }

  def parseNatDependentFunctionType: PhraseType = {
    if (values.hasNext) {
      values.next match {
        case l: NatIdentifier =>
          peakToken match {
            case ":" => nextToken
              peakToken match {
                case "nat" => nextToken
                  peakToken match {
                    case ")" => nextToken
                      peakToken match {
                        case "->" => nextToken
                          l `()->:` parsePhraseType
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

  def parseWrappedDataType: DataType = {
    nextToken match {
      case "[" =>
        val dt = parseDataType
        nextToken match {
          case "]" => dt
          case _ => error
        }
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
      case "->" => FunType(pt1, parsePhraseType)
      case _ => error
    }
  }
}
