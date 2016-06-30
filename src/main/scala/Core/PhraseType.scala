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
      override def apply[T2 <: PhraseType](p: Phrase[T2]): Result[Phrase[T2]] = {
        //p.t = substitute(ae, `for`, p.t).asInstanceOf[T2]
        Continue(p, this)
      }

      override def apply(e: ArithExpr) = substitute(ae, `for`, e)

      override def apply[DT <: DataType](dt: DT) = substitute(ae, `for`, dt)
    }

    val p = VisitAndRebuild(in, fun())
    TypeChecker(p)
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
        ArrayType(ArithExpr.substitute(a.size, Map( (`for`, ae) )),
          substitute(ae, `for`, a.elemType))
      case r: RecordType =>
        RecordType(substitute(ae, `for`, r.fst), substitute(ae, `for`, r.snd))
    }).asInstanceOf[T]
  }

  def substitute(ae: ArithExpr, `for`: NamedVar, in: ArithExpr): ArithExpr = {
   ArithExpr.substitute(in, Map( (`for`, ae) ))
  }

  class PhraseTypeParser(val string: String,
                         var strings: Iterator[String],
                         val values: Iterator[Any]) {

    def check(cond: Boolean): Unit = {
      if (!cond) {
        throw new Exception(s"Could not parse $string into a PhraseType")
      }
    }

    def parseArrayType(n: ArithExpr): ArrayType = {
      if (strings.hasNext) {
        strings.next.trim match {
          case "." => ArrayType(n, parseDataType)
          case _ =>
            throw new Exception(s"Could not parse $string into a PhraseType")
        }
      } else {
        throw new Exception(s"Could not parse $string into a PhraseType")
      }
    }

    def parseRecordOrBaseType(dt: DataType): DataType = {
      if (!strings.hasNext) { dt }

      val (iter1, iter2) = strings.duplicate
      strings = iter1

      iter2.next.trim match {
        case "x" =>
          strings.next // consume token
          RecordType(dt, parseDataType)
        case _ => dt
      }
    }

    def parseDataType: DataType = {
      if (values.hasNext) {
        values.next match {
          case n: ArithExpr => parseArrayType(n)
          case dt: DataType => parseRecordOrBaseType(dt)
          case null => null.asInstanceOf[DataType]
          case _ =>
            throw new Exception(s"Could not parse $string into a PhraseType")
        }
      } else {
        throw new Exception(s"Could not parse $string into a PhraseType")
      }
    }

    def parseBasePhraseType: PhraseType = {
      if (strings.hasNext) {
        strings.next.trim match {
          case "exp[" =>
            val expType = parseExpType
            check(strings.hasNext && strings.next().trim == "]")
            expType
          case "acc[" =>
            val accType = parseAccType
            check(strings.hasNext && strings.next().trim == "]")
            accType
          case "var[" =>
            val dataType = parseDataType
            val varType = ExpType(dataType) x AccType(dataType)
            check(strings.hasNext && strings.next().trim == "]")
            varType
          case "comm" => CommandType()
          case _ =>
            throw new Exception(s"Could not parse $string into a PhraseType")
        }
      } else {
        throw new Exception(s"Could not parse $string into a PhraseType")
      }
    }

    def parseExpType: ExpType = ExpType(parseDataType)

    def parseAccType: AccType = AccType(parseDataType)

    def parsePhraseType: PhraseType = {
      val pt1 = parseBasePhraseType
      if (!strings.hasNext) return pt1
      strings.next.trim match {
        case "x" =>
          PairType(pt1, parsePhraseType)
        case "->" =>
          FunctionType(pt1, parsePhraseType)
        case _ =>
          throw new Exception(s"Could not parse $string into a PhraseType")
      }
    }
  }

}
