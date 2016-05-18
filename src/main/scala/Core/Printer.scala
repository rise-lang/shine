package Core

import OperationalSemantics._

object Printer {
  def toC[T <: PhraseType](p: Phrase[T]): String = {
    p match {
      case IdentPhrase(name) => name

      case Proj1Phrase(pair) => toC(Lift.liftPair(pair)._1)

      case Proj2Phrase(pair) => toC(Lift.liftPair(pair)._2)

      case IfThenElsePhrase(condP, thenP, elseP) =>
        s"if (${toC(condP)}) { ${toC(thenP)}; } else { ${toC(elseP)}; }"

      case LiteralPhrase(d) =>
        val dt = p.t match { case ExpType(dataType) => dataType }
        literal(d, dt)

      case BinOpPhrase(op, lhs, rhs) => "(" + toC(lhs) + " " + op.toString + " " + toC(rhs) + ")"

      case ExpPatternPhrase(pattern) => pattern.toC

      case AccPatternPhrase(pattern) => pattern.toC

      case CommandPatternPhrase(pattern) => pattern.toC

      case LambdaPhrase(_, _) | ApplyPhrase(_, _) | PairPhrase(_, _)  =>
        throw new Exception("This should not happen")
    }
  }

  def nameOf(t: DataType): String = {
    t match {
      case RecordType(fst, snd) =>
        "record" + nameOf(fst) + "_" + nameOf(snd)
      case ArrayType(n, elemType) => nameOf(elemType) + "*"
      case `bool`   => "int"
      case `int`    => "int"
      case `int4`   => "int4"
      case `float`  => "float"
    }
  }

  def definitionOf(t: DataType): String = {
    t match {
      case RecordType(fst, snd) =>
        s"typedef struct ${nameOf(t)} {\n ${nameOf(fst)} _fst;\n ${nameOf(snd)} _snd;\n }\n"

      case _ => nameOf(t)
    }
  }

  def literal(d: Data, t: DataType): String = {
    d match {
      case BoolData(b) => if (b) "1" else "0"
      case IntData(i) => i.toString
      case Int4Data(i0, i1, i2, i3) => s"(int4)( $i0, $i1, $i2, $i3 )"
      case FloatData(f) => f.toString + "f"
      case RecordData(fst, snd) =>
        val (fstT, sndT) = t match {
          case r: RecordType => (r.fst, r.snd)
          case _ => throw new Exception("This should never happen")
        }
        s"(struct ${nameOf(t)}){ ${literal(fst, fstT)} , ${literal(snd, sndT)} }"
      case ArrayData(a) =>
        val elemT = t match {
          case ArrayType(_, et) => et
          case _ => throw new Exception("This should never happen")
        }
        s"(${nameOf(elemT)}[]){ ${a.map(literal(_, elemT)).reduce( (x,y) => x + ", " + y )} }"
    }
  }
}
