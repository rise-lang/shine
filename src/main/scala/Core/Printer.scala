package Core

import OperationalSemantics._
import PhraseType.x

object Printer {
  def toC[T <: PhraseType](p: Phrase[T]): String = {
    p match {
      case IdentPhrase(name) => name

      case Proj1Phrase(pair) => toC(Lift.liftPair(pair)._1)
      case Proj2Phrase(pair) => toC(Lift.liftPair(pair)._2)

      case RecordExpPhase(fst, snd) =>
        val dt = p.t match { case ExpType(dataType) => dataType }
        s"(struct ${nameOf(dt)}){ ${toC(fst)} , ${toC(snd)} }"

      case FstExprPhrase(record) =>
        toC(record) + ".fst"

      case SndExprPhrase(record) =>
        toC(record) + ".snd"

      case LengthPhrase(array) => array.t match {
        case ExpType(ArrayType(n, dt)) => n.toString
        case AccType(ArrayType(n, dt)) => n.toString
      }

      case ArrayAccAccessPhrase(array, index) => toC(array) + "[" + toC(index) + "]"

      case ArrayExpAccessPhrase(array, index) => toC(array) + "[" + toC(index) + "]"

      case SkipPhrase() => ""

      case SeqPhrase(c1, c2) => toC(c1) + ";\n" + toC(c2)

      case NewPhrase(fP) =>
        val f = Lift.liftFunction(fP)
        val v = IdentPhrase[ExpType x AccType](OperationalSemantics.newName())
        val dt = fP.t.inT.t1.dataType
        v.t = PairType(ExpType(dt), AccType(dt))
        s"{\n${nameOf(dt)} ${v.name};\n${toC(f(v))}; \n}"

      case AssignPhrase(lhs, rhs) =>
        toC(lhs) + " = " + toC(rhs) + ";\n"

      case IfThenElsePhrase(condP, thenP, elseP) =>
        s"if (${toC(condP)}) { ${toC(thenP)}; } else { ${toC(elseP)}; }"

      case ForPhrase(n, fP) =>
        val f = Lift.liftFunction(fP)
        val i = IdentPhrase[ExpType](OperationalSemantics.newName())
        i.t = ExpType(int)
        s"for (int ${i.name} = 0; ${i.name} < ${toC(n)}; ++${i.name}) {\n${toC(f(i))}}\n"

      case LiteralPhrase(d) =>
        val dt = p.t match { case ExpType(dataType) => dataType }
        literal(d, dt)

      case BinOpPhrase(op, lhs, rhs) => "(" + toC(lhs) + " " + op.toString + " " + toC(rhs) + ")"

      case LambdaPhrase(_, _) | ApplyPhrase(_, _) | PairPhrase(_, _) | ExpPatternPhrase(_) | CommandPatternPhrase(_) =>
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
