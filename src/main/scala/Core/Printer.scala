package Core

import Core.OperationalSemantics._
import Core.PhraseType.{->, x}

object Printer {
  def toC[T <: PhraseType](s: OperationalSemantics.Store, p: Phrase[T]): String = {
    p match {
      case Ident(name) => name

      case Proj1(pair) =>
        import OperationalSemantics.implicits._
        val (fst, _) = OperationalSemantics.eval(s, pair)
        toC(s, fst)

      case Proj2(pair) =>
        import OperationalSemantics.implicits._
        val (_, snd) = OperationalSemantics.eval(s, pair)
        toC(s, snd)


      case Record(fields @ _*) =>
        val dt = p.t match { case ExpType(dataType) => dataType }
        s"(struct ${nameOf(dt)}){ ${fields.map(toC(s, _)).reduce( (x,y) => x + ", " + y )} }"

      case FieldAccess(n, record) =>
        toC(s, record) + "._" + n.toString

      case LengthPhrase(array) => array.t match {
        case ExpType(ArrayType(n, dt)) => n.toString
        case AccType(ArrayType(n, dt)) => n.toString
      }

      case ArrayAccAccessPhrase(array, index) => toC(s, array) + "[" + toC(s, index) + "]"

      case ArrayExpAccessPhrase(array, index) => toC(s, array) + "[" + toC(s, index) + "]"

      case SkipPhrase() => ""

      case Seq(c1, c2) => toC(s, c1) + ";\n" + toC(s, c2)

      case NewPhrase(fP) =>
        import OperationalSemantics.implicits._
        val f = OperationalSemantics.eval(s, fP)
        val v = Ident[ExpType x AccType](OperationalSemantics.newName())
        val dt = fP.t.inT.t1.dataType
        s"{\n${nameOf(dt)} ${v.name};\n${toC(s, f(v))}; \n}"

      case Assign(lhs, rhs) =>
        toC(s, lhs) + " = " + toC(s, rhs)

      case IfThenElse(condP, thenP, elseP) =>
        s"if (${toC(s, condP)}) { ${toC(s, thenP)}; } else { ${toC(s, elseP)}; }"

      case ForPhrase(n, fP) =>
        import OperationalSemantics.implicits._
        val f = OperationalSemantics.eval(s, fP)
        val i = Ident[ExpType](OperationalSemantics.newName())
        s"for (int ${i.name} = 0; ${i.name} < ${toC(s, n)}; ++${i.name}) {\n${toC(s, f(i))};\n}\n"

      case Literal(d) =>
        val dt = p.t match { case ExpType(dataType) => dataType }
        literal(d, dt)

      case BinOp(op, lhs, rhs) => "(" + toC(s, lhs) + " " + op.toString + " " + toC(s, rhs) + ")"

      case Lambda(_, _) | Apply(_, _) | Pair(_, _) | PatternPhrase(_) =>
        throw new Exception("This should not happen")
    }
  }

  def nameOf(t: DataType): String = {
    t match {
      case RecordType(fieldTs@_*) =>
        "record" + fieldTs.foldLeft("")((s, dt) => s + "_" + nameOf(dt))
      case ArrayType(n, elemType) => nameOf(elemType) + "*"
      case `bool`   => "int"
      case `int`    => "int"
      case `int4`   => "int4"
      case `float`  => "float"
    }
  }

  def definitionOf(t: DataType): String = {
    t match {
      case RecordType(fieldTs@_*) =>
        s"typedef struct ${nameOf(t)} {\n ${fieldTs.zipWithIndex.map({ case (field, i) =>
          nameOf(field) + " _" + i.toString + ";\n"
        })} }\n"

      case _ => nameOf(t)
    }
  }

  def literal(d: Data, t: DataType): String = {
    d match {
      case BoolData(b) => if (b) "1" else "0"
      case IntData(i) => i.toString
      case Int4Data(i0, i1, i2, i3) => s"(int4)( $i0, $i1, $i2, $i3 )"
      case FloatData(f) => f.toString + "f"
      case RecordData(fields @ _*) =>
        val ts = t match {
          case RecordType(fieldTs@_*) => fieldTs
          case _ => throw new Exception("This should never happen")
        }
        s"(struct ${nameOf(t)}){ ${fields.zipWithIndex.map({ case (field, i) =>
          literal(field, ts(i))
        }).reduce( (x,y) => x + ", " + y )} }"
      case ArrayData(a) =>
        val elemT = t match {
          case ArrayType(_, et) => et
          case _ => throw new Exception("This should never happen")
        }
        s"{ ${a.map(literal(_, elemT)).reduce( (x,y) => x + ", " + y )} }"
    }
  }
}
