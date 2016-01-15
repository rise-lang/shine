package Core

import PhraseType._

import scala.collection.immutable.HashMap
import scala.language.implicitConversions

object OperationalSemantics {

  sealed abstract class Data(val dataType: DataType)
  final case class BoolData(b: Boolean) extends Data(bool)
  final case class IntData(i: Int) extends Data(int)
  final case class Int4Data(i0: Int, i1: Int, i2: Int, i3: Int) extends Data(int4)
  final case class FloatData(f: Float) extends Data(float)
  final case class ArrayData(a: Vector[Data]) extends Data(ArrayType(a.length, a.head.dataType))
  final case class RecordData(fields: Data*) extends Data(RecordType(fields.map(_.dataType):_*))

  object makeArrayData {
    def apply(seq: Data*) = ArrayData(Vector(seq: _*))
  }

  sealed trait AccIdentifier
  case class NamedIdentifier(name: String) extends AccIdentifier
  case class ArrayAccessIdentifier(array: AccIdentifier, index: Int) extends AccIdentifier

  implicit def IntToIntData(i: Int): IntData = IntData(i)

  object newName {
    var counter = 0

    def apply(): String = {
      counter += 1
      "v" + counter
    }
  }

  type Store = HashMap[String, Data]

  // substitutes `phrase` for `for` in `in`
  def substitute[T1 <: PhraseType, T2 <: PhraseType](phrase: Phrase[T1],
                                                     `for`: Phrase[T1],
                                                     in: Phrase[T2]): Phrase[T2] = {
    if (`for` == in) {
      phrase.asInstanceOf[Phrase[T2]] // T1 == T2
    } else {
      val res = (in match {
        // these cases must all be `<: Phrase[T2]`, because they match on in.
        // The casts should be unnecessary
        case _: Ident[_]   => in

        case l: Lambda[_, _] =>
          Lambda(l.param, substitute(phrase, `for`, l.body))

        case app: Apply[a, T2] =>
          val newFun = substitute(phrase, `for`, app.fun)
          val newArg = substitute(phrase, `for`, app.arg)
          Apply(newFun, newArg)

        case pair: Pair[a, b] =>
          Pair(substitute(phrase, `for`, pair.fst), substitute(phrase, `for`, pair.snd))

        case p: Proj1[T2, b] => Proj1(substitute(phrase, `for`, p.pair))
        case p: Proj2[a, T2] => Proj2(substitute(phrase, `for`, p.pair))

        case Record(fields@_*) =>
          Record(fields.map(f => substitute(phrase, `for`, f)):_*)

        case FieldAccess(n, record) =>
          FieldAccess(n, substitute(phrase, `for`, record))

        case LengthPhrase(array) => LengthPhrase(substitute(phrase, `for`, array))

        case ArrayExpAccessPhrase(array, index) =>
          ArrayExpAccessPhrase(substitute(phrase, `for`, array), substitute(phrase, `for`, index))

        case ArrayAccAccessPhrase(array, index) =>
          ArrayAccAccessPhrase(substitute(phrase, `for`, array), substitute(phrase, `for`, index))

        case _: SkipPhrase => in

        case Seq(c1, c2) =>
          Seq(substitute(phrase, `for`, c1), substitute(phrase, `for`, c2))

        case NewPhrase(f) =>
          NewPhrase(substitute(phrase, `for`, f))

        case Assign(lhs, rhs) =>
          Assign(substitute(phrase, `for`, lhs), substitute(phrase, `for`, rhs))

        case i: IfThenElse[T2] =>
          val newCond = substitute(phrase, `for`, i.cond)
          val newThenP = substitute(phrase, `for`, i.thenP)
          val newElseP = substitute(phrase, `for`, i.elseP)
          IfThenElse(newCond, newThenP, newElseP)

        case ForPhrase(n, body) =>
          ForPhrase(substitute(phrase, `for`, n), substitute(phrase, `for`, body))

        case _: Literal    => in

        case BinOp(op, lhs, rhs) =>
          BinOp(op, substitute(phrase, `for`, lhs), substitute(phrase, `for`, rhs))

        case PatternPhrase(pattern) => PatternPhrase(pattern.substitute(phrase, `for`))
      }).asInstanceOf[Phrase[T2]]
      res.t = in.t // preserve type
      res
    }
  }

  def eval[T <: PhraseType, R](s: Store, p: Phrase[T])
                              (implicit evaluator: Evaluator[T, R]): R = {
    p match {
      case app: Apply[a, T] =>
        val fun: (Phrase[a]) => Phrase[T] = eval(s, app.fun)
        eval(s, fun(app.arg))

      case p1: Proj1[a, b] =>
        val pair: (Phrase[a], Phrase[b]) = eval(s, p1.pair)
        eval(s, pair._1)

      case p2: Proj2[a, b] =>
        val pair: (Phrase[a], Phrase[b]) = eval(s, p2.pair)
        eval(s, pair._2)

      case IfThenElse(cond, thenP, elseP) =>
        if (evalCondExp(s, cond)) {
          eval(s, thenP)
        } else {
          eval(s, elseP)
        }

      case _ => evaluator(s, p)
    }
  }

  trait Evaluator[T <: PhraseType, R] {
    def apply(s: Store, p: Phrase[T]): R
  }


  implicit def FunctionEvaluator[T1 <: PhraseType, T2 <: PhraseType]: Evaluator[T1 -> T2, (Phrase[T1] => Phrase[T2])] =
    new Evaluator[T1 -> T2, (Phrase[T1] => Phrase[T2])] {
      def apply(s: Store, p: Phrase[T1 -> T2]): (Phrase[T1] => Phrase[T2]) = {
        p match {
          case l: Lambda[T1, T2] => (arg: Phrase[T1]) => substitute(arg, `for` = l.param, in = l.body)
          case Ident(_) | Apply(_, _) | IfThenElse(_, _, _) | Proj1(_) | Proj2(_) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def PairEvaluator[T1 <: PhraseType, T2 <: PhraseType]: Evaluator[T1 x T2, (Phrase[T1], Phrase[T2])] =
    new Evaluator[T1 x T2, (Phrase[T1], Phrase[T2])] {
      def apply(s: Store, p: Phrase[T1 x T2]): (Phrase[T1], Phrase[T2]) = {
        p match {
          case i: Ident[T1 x T2] => (Ident[T1](i.name), Ident[T2](i.name))
          case pair: Pair[T1, T2] => (pair.fst, pair.snd)
          case Apply(_, _) | IfThenElse(_, _, _) | Proj1(_) | Proj2(_) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def ExpEvaluator: Evaluator[ExpType, Data] =
    new Evaluator[ExpType, Data] {
      def apply(s: Store, p: Phrase[ExpType]): Data = {
        p match {
          case Ident(name) => s(name)

          case Record(fields@_*) =>
            RecordData(fields.map(f => eval(s, f)): _*)

          case FieldAccess(n, record) =>
            val data: Data = eval(s, record)
            data match {
              case r: RecordData => r.fields(n)
              case _ => throw new Exception("This should not happen")
            }

          case LengthPhrase(arrayP) =>
            arrayP.t match {
              case ExpType(ArrayType(n, _)) => n
              case AccType(ArrayType(n, _)) => n
            }

          case ArrayExpAccessPhrase(array, index) =>
            (eval(s, array), eval(s, index)) match {
              case (ArrayData(xs), IntData(i)) => xs(i)
              case _ => throw new Exception("This should not happen")
            }

          case Literal(d) => d

          case BinOp(op, lhs, rhs) =>
            op match {
              case BinOp.Op.ADD => evalIntExp(s, lhs) + evalIntExp(s, rhs)
              case BinOp.Op.SUB => evalIntExp(s, lhs) - evalIntExp(s, rhs)
              case BinOp.Op.MUL => evalIntExp(s, lhs) * evalIntExp(s, rhs)
              case BinOp.Op.DIV => evalIntExp(s, lhs) / evalIntExp(s, rhs)
              case BinOp.Op.MOD => evalIntExp(s, lhs) % evalIntExp(s, rhs)
            }

          case PatternPhrase(pattern) => pattern.eval(s)

          case Apply(_, _) | IfThenElse(_, _, _) | Proj1(_) | Proj2(_) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def AccEvaluator: Evaluator[AccType, AccIdentifier] =
    new Evaluator[AccType, AccIdentifier] {
      def apply(s: Store, p: Phrase[AccType]): AccIdentifier = {
        p match {
          case Ident(name) => NamedIdentifier(name)
          case ArrayAccAccessPhrase(arrayP, indexP) =>
            val array = eval(s, arrayP)
            val index = eval(s, indexP) match {
              case IntData(i) => i
              case _ => throw new Exception("This should not happen")
            }
            ArrayAccessIdentifier(array, index)
          case Apply(_, _) | IfThenElse(_, _, _) | Proj1(_) | Proj2(_) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def CommandEvaluator: Evaluator[CommandType, Store] =
    new Evaluator[CommandType, Store] {
      def apply(s: Store, p: Phrase[CommandType]): Store = {
        p match {
          case Ident(_) => throw new Exception("This should never happen")

          case SkipPhrase() => s

          case Seq(c1, c2) =>
            val s1 = eval(s, c1)
            eval(s1, c2)

          case NewPhrase(fP) =>
            val f = eval(s, fP)
            val arg = Ident[ExpType x AccType](newName())
            val s1: Store = eval(s + (arg.name -> 0), f(arg))
            s1 - arg.name

          case Assign(lhs, rhs) =>
            def evalAssign(s: Store, lhs: AccIdentifier, rhs: Data): (String, Data) = {
              lhs match {
                case NamedIdentifier(name) =>
                  assert(s.contains(name))
                  (name, rhs)
                case ArrayAccessIdentifier(array, index) =>
                  val (name, rhsValue) = evalAssign(s, array, rhs)
                  assert(s.contains(name))
                  s(name) match {
                    case ArrayData(vec) => (name, ArrayData(vec.updated(index, rhsValue)))
                    case _ => throw new Exception("This should not happen")
                  }
              }
            }

            val (identifier, value) = evalAssign(s, eval(s, lhs), eval(s, rhs))
            s + (identifier -> value)

          case ForPhrase(nP, bodyP) =>
            val n = evalIntExp(s, nP)
            val body = eval(s, bodyP)
            var s1 = s
            for (i <- 0 until n) {
              s1 = eval(s1, body(Literal(i)))
            }
            s1

          case Apply(_, _) | IfThenElse(_, _, _) | Proj1(_) | Proj2(_) =>
            throw new Exception("This should never happen")
        }
      }
    }


  def evalCondExp(s: Store, p: Phrase[ExpType]): Boolean = {
    eval(s, p) match {
      case BoolData(b) => b
      case IntData(i)  => i != 0
      case _ => throw new Exception("This should never happen")
    }
  }

  def evalIntExp(s: Store, p: Phrase[ExpType]): Int = {
    eval(s, p) match {
      case IntData(i) => i
      case _ => throw new Exception("This should never happen")
    }
  }

}
