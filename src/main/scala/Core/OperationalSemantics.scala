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
  final case class RecordData(fst: Data, snd: Data) extends Data(RecordType(fst.dataType, snd.dataType))

  object makeArrayData {
    def apply(seq: Data*) = ArrayData(Vector(seq: _*))
  }

  sealed trait AccIdentifier
  case class NamedIdentifier(name: String) extends AccIdentifier
  case class ArrayAccessIdentifier(array: AccIdentifier, index: Int) extends AccIdentifier
  case class RecordIdentiers(fst: AccIdentifier, snd: AccIdentifier) extends AccIdentifier

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
        case _: IdentPhrase[_]   => in

        case l: LambdaPhrase[_, _] =>
          LambdaPhrase(l.param, substitute(phrase, `for`, l.body))

        case app: ApplyPhrase[a, T2] =>
          val newFun = substitute(phrase, `for`, app.fun)
          val newArg = substitute(phrase, `for`, app.arg)
          ApplyPhrase(newFun, newArg)

        case pair: PairPhrase[a, b] =>
          PairPhrase(substitute(phrase, `for`, pair.fst), substitute(phrase, `for`, pair.snd))

        case p: Proj1Phrase[T2, b] => Proj1Phrase(substitute(phrase, `for`, p.pair))
        case p: Proj2Phrase[a, T2] => Proj2Phrase(substitute(phrase, `for`, p.pair))

        case RecordExpPhase(fst, snd) =>
          RecordExpPhase(substitute(phrase, `for`, fst), substitute(phrase, `for`, snd))

        case RecordAccPhase(fst, snd) =>
          RecordAccPhase(substitute(phrase, `for`, fst), substitute(phrase, `for`, snd))

        case FstExprPhrase(record) =>
          FstExprPhrase(substitute(phrase, `for`, record))

        case SndExprPhrase(record) =>
          SndExprPhrase(substitute(phrase, `for`, record))

        case FstAccPhrase(record) =>
          FstAccPhrase(substitute(phrase, `for`, record))

        case SndAccPhrase(record) =>
          SndAccPhrase(substitute(phrase, `for`, record))

        case LengthPhrase(array) => LengthPhrase(substitute(phrase, `for`, array))

        case ArrayExpAccessPhrase(array, index) =>
          ArrayExpAccessPhrase(substitute(phrase, `for`, array), substitute(phrase, `for`, index))

        case ArrayAccAccessPhrase(array, index) =>
          ArrayAccAccessPhrase(substitute(phrase, `for`, array), substitute(phrase, `for`, index))

        case _: SkipPhrase => in

        case SeqPhrase(c1, c2) =>
          SeqPhrase(substitute(phrase, `for`, c1), substitute(phrase, `for`, c2))

        case NewPhrase(f) =>
          NewPhrase(substitute(phrase, `for`, f))

        case AssignPhrase(lhs, rhs) =>
          AssignPhrase(substitute(phrase, `for`, lhs), substitute(phrase, `for`, rhs))

        case i: IfThenElsePhrase[T2] =>
          val newCond = substitute(phrase, `for`, i.cond)
          val newThenP = substitute(phrase, `for`, i.thenP)
          val newElseP = substitute(phrase, `for`, i.elseP)
          IfThenElsePhrase(newCond, newThenP, newElseP)

        case ForPhrase(n, body) =>
          ForPhrase(substitute(phrase, `for`, n), substitute(phrase, `for`, body))

        case _: LiteralPhrase    => in

        case BinOpPhrase(op, lhs, rhs) =>
          BinOpPhrase(op, substitute(phrase, `for`, lhs), substitute(phrase, `for`, rhs))

        case ExpPatternPhrase(pattern) => ExpPatternPhrase(pattern.substitute(phrase, `for`))

        case AccPatternPhrase(pattern) => AccPatternPhrase(pattern.substitute(phrase, `for`))

        case CommandPatternPhrase(pattern) => CommandPatternPhrase(pattern.substitute(phrase, `for`))
      }).asInstanceOf[Phrase[T2]]
      res.t = in.t // preserve type
      res
    }
  }

  def eval[T <: PhraseType, R](s: Store, p: Phrase[T])
                              (implicit evaluator: Evaluator[T, R]): R = {
    p match {
      case app: ApplyPhrase[a, T] =>
        val fun: (Phrase[a]) => Phrase[T] = eval(s, app.fun)
        eval(s, fun(app.arg))

      case p1: Proj1Phrase[a, b] =>
        val pair: (Phrase[a], Phrase[b]) = eval(s, p1.pair)
        eval(s, pair._1)

      case p2: Proj2Phrase[a, b] =>
        val pair: (Phrase[a], Phrase[b]) = eval(s, p2.pair)
        eval(s, pair._2)

      case IfThenElsePhrase(cond, thenP, elseP) =>
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


  implicit def UnaryFunctionEvaluator[T1 <: PhraseType,
                                      T2 <: PhraseType]: Evaluator[T1 -> T2,
                                                                   (Phrase[T1] => Phrase[T2])] =
    new Evaluator[T1 -> T2, (Phrase[T1] => Phrase[T2])] {
      def apply(s: Store, p: Phrase[T1 -> T2]): (Phrase[T1] => Phrase[T2]) = {
        p match {
          case l: LambdaPhrase[T1, T2] => (arg: Phrase[T1]) => substitute(arg, `for` = l.param, in = l.body)
          case IdentPhrase(_) | ApplyPhrase(_, _) | IfThenElsePhrase(_, _, _) | Proj1Phrase(_) | Proj2Phrase(_) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def BinaryFunctionEvaluator[T1 <: PhraseType,
                                       T2 <: PhraseType,
                                       T3 <: PhraseType]: Evaluator[T1 -> (T2 -> T3),
                                                                    (Phrase[T1] => Phrase[T2] => Phrase[T3])] =
    new Evaluator[T1 -> (T2 -> T3), (Phrase[T1] => Phrase[T2] => Phrase[T3])] {
      def apply(s: Store, p: Phrase[T1 -> (T2 -> T3)]): (Phrase[T1] => Phrase[T2] => Phrase[T3]) = {
        p match {
          case l: LambdaPhrase[T1, T2 -> T3] => (arg: Phrase[T1]) =>
            eval(s, substitute(arg, `for` = l.param, in = l.body))(UnaryFunctionEvaluator)

          case IdentPhrase(_) | ApplyPhrase(_, _) | IfThenElsePhrase(_, _, _) | Proj1Phrase(_) | Proj2Phrase(_) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def TrinaryFunctionEvaluator[T1 <: PhraseType,
                                        T2 <: PhraseType,
                                        T3 <: PhraseType,
                                        T4 <: PhraseType]: Evaluator[T1 -> (T2 -> (T3 -> T4)),
                                                                     (Phrase[T1] => Phrase[T2] => Phrase[T3] => Phrase[T4])] =
    new Evaluator[T1 -> (T2 -> (T3 -> T4)), (Phrase[T1] => Phrase[T2] => Phrase[T3] => Phrase[T4])] {
      def apply(s: Store, p: Phrase[T1 -> (T2 -> (T3 -> T4))]): (Phrase[T1] => Phrase[T2] => Phrase[T3] => Phrase[T4]) = {
        p match {
          case l: LambdaPhrase[T1, T2 -> (T3 -> T4)] => (arg: Phrase[T1]) =>
            eval(s, substitute(arg, `for` = l.param, in = l.body))(BinaryFunctionEvaluator)

          case IdentPhrase(_) | ApplyPhrase(_, _) | IfThenElsePhrase(_, _, _) | Proj1Phrase(_) | Proj2Phrase(_) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def PairEvaluator[T1 <: PhraseType, T2 <: PhraseType]: Evaluator[T1 x T2, (Phrase[T1], Phrase[T2])] =
    new Evaluator[T1 x T2, (Phrase[T1], Phrase[T2])] {
      def apply(s: Store, p: Phrase[T1 x T2]): (Phrase[T1], Phrase[T2]) = {
        p match {
          case i: IdentPhrase[T1 x T2] => (IdentPhrase[T1](i.name), IdentPhrase[T2](i.name))
          case pair: PairPhrase[T1, T2] => (pair.fst, pair.snd)
          case ApplyPhrase(_, _) | IfThenElsePhrase(_, _, _) | Proj1Phrase(_) | Proj2Phrase(_) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def ExpEvaluator: Evaluator[ExpType, Data] =
    new Evaluator[ExpType, Data] {
      def apply(s: Store, p: Phrase[ExpType]): Data = {
        p match {
          case IdentPhrase(name) => s(name)

          case RecordExpPhase(fst, snd) =>
            RecordData(eval(s, fst), eval(s, snd))

          case FstExprPhrase(record) =>
            eval(s, record) match {
              case r: RecordData => r.fst
              case _ => throw new Exception("This should not happen")
            }

          case SndExprPhrase(record) =>
            eval(s, record) match {
              case r: RecordData => r.snd
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

          case LiteralPhrase(d) => d

          case BinOpPhrase(op, lhs, rhs) =>
            op match {
              case BinOpPhrase.Op.ADD => evalIntExp(s, lhs) + evalIntExp(s, rhs)
              case BinOpPhrase.Op.SUB => evalIntExp(s, lhs) - evalIntExp(s, rhs)
              case BinOpPhrase.Op.MUL => evalIntExp(s, lhs) * evalIntExp(s, rhs)
              case BinOpPhrase.Op.DIV => evalIntExp(s, lhs) / evalIntExp(s, rhs)
              case BinOpPhrase.Op.MOD => evalIntExp(s, lhs) % evalIntExp(s, rhs)
            }

          case ExpPatternPhrase(pattern) =>
            pattern.eval(s)

          case ApplyPhrase(_, _) | IfThenElsePhrase(_, _, _) | Proj1Phrase(_) | Proj2Phrase(_) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def AccEvaluator: Evaluator[AccType, AccIdentifier] =
    new Evaluator[AccType, AccIdentifier] {
      def apply(s: Store, p: Phrase[AccType]): AccIdentifier = {
        p match {
          case IdentPhrase(name) => NamedIdentifier(name)

          case RecordAccPhase(fst, snd) =>
            RecordIdentiers(eval(s, fst), eval(s, snd))

          case FstAccPhrase(record) =>
            eval(s, record) match {
              case r: RecordIdentiers => r.fst
              case _ => throw new Exception("This should not happen")
            }

          case SndAccPhrase(record) =>
            eval(s, record) match {
              case r: RecordIdentiers => r.snd
              case _ => throw new Exception("This should not happen")
            }

          case ArrayAccAccessPhrase(arrayP, indexP) =>
            val array = eval(s, arrayP)
            val index = eval(s, indexP) match {
              case IntData(i) => i
              case _ => throw new Exception("This should not happen")
            }
            ArrayAccessIdentifier(array, index)

          case AccPatternPhrase(pattern) => pattern.eval(s)

          case ApplyPhrase(_, _) | IfThenElsePhrase(_, _, _) | Proj1Phrase(_) | Proj2Phrase(_) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def CommandEvaluator: Evaluator[CommandType, Store] =
    new Evaluator[CommandType, Store] {
      def apply(s: Store, p: Phrase[CommandType]): Store = {
        p match {
          case IdentPhrase(_) => throw new Exception("This should never happen")

          case SkipPhrase() => s

          case SeqPhrase(c1, c2) =>
            val s1 = eval(s, c1)
            eval(s1, c2)

          case NewPhrase(fP) =>
            val f = eval(s, fP)
            val arg = IdentPhrase[ExpType x AccType](newName())
            val s1: Store = eval(s + (arg.name -> 0), f(arg))
            s1 - arg.name

          case AssignPhrase(lhs, rhs) =>
            def evalAssign(s: Store, lhs: AccIdentifier, rhs: Data,
                           continuation: (Store, String, Data) => Store): Store = {
              lhs match {
                case NamedIdentifier(name) =>
                  assert(s.contains(name))
                  continuation(s, name, rhs)

                case ArrayAccessIdentifier(array, index) =>
                  evalAssign(s, array, rhs, (s, arrayName, rhsValue) => {
                    assert(s.contains(arrayName))
                    s(arrayName) match {
                      case ArrayData(vec) => continuation(s, arrayName, ArrayData(vec.updated(index, rhsValue)))
                      case _ => throw new Exception("This should not happen")
                    }
                  })

                case RecordIdentiers(fstI, sndI) =>
                  rhs match {
                    case RecordData(fstD, sndD) =>
                      val s1 = evalAssign(s, fstI, fstD, continuation)
                      evalAssign(s1, sndI, sndD, continuation)
                    case _ => throw new Exception("This should not happen")
                  }
              }
            }

            evalAssign(s, eval(s, lhs), eval(s, rhs), (s, identifier, value) => {
              s + (identifier -> value)
            })

          case ForPhrase(nP, bodyP) =>
            val n = evalIntExp(s, nP)
            val body = eval(s, bodyP)
            (0 until n).foldLeft(s)( (s1, i) => {
              eval(s1, body(LiteralPhrase(i)))
            } )


          case CommandPatternPhrase(pattern) => pattern.eval(s)

          case ApplyPhrase(_, _) | IfThenElsePhrase(_, _, _) | Proj1Phrase(_) | Proj2Phrase(_) =>
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
