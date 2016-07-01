package Core

import apart.arithmetic.ArithExpr

import scala.collection.immutable.HashMap
import scala.language.implicitConversions
import scala.language.postfixOps
import scala.language.reflectiveCalls

object OperationalSemantics {

  sealed abstract class Data(val dataType: DataType)
  final case class IndexData(i: ArithExpr) extends Data(int)
  final case class BoolData(b: Boolean) extends Data(bool)
  final case class IntData(i: Int) extends Data(int) {
    override def toString = i.toString
  }
//  final case class Int4Data(i0: Int, i1: Int, i2: Int, i3: Int) extends Data(int4)
  final case class FloatData(f: Float) extends Data(float) {
    override def toString = f.toString + "f"
  }
  final case class VectorData(a: Vector[Data]) extends Data(VectorType(a.length, a.head.dataType match {
    case b: BasicType => b
    case _ => throw new Exception("This should not happen")
  }))
  final case class ArrayData(a: Vector[Data]) extends Data(ArrayType(a.length, a.head.dataType))
  final case class RecordData(fst: Data, snd: Data) extends Data(RecordType(fst.dataType, snd.dataType))

  object Data {
    def toString(d: Data): String = {
      d match {
        case i: IntData => i.i.toString
        case b: BoolData => b.b.toString
        case f: FloatData => f.f.toString
        case i: IndexData => i.i.toString
        //          case i: Int4Data => Literal(s"(int4)(${i.i0.toString}, ${i.i1.toString}, ${i.i2.toString}, ${i.i3.toString})")
        case v: VectorData => v.a.length match {
          case 2 | 3 | 4 | 8 | 16 =>
            val dt = DataType.toString(v.a.head.dataType)
            val n = v.a.length
            s"($dt$n)(" + v.a.map(x => toString(x)).reduce( _ + ", " + _ ) + ")"
        }
        case _: RecordData => ???
        case _: ArrayData => ???
      }
    }
  }

  object makeArrayData {
    def apply(seq: Data*) = ArrayData(Vector(seq: _*))
  }

  sealed trait AccIdentifier
  case class NamedIdentifier(name: String) extends AccIdentifier
  case class ArrayAccessIdentifier(array: AccIdentifier, index: ArithExpr) extends AccIdentifier
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
          case l: LambdaPhrase[T1, T2] =>
            (arg: Phrase[T1]) => l.body `[` arg `/` l.param `]`
          case IdentPhrase(_, _) | ApplyPhrase(_, _) | NatDependentApplyPhrase(_, _) |
               IfThenElsePhrase(_, _, _) | Proj1Phrase(_) | Proj2Phrase(_) =>
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
            eval(s, l.body `[` arg `/` l.param `]` )(UnaryFunctionEvaluator)

          case IdentPhrase(_, _) | ApplyPhrase(_, _) | NatDependentApplyPhrase(_, _) |
               IfThenElsePhrase(_, _, _) | Proj1Phrase(_) | Proj2Phrase(_) =>
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
            eval(s, l.body `[` arg  `/` l.param `]` )(BinaryFunctionEvaluator)

          case IdentPhrase(_, _) | ApplyPhrase(_, _) | NatDependentApplyPhrase(_, _) |
               IfThenElsePhrase(_, _, _) | Proj1Phrase(_) | Proj2Phrase(_) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def PairEvaluator[T1 <: PhraseType, T2 <: PhraseType]: Evaluator[T1 x T2, (Phrase[T1], Phrase[T2])] =
    new Evaluator[T1 x T2, (Phrase[T1], Phrase[T2])] {
      def apply(s: Store, p: Phrase[T1 x T2]): (Phrase[T1], Phrase[T2]) = {
        p match {
          case i: IdentPhrase[T1 x T2] =>
            if (i.t == null) {
              val t1 = null.asInstanceOf[T1]
              val t2 = null.asInstanceOf[T2]
              (IdentPhrase[T1](i.name, t1), IdentPhrase[T2](i.name, t2))
            } else {
              (IdentPhrase[T1](i.name, i.t.t1), IdentPhrase[T2](i.name, i.t.t2))
            }
          case pair: PairPhrase[T1, T2] => (pair.fst, pair.snd)
          case ApplyPhrase(_, _) | NatDependentApplyPhrase(_, _) |
               IfThenElsePhrase(_, _, _) | Proj1Phrase(_) | Proj2Phrase(_) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def ExpEvaluator: Evaluator[ExpType, Data] =
    new Evaluator[ExpType, Data] {
      def apply(s: Store, p: Phrase[ExpType]): Data = {
        p match {
          case IdentPhrase(name, _) => s(name)

          case LiteralPhrase(d) => d

          case UnaryOpPhrase(op, x) =>
            op match {
              case UnaryOpPhrase.Op.NEG => - evalIntExp(s, x)
            }

          case BinOpPhrase(op, lhs, rhs) =>
            op match {
              case BinOpPhrase.Op.ADD => evalIntExp(s, lhs) + evalIntExp(s, rhs)
              case BinOpPhrase.Op.SUB => evalIntExp(s, lhs) - evalIntExp(s, rhs)
              case BinOpPhrase.Op.MUL => evalIntExp(s, lhs) * evalIntExp(s, rhs)
              case BinOpPhrase.Op.DIV => evalIntExp(s, lhs) / evalIntExp(s, rhs)
              case BinOpPhrase.Op.MOD => evalIntExp(s, lhs) % evalIntExp(s, rhs)
              case BinOpPhrase.Op.GT => if (evalIntExp(s, lhs) > evalIntExp(s, rhs)) 1 else 0
              case BinOpPhrase.Op.LT => if (evalIntExp(s, lhs) < evalIntExp(s, rhs)) 1 else 0
            }

          case p: ExpPattern => p.eval(s)

          case ApplyPhrase(_, _) | NatDependentApplyPhrase(_, _) |
               IfThenElsePhrase(_, _, _) | Proj1Phrase(_) | Proj2Phrase(_) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def AccEvaluator: Evaluator[AccType, AccIdentifier] =
    new Evaluator[AccType, AccIdentifier] {
      def apply(s: Store, p: Phrase[AccType]): AccIdentifier = {
        p match {
          case IdentPhrase(name, _) => NamedIdentifier(name)

          case p: AccPattern => p.eval(s)

          case ApplyPhrase(_, _) | NatDependentApplyPhrase(_, _) |
               IfThenElsePhrase(_, _, _) | Proj1Phrase(_) | Proj2Phrase(_) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def CommandEvaluator: Evaluator[CommandType, Store] =
    new Evaluator[CommandType, Store] {
      def apply(s: Store, p: Phrase[CommandType]): Store = {
        p match {
          case IdentPhrase(_, _) => throw new Exception("This should never happen")

          case p: IntermediateCommandPattern => p.eval(s)

          case ApplyPhrase(_, _) | NatDependentApplyPhrase(_, _) |
               IfThenElsePhrase(_, _, _) | Proj1Phrase(_) | Proj2Phrase(_) =>
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

  def evalIndexExp(s: Store, p: Phrase[ExpType]): ArithExpr = {
    eval(s, p) match {
      case IndexData(i) => i
      case IntData(i) => i
      case _ => throw new Exception("This should never happen")
    }
  }

  def evalIntExp(s: Store, p: Phrase[ExpType]): Int = {
    eval(s, p) match {
      case IntData(i) => i
      case IndexData(i) => i.eval
      case _ => throw new Exception("This should never happen")
    }
  }

}
