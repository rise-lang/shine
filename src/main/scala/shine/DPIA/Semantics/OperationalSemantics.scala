package shine.DPIA.Semantics

import arithexpr.arithmetic.ArithExpr
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

import scala.collection.immutable.HashMap
import scala.language.{implicitConversions, postfixOps, reflectiveCalls}

object OperationalSemantics {
  sealed abstract class Data(val dataType: DataType)
  final case class IndexData(i: Nat, n: Nat) extends Data(IndexType(n))

  final case class NatData(n: Nat) extends Data(NatType)

  final case class BoolData(b: Boolean) extends Data(bool)
  final case class IntData(i: Int) extends Data(int) {
    override def toString: String = i.toString
  }
//  final case class Int4Data(i0: Int, i1: Int, i2: Int, i3: Int) extends Data(int4)
  final case class FloatData(f: Float) extends Data(f32) {
    override def toString: String = f.toString + "f"
  }
  final case class DoubleData(d: Double) extends Data(f64) {
    override def toString: String = d.toString
  }
  final case class VectorData(a: Vector[Data]) extends Data(VectorType(a.length, a.head.dataType match {
    case b: ScalarType => b
    case _ => throw new Exception("This should not happen")
  }))
  final case class ArrayData(a: Vector[Data]) extends Data(ArrayType(a.length, a.head.dataType))
  final case class PairData(fst: Data, snd: Data) extends Data(PairType(fst.dataType, snd.dataType))

  object makeArrayData {
    def apply(seq: Data*) = ArrayData(Vector(seq: _*))
  }

  sealed trait AccIdentifier
  case class NamedIdentifier(name: String) extends AccIdentifier
  case class ArrayAccessIdentifier(array: AccIdentifier, index: Nat) extends AccIdentifier
  case class VectorAccessIdentifier(vector: AccIdentifier, index: Nat) extends AccIdentifier
  case class PairIdentifier(fst: AccIdentifier, snd: AccIdentifier) extends AccIdentifier

  implicit def IntToIntData(i: Int): IntData = IntData(i)

  type Store = HashMap[String, Data]

  def eval[T <: PhraseType, R](s: Store, p: Phrase[T])
                              (implicit evaluator: Evaluator[T, R]): R = {
    p match {
      case app: Apply[a, T] =>
        val fun: Phrase[a] => Phrase[T] = eval(s, app.fun)
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


  implicit def UnaryFunctionEvaluator[T1 <: PhraseType,
                                      T2 <: PhraseType]: Evaluator[T1 ->: T2,
                                                                   Phrase[T1] => Phrase[T2]] =
    new Evaluator[T1 ->: T2, Phrase[T1] => Phrase[T2]] {
      def apply(s: Store, p: Phrase[T1 ->: T2]): Phrase[T1] => Phrase[T2] = {
        p match {
          case l: Lambda[T1, T2] =>
            (arg: Phrase[T1]) => l.body `[` arg `/` l.param `]`
          case Identifier(_, _) | Apply(_, _) | DepApply(_, _) |
               IfThenElse(_, _, _) | Proj1(_) | Proj2(_) | LetNat(_, _, _) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def BinaryFunctionEvaluator[T1 <: PhraseType,
                                       T2 <: PhraseType,
                                       T3 <: PhraseType]: Evaluator[T1 ->: T2 ->: T3,
                                                                    Phrase[T1] => Phrase[T2] => Phrase[T3]] =
    new Evaluator[T1 ->: T2 ->: T3, Phrase[T1] => Phrase[T2] => Phrase[T3]] {
      def apply(s: Store, p: Phrase[T1 ->: T2 ->: T3]): Phrase[T1] => Phrase[T2] => Phrase[T3] = {
        p match {
          case l: Lambda[T1, T2 ->: T3] => (arg: Phrase[T1]) =>
            eval(s, l.body `[` arg `/` l.param `]` )(UnaryFunctionEvaluator)

          case Identifier(_, _) | Apply(_, _) | DepApply(_, _) |
               IfThenElse(_, _, _) | Proj1(_) | Proj2(_) | LetNat(_, _, _) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def TrinaryFunctionEvaluator[T1 <: PhraseType,
                                        T2 <: PhraseType,
                                        T3 <: PhraseType,
                                        T4 <: PhraseType]: Evaluator[T1 ->: T2 ->: T3 ->: T4,
                                                                     Phrase[T1] => Phrase[T2] => Phrase[T3] => Phrase[T4]] =
    new Evaluator[T1 ->: T2 ->: T3 ->: T4, Phrase[T1] => Phrase[T2] => Phrase[T3] => Phrase[T4]] {
      def apply(s: Store, p: Phrase[T1 ->: T2 ->: T3 ->: T4]): Phrase[T1] => Phrase[T2] => Phrase[T3] => Phrase[T4] = {
        p match {
          case l: Lambda[T1, T2 ->: T3 ->: T4] => (arg: Phrase[T1]) =>
            eval(s, l.body `[` arg  `/` l.param `]` )(BinaryFunctionEvaluator)

          case Identifier(_, _) | Apply(_, _) | DepApply(_, _) |
               IfThenElse(_, _, _) | Proj1(_) | Proj2(_) | LetNat(_, _, _) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def NatDependentFunctionEvaluator[T <: PhraseType]: Evaluator[`(nat)->:`[T], NatIdentifier => Phrase[T]] =
    new Evaluator[`(nat)->:`[T], NatIdentifier => Phrase[T]] {
      def apply(s: Store, p: Phrase[`(nat)->:`[T]]): NatIdentifier => Phrase[T] = {
        p match {
          case l: DepLambda[NatKind, T] =>
            (arg: NatIdentifier) => l.body `[` arg `/` l.x `]`
          case Identifier(_, _) | Apply(_, _) | DepApply(_, _) |
               IfThenElse(_, _, _) | Proj1(_) | Proj2(_) | LetNat(_, _, _) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def PairEvaluator[T1 <: PhraseType, T2 <: PhraseType]: Evaluator[T1 x T2, (Phrase[T1], Phrase[T2])] =
    new Evaluator[T1 x T2, (Phrase[T1], Phrase[T2])] {
      def apply(s: Store, p: Phrase[T1 x T2]): (Phrase[T1], Phrase[T2]) = {
        p match {
          case i: Identifier[T1 x T2] =>
            (Identifier[T1](i.name, i.t.t1), Identifier[T2](i.name, i.t.t2))
          case pair: PhrasePair[T1, T2] => (pair.fst, pair.snd)
          case Apply(_, _) | DepApply(_, _) |
               IfThenElse(_, _, _) | Proj1(_) | Proj2(_) | LetNat(_, _, _) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def ExpEvaluator: Evaluator[ExpType, Data] =
    new Evaluator[ExpType, Data] {
      def apply(s: Store, p: Phrase[ExpType]): Data = {
        p match {
          case Identifier(name, _) => s(name)

          case Literal(d) => d

          case Natural(n) => NatData(n)

          case UnaryOp(op, x) =>
            op match {
              case Operators.Unary.NEG => - evalIntExp(s, x)
            }

          case BinOp(op, lhs, rhs) =>
            op match {
              case Operators.Binary.ADD => evalIntExp(s, lhs) + evalIntExp(s, rhs)
              case Operators.Binary.SUB => evalIntExp(s, lhs) - evalIntExp(s, rhs)
              case Operators.Binary.MUL => evalIntExp(s, lhs) * evalIntExp(s, rhs)
              case Operators.Binary.DIV => evalIntExp(s, lhs) / evalIntExp(s, rhs)
              case Operators.Binary.MOD => evalIntExp(s, lhs) % evalIntExp(s, rhs)
              case Operators.Binary.GT => if (evalIntExp(s, lhs) > evalIntExp(s, rhs)) 1 else 0
              case Operators.Binary.LT => if (evalIntExp(s, lhs) < evalIntExp(s, rhs)) 1 else 0
            }

          case c: ExpPrimitive => c.eval(s)

          case Apply(_, _) | DepApply(_, _) |
               IfThenElse(_, _, _) | Proj1(_) | Proj2(_) | LetNat(_, _, _) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def AccEvaluator: Evaluator[AccType, AccIdentifier] =
    new Evaluator[AccType, AccIdentifier] {
      def apply(s: Store, p: Phrase[AccType]): AccIdentifier = {
        p match {
          case Identifier(name, _) => NamedIdentifier(name)

          case c: AccPrimitive => c.eval(s)

          case Apply(_, _) | DepApply(_, _) |
               IfThenElse(_, _, _) | Proj1(_) | Proj2(_) | LetNat(_, _, _) =>
            throw new Exception("This should never happen")
        }
      }
    }

  implicit def CommandEvaluator: Evaluator[CommType, Store] =
    new Evaluator[CommType, Store] {
      def apply(s: Store, p: Phrase[CommType]): Store = {
        p match {
          case Identifier(_, _) => throw new Exception("This should never happen")

          case c: CommandPrimitive => c.eval(s)

          case Apply(_, _) | DepApply(_, _) |
               IfThenElse(_, _, _) | Proj1(_) | Proj2(_) | LetNat(_, _, _) =>
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

  def evalIndexExp(s: Store, p: Phrase[ExpType]): Nat = {
    import shine.DPIA.DSL.mapTransientNat
    eval(s, mapTransientNat(p, x => x)) match {
      case NatData(i) => i
      case IntData(i) => i
      case _ => throw new Exception("This should never happen")
    }
  }

  def evalIndexExp(p: Phrase[ExpType]): Nat = {
    evalIndexExp(new Store(), p)
  }

  def evalIntExp(s: Store, p: Phrase[ExpType]): Int = {
    eval(s, p) match {
      case IntData(i) => i
      case IndexData(i, _) => i.eval
      case NatData(i) => i.eval
      case _ => throw new Exception("This should never happen")
    }
  }

  def evalIntExp(p: Phrase[ExpType]): Int = {
    evalIntExp(new Store(), p)
  }

  def toScalaOp(op: Operators.Binary.Value): (Nat, Nat) => Nat = {
    op match {
      case Operators.Binary.ADD => (x, y) => x + y
      case Operators.Binary.SUB => (x, y) => x - y
      case Operators.Binary.MUL => (x, y) => x * y
      case Operators.Binary.DIV => (x, y) => x / y
      case Operators.Binary.MOD => (x, y) => x % y
      case Operators.Binary.GT => (x, y) => ArithExpr.ifThenElse(x gt y, 1 , 0)
      case Operators.Binary.LT => (x, y) => ArithExpr.ifThenElse(x lt y, 1,  0)
    }
  }

}
