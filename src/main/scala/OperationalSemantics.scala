
import PhraseType._

import scala.collection.immutable.HashMap
import scala.language.implicitConversions

object OperationalSemantics {

  //sealed
  abstract class Data(val dataType: DataType)
  case class BoolData(b: Boolean) extends Data(bool)
  case class IntData(i: Int) extends Data(int)
  case class Int4Data(i0: Int, i1: Int, i2: Int, i3: Int) extends Data(int4)
  case class FloatData(f: Float) extends Data(float)
  case class ArrayData(a: Vector[Data]) extends Data(ArrayType(a.length, a.head.dataType))
  case class RecordData(fields: Data*) extends Data(RecordType(fields.map(_.dataType):_*))

  object makeArrayData {
    def apply(seq: Data*) = ArrayData(Vector(seq: _*))
  }

  sealed trait AccIdentifier
  case class NamedIdentifier(name: String) extends AccIdentifier
  case class ArrayAccessIdentifier(array: AccIdentifier, index: Int) extends AccIdentifier

  implicit def IntToIntData(i: Int): IntData = IntData(i)

  type Store = HashMap[String, Data]

  def substitute[T1 <: PhraseType, T2 <: PhraseType](p1: Phrase[T1],
                                                     p2: Phrase[T1],
                                                     in: Phrase[T2]): Phrase[T2] = {
    if (p2 == in) {
      p1.asInstanceOf[Phrase[T2]] // T1 == T2
    } else {
      val res = in match {
        // these cases must all be `<: Phrase[T2]`, because they match on in.
        // The casts should be unnecessary
        case l: Lambda[_, _] =>
          val newParam = substitute(p1, p2, l.param).asInstanceOf[Ident[PhraseType]]
          val newBody = substitute(p1, p2, l.body)
          Lambda(newParam, newBody)

        case app: Apply[a, T2] =>
          val newFun = substitute(p1, p2, app.fun)
          val newArg = substitute(p1, p2, app.arg)
          Apply(newFun, newArg)

        case pair: Pair[a, b] =>
          Pair(substitute(p1, p2, pair.fst), substitute(p1, p2, pair.snd))

        case p: Proj1[T2, b] => Proj1(substitute(p1, p2, p.pair))
        case p: Proj2[a, T2] => Proj2(substitute(p1, p2, p.pair))

        case FieldAccess(n, record) =>
          FieldAccess(n, substitute(p1, p2, record))

        case Seq(c1, c2) =>
          Seq(substitute(p1, p2, c1), substitute(p1, p2, c2))

        case NewPhrase(f) =>
          NewPhrase(substitute(p1, p2, f))

        case Assign(lhs, rhs) =>
          Assign(substitute(p1, p2, lhs), substitute(p1, p2, rhs))

        case i: IfThenElse[T2] =>
          val newCond = substitute(p1, p2, i.cond)
          val newThenP = substitute(p1, p2, i.thenP)
          val newElseP = substitute(p1, p2, i.elseP)
          IfThenElse(newCond, newThenP, newElseP)

        case ForPhrase(n, body) =>
          ForPhrase(substitute(p1, p2, n), substitute(p1, p2, body))

        case BinOp(op, lhs, rhs) =>
          BinOp(op, substitute(p1, p2, lhs), substitute(p1, p2, rhs))

        case MapPhrase(f, array) =>
          MapPhrase(substitute(p1, p2, f), substitute(p1, p2, array))

        case ZipPhrase(lhs, rhs) =>
          ZipPhrase(substitute(p1, p2, lhs), substitute(p1, p2, rhs))

        case ReducePhrase(f, init, array) =>
          ReducePhrase(substitute(p1, p2, f), substitute(p1, p2, init), substitute(p1, p2, array))

        case SplitPhrase(n, array) =>
          SplitPhrase(n, substitute(p1, p2, array))

        case JoinPhrase(array) =>
          JoinPhrase(substitute(p1, p2, array))

        case IteratePhrase(n, f, array) =>
          IteratePhrase(n, substitute(p1, p2, f), substitute(p1, p2, array))

        case LengthPhrase(array) => LengthPhrase(substitute(p1, p2, array))

        case ArrayExpAccessPhrase(array, index) =>
          ArrayExpAccessPhrase(substitute(p1, p2, array), substitute(p1, p2, index))

        case ArrayAccAccessPhrase(array, index) =>
          ArrayAccAccessPhrase(substitute(p1, p2, array), substitute(p1, p2, index))

        case Record(fields@_*) =>
          Record(fields.map(f => substitute(p1, p2, f)):_*)

        case _: Ident[_]   => in
        case _: IntLiteral => in
        case _: Literal    => in
        case _: SkipPhrase => in
      }
      res.asInstanceOf[Phrase[T2]]
    }
  }

  def eval[T <: PhraseType, R](s: Store, p: Phrase[T])
                              (implicit evaluator: Evaluator[T, R]): R = {
    import implicits._
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

  object implicits {

    implicit def FunctionEvaluator[T1 <: PhraseType, T2 <: PhraseType]: Evaluator[T1 -> T2, (Phrase[T1] => Phrase[T2])] =
      new Evaluator[T1 -> T2, (Phrase[T1] => Phrase[T2])] {
        def apply(s: Store, p: Phrase[T1 -> T2]): (Phrase[T1] => Phrase[T2]) = {
          p match {
            case l: Lambda[T1, T2] => (arg: Phrase[T1]) => substitute(arg, l.param, in = l.body)
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

            case FieldAccess(n, record) =>
              val data: Data = eval(s, record)
              data match {
                case r: RecordData => r.fields(n)
              }

            case IntLiteral(i) => IntData(i)

            case Literal(d) => d

            case BinOp(op, lhs, rhs) =>
              op match {
                case BinOp.Op.ADD => evalIntExp(s, lhs) + evalIntExp(s, rhs)
                case BinOp.Op.SUB => evalIntExp(s, lhs) - evalIntExp(s, rhs)
                case BinOp.Op.MUL => evalIntExp(s, lhs) * evalIntExp(s, rhs)
                case BinOp.Op.DIV => evalIntExp(s, lhs) / evalIntExp(s, rhs)
                case BinOp.Op.MOD => evalIntExp(s, lhs) % evalIntExp(s, rhs)
              }

            case MapPhrase(fP, in) =>
              val f = eval(s, fP)
              eval(s, in) match {
                case ArrayData(xs) =>
                  ArrayData(xs.map { x => eval(s, f(Literal(x))) })
              }

            case ZipPhrase(lhsP, rhsP) =>
              (eval(s, lhsP), eval(s, rhsP)) match {
                case (ArrayData(lhs), ArrayData(rhs)) =>
                  ArrayData((lhs zip rhs) map { p =>
                    RecordData(p._1, p._2)
                  })
              }

            case ReducePhrase(fP, initP, arrayP) =>
              val f = eval(s, fP)
              val init = eval(s, initP)
              eval(s, arrayP) match {
                case ArrayData(xs) =>
                  ArrayData(Vector(xs.fold(init) {
                    (x, y) => eval(s, f(Pair(Literal(x), Literal(y))))
                  }))
              }

            case SplitPhrase(n, arrayP) =>
              eval(s, arrayP) match {
                case ArrayData(array) =>

                  def split[T](n: Int, vector: Vector[T]): Vector[Vector[T]] = {
                    val builder = Vector.newBuilder[Vector[T]]
                    var vec = vector
                    for (i <- 0 until vector.length / n) {
                      val (head, tail) = vec splitAt n
                      vec = tail
                      builder += head
                    }
                    builder.result()
                  }

                  ArrayData(split(n, array).map(ArrayData))
              }

            case JoinPhrase(arrayP) =>
              eval(s, arrayP) match {
                case ArrayData(outer) =>
                  val arrays = outer.map(row => row match {
                    case ArrayData(inner) => inner
                  })
                  ArrayData(arrays.flatten)
              }

            case IteratePhrase(n, fP, arrayP) =>
              val f = eval(s, fP)
              eval(s, arrayP) match {
                case ArrayData(xs) =>
                  var array = arrayP
                  for (_ <- 0 until n) {
                    array = f(array)
                  }
                  eval(s, array)
              }

            case LengthPhrase(arrayP) =>
              arrayP.t match {
                case ExpType(ArrayType(n, _)) => n
                case AccType(ArrayType(n, _)) => n
              }

            case ArrayExpAccessPhrase(array, index) =>
              (eval(s, array), eval(s, index)) match {
                case (ArrayData(xs), IntData(i)) => xs(i)
              }

            case Record(fields@_*) =>
              RecordData(fields.map(f => eval(s, f)): _*)

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
              val arg = Ident[ExpType x AccType](λ.newName())
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
                      case ArrayData(vec) =>
                        (name, ArrayData(vec.updated(index, rhsValue)))
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
                s1 = eval(s1, body(IntLiteral(i)))
              }
              s1

            case Apply(_, _) | IfThenElse(_, _, _) | Proj1(_) | Proj2(_) =>
              throw new Exception("This should never happen")
          }
        }
      }

  }

  def evalCondExp(s: Store, p: Phrase[ExpType]): Boolean = {
    import implicits._
    eval(s, p) match {
      case BoolData(b) => b
      case IntData(i)  => i == 0
      case _ => throw new Exception("This should never happen")
    }
  }

  def evalIntExp(s: Store, p: Phrase[ExpType]): Int = {
    import implicits._
    eval(s, p) match {
      case IntData(i) => i
      case _ => throw new Exception("This should never happen")
    }
  }

}
