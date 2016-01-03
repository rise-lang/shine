
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

  sealed trait AccIdentifier
  case class NamedIdentifier(name: String) extends AccIdentifier
  case class ArrayAccessIdentifier(array: AccIdentifier, index: Int) extends AccIdentifier

  implicit def toIntData(i: Int): IntData = IntData(i)

  type Store = HashMap[String, Data]

  def substitute[T1 <: PhraseType, T2 <: PhraseType](p1: Phrase[T1],
                                                     p2: Phrase[T1],
                                                     in: Phrase[T2]): Phrase[T2] = {
    if (p2 == in) {
      p1.asInstanceOf[Phrase[T2]] // T1 == T2
    } else {
      in match {
        // these cases must all be `<: Phrase[T2]`, because they match on in.
        // The casts should be unnecessary
        case l: Lambda[_, _] =>
          val newParam = substitute(p1, p2, l.param)
          val newBody = substitute(p1, p2, l.body)
          Lambda(newParam, newBody).asInstanceOf[Phrase[T2]]

        case app: Apply[a, T2] =>
          val newFun = substitute(p1, p2, app.fun)
          val newArg = substitute(p1, p2, app.arg)
          Apply(newFun, newArg)

        case pair: Pair[a, b] =>
          Pair(substitute(p1, p2, pair.fst), substitute(p1, p2, pair.snd)).asInstanceOf[Phrase[T2]]

        case p: Proj1[T2, b] => Proj1(substitute(p1, p2, p.pair))
        case p: Proj2[a, T2] => Proj2(substitute(p1, p2, p.pair))

        case FieldAccess(n, record) =>
          FieldAccess(n, substitute(p1, p2, record)).asInstanceOf[Phrase[T2]]

        case Seq(c1, c2) =>
          Seq(substitute(p1, p2, c1), substitute(p1, p2, c2)).asInstanceOf[Phrase[T2]]

        case NewPhrase(f) =>
          NewPhrase(substitute(p1, p2, f)).asInstanceOf[Phrase[T2]]

        case Assign(lhs, rhs) =>
          Assign(substitute(p1, p2, lhs), substitute(p1, p2, rhs)).asInstanceOf[Phrase[T2]]

        case i: IfThenElse[T2] =>
          val newCond = substitute(p1, p2, i.cond)
          val newThenP = substitute(p1, p2, i.thenP)
          val newElseP = substitute(p1, p2, i.elseP)
          IfThenElse(newCond, newThenP, newElseP)

        case ForPhrase(n, body) =>
          ForPhrase(substitute(p1, p2, n), substitute(p1, p2, body)).asInstanceOf[Phrase[T2]]

        case BinOp(op, lhs, rhs) =>
          BinOp(op, substitute(p1, p2, lhs), substitute(p1, p2, rhs)).asInstanceOf[Phrase[T2]]

        case MapPhrase(f, array) =>
          MapPhrase(substitute(p1, p2, f), substitute(p1, p2, array)).asInstanceOf[Phrase[T2]]

        case ZipPhrase(lhs, rhs) =>
          ZipPhrase(substitute(p1, p2, lhs), substitute(p1, p2, rhs)).asInstanceOf[Phrase[T2]]

        case ReducePhrase(f, init, array) =>
          ReducePhrase(substitute(p1, p2, f), substitute(p1, p2, init), substitute(p1, p2, array)).asInstanceOf[Phrase[T2]]

        case SplitPhrase(n, array) =>
          SplitPhrase(n, substitute(p1, p2, array)).asInstanceOf[Phrase[T2]]

        case JoinPhrase(array) =>
          JoinPhrase(substitute(p1, p2, array)).asInstanceOf[Phrase[T2]]

        case IteratePhrase(n, f, array) =>
          IteratePhrase(n, substitute(p1, p2, f), substitute(p1, p2, array)).asInstanceOf[Phrase[T2]]

        case LengthPhrase(array) => LengthPhrase(substitute(p1, p2, array)).asInstanceOf[Phrase[T2]]

        case ArrayExpAccessPhrase(array, index) =>
          ArrayExpAccessPhrase(substitute(p1, p2, array), substitute(p1, p2, index)).asInstanceOf[Phrase[T2]]

        case ArrayAccAccessPhrase(array, index) =>
          ArrayAccAccessPhrase(substitute(p1, p2, array), substitute(p1, p2, index)).asInstanceOf[Phrase[T2]]

        case Record(fields@_*) =>
          Record(fields.map(f => substitute(p1, p2, f)):_*).asInstanceOf[Phrase[T2]]

        case _: Ident[_]   => in
        case _: IntLiteral => in
        case _: Literal    => in
        case _: SkipPhrase => in
      }
    }
  }

  def evalFunction[T1 <: PhraseType,
                   T2 <: PhraseType](s: Store, p: Phrase[T1 -> T2]): (Phrase[T1] => Phrase[T2]) = {
    p match {
      case i: Ident[T1 -> T2] => throw new Exception("Don't know how to implement this")

      case l: Lambda[T1, T2] =>
        (arg: Phrase[T1]) => substitute(arg, l.param, in=l.body)

      case app: Apply[a, T1 -> T2] =>
        val fun = evalFunction(s, app.fun)
        evalFunction(s, fun(app.arg))

      case p1: Proj1[T1 -> T2, b] =>
        val pair = evalPair(s, p1.pair)
        evalFunction(s, pair._1)

      case p2: Proj2[a, T1 -> T2] =>
        val pair = evalPair(s, p2.pair)
        evalFunction(s, pair._2)

      case ifThenElse: IfThenElse[T1 -> T2] =>
        val cond = evalCondExp(s, ifThenElse.cond)
        if (cond == 0) {
          evalFunction(s, ifThenElse.thenP)
        } else {
          evalFunction(s, ifThenElse.elseP)
        }
    }
  }

  def evalPair[T1 <: PhraseType,
               T2 <: PhraseType](s: Store, p: Phrase[T1 x T2]): (Phrase[T1], Phrase[T2]) = {
    p match {
      case i: Ident[T1 x T2] =>
        (Ident[T1](i.name), Ident[T2](i.name))

      case pair: Pair[T1, T2] => (pair.fst, pair.snd)

      case app: Apply[a, T1 x T2] =>
        val fun = evalFunction(s, app.fun)
        evalPair(s, fun(app.arg))

      case p1: Proj1[T1 x T2, b] =>
        val pair = evalPair(s, p1.pair)
        evalPair(s, pair._1)

      case p2: Proj2[a, T1 x T2] =>
        val pair = evalPair(s, p2.pair)
        evalPair(s, pair._2)

      case ifThenElse: IfThenElse[T1 x T2] =>
        val cond = evalCondExp(s, ifThenElse.cond)
        if (cond == 0) {
          evalPair(s, ifThenElse.thenP)
        } else {
          evalPair(s, ifThenElse.elseP)
        }
    }
  }

  def evalCondExp(s: Store, p: Phrase[ExpType]): Int = {
    evalExp(s, p) match {
      case b: BoolData => if (b.b) { 0 } else { 1 }
      case i: IntData => i.i
    }
  }

  def evalIntExp(s: Store, p: Phrase[ExpType]): Int = {
    evalExp(s, p) match {
      case i: IntData => i.i
    }
  }

  def evalExp(s: Store, p: Phrase[ExpType]): Data = {
    p match {
      case v: Ident[ExpType] => s(v.name)

      case app: Apply[a, ExpType] =>
        val fun = evalFunction(s, app.fun)
        evalExp(s, fun(app.arg))

      case FieldAccess(n, record) =>
        evalExp(s, record) match {
          case r: RecordData => r.fields(n)
        }

      case p1: Proj1[ExpType, b] =>
        val pair = evalPair(s, p1.pair)
        evalExp(s, pair._1)

      case p2: Proj2[a, ExpType] =>
        val pair = evalPair(s, p2.pair)
        evalExp(s, pair._2)

      case ifThenElse: IfThenElse[ExpType] =>
        val cond = evalCondExp(s, ifThenElse.cond)
        if (cond == 0) {
          evalExp(s, ifThenElse.thenP)
        } else {
          evalExp(s, ifThenElse.elseP)
        }

      case IntLiteral(i) => IntData(i)

      case Literal(d) => d

      case op: BinOp =>
        op.op match {
          case BinOp.Op.ADD => evalIntExp(s, op.lhs) + evalIntExp(s, op.rhs)
          case BinOp.Op.SUB => evalIntExp(s, op.lhs) - evalIntExp(s, op.rhs)
          case BinOp.Op.MUL => evalIntExp(s, op.lhs) * evalIntExp(s, op.rhs)
          case BinOp.Op.DIV => evalIntExp(s, op.lhs) / evalIntExp(s, op.rhs)
          case BinOp.Op.MOD => evalIntExp(s, op.lhs) % evalIntExp(s, op.rhs)
        }

      case m: MapPhrase =>
        val f = evalFunction(s, m.f)
        evalExp(s, m.in) match {
          case ArrayData(xs) =>
            ArrayData(xs.map { x => evalExp(s, f(Literal(x))) })
        }

      case z: ZipPhrase =>
        (evalExp(s, z.lhs), evalExp(s, z.rhs)) match {
          case (ArrayData(lhs), ArrayData(rhs)) =>
            ArrayData( (lhs zip rhs) map { p =>
              RecordData(p._1, p._2)
            } )
        }

      case r: ReducePhrase =>
        val f = evalFunction(s, r.f)
        val init = evalExp(s, r.init)
        evalExp(s, r.array) match {
          case ArrayData(xs) =>
            ArrayData(Vector(xs.fold(init) {
              (x, y) => evalExp(s, f(Pair(Literal(x), Literal(y))))
            }))
        }

      case SplitPhrase(n, arrayP) =>
        evalExp(s, arrayP) match {
          case ArrayData(array) =>
            ArrayData(split(n, array).map(ArrayData))
        }

      case JoinPhrase(arrayP) =>
        evalExp(s, arrayP) match {
          case ArrayData(outer) =>
            val arrays = outer.map(row => row match {
              case ArrayData(inner) => inner
            })
            ArrayData(arrays.flatten)
        }

      case i: IteratePhrase =>
        val f = evalFunction(s, i.f)
        evalExp(s, i.array) match {
          case ArrayData(xs) =>
            var array = i.array
            for (_ <- 0 until i.n) { array = f(array) }
            evalExp(s, array)
        }

      case LengthPhrase(arrayP) =>
        arrayP.t match {
          case ExpType(ArrayType(n, _)) => n
          case AccType(ArrayType(n, _)) => n
        }

      case ArrayExpAccessPhrase(array, index) =>
        (evalExp(s, array), evalExp(s, index)) match {
          case (ArrayData(xs), IntData(i)) => xs(i)
        }

      case Record(fields@_*) =>
        RecordData(fields.map(f => evalExp(s, f)):_*)
    }
  }

  def evalAcc(s: Store, p: Phrase[AccType]): AccIdentifier = {
    p match {
      case v: Ident[AccType] => NamedIdentifier(v.name)

      case app: Apply[a, AccType] =>
        val fun = evalFunction(s, app.fun)
        evalAcc(s, fun(app.arg))

      case p1: Proj1[AccType, b] =>
        val pair = evalPair(s, p1.pair)
        evalAcc(s, pair._1)

      case p2: Proj2[a, AccType] =>
        val pair = evalPair(s, p2.pair)
        evalAcc(s, pair._2)

      case ifThenElse: IfThenElse[AccType] =>
        val cond = evalCondExp(s, ifThenElse.cond)
        if (cond == 0) {
          evalAcc(s, ifThenElse.thenP)
        } else {
          evalAcc(s, ifThenElse.elseP)
        }

      case ArrayAccAccessPhrase(arrayP, indexP) =>
        val array = evalAcc(s, arrayP)
        val index = evalExp(s, indexP) match { case IntData(i) => i }
        ArrayAccessIdentifier(array, index)
    }
  }

  def evalCommand(s: Store, p: Phrase[CommandType]): Store = {
    p match {
      case i: Ident[CommandType] =>
        throw new Exception("I don't known how to implement this")

      case app: Apply[a, CommandType] =>
        val fun = evalFunction(s, app.fun)
        evalCommand(s, fun(app.arg))

      case p1: Proj1[CommandType, b] =>
        val pair = evalPair(s, p1.pair)
        evalCommand(s, pair._1)

      case p2: Proj2[a, CommandType] =>
        val pair = evalPair(s, p2.pair)
        evalCommand(s, pair._2)

      case _: SkipPhrase => s

      case Seq(c1, c2) =>
        val s1 = evalCommand(s, c1)
        evalCommand(s1, c2)

      case NewPhrase(f) => evalNew(s, f)

      case Assign(lhs, rhs) =>
        val (identifier, value) = evalAssign(s, evalAcc(s, lhs), evalExp(s, rhs))
        s + (identifier -> value)

      case ifThenElse: IfThenElse[CommandType] =>
        val cond = evalCondExp(s, ifThenElse.cond)
        if (cond == 0) {
          evalCommand(s, ifThenElse.thenP)
        } else {
          evalCommand(s, ifThenElse.elseP)
        }

      case ForPhrase(nP, bodyP) =>
        val n = evalIntExp(s, nP)
        val body = evalFunction(s, bodyP)
        var s1 = s
        for (i <- 0 until n) {
          s1 = evalCommand(s1, body(IntLiteral(i)))
        }
        s1
    }
  }

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

  def evalNew(s: Store, f: Phrase[ ExpType x AccType -> CommandType ]): Store = {
    f match {
      case v: Ident[ExpType x AccType -> CommandType] =>
        throw new Exception("I don't known how to implement this")

      case l: Lambda[ExpType x AccType, CommandType] =>
        val params = evalPair(s, l.param)
        evalAcc(s, params._2) match {
          case NamedIdentifier(address) =>
            val s1 = evalCommand(s + (address -> 0), l.body)
            s1 - address
          case ArrayAccessIdentifier(baseAddress, offset) =>
            throw new Exception("I don't known how to implement this")
        }

      case app: Apply[a, ExpType x AccType -> CommandType] =>
        val fun = evalFunction(s, app.fun)
        evalNew(s, fun(app.arg))

      case p1: Proj1[ExpType x AccType -> CommandType, b] =>
        val pair = evalPair(s, p1.pair)
        evalNew(s, pair._1)

      case p2: Proj2[a, ExpType x AccType -> CommandType] =>
        val pair = evalPair(s, p2.pair)
        evalNew(s, pair._2)

      case ifThenElse: IfThenElse[ExpType x AccType -> CommandType] =>
        val cond = evalCondExp(s, ifThenElse.cond)
        if (cond == 0) {
          evalNew(s, ifThenElse.thenP)
        } else {
          evalNew(s, ifThenElse.elseP)
        }
    }
  }

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

}
