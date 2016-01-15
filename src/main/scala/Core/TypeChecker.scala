package Core

import PhraseType._

class TypeException(msg: String) extends Exception(msg)

object TypeChecker {

  def error(found: String, expected: String) = {
    throw new TypeException("Type error: found " + found + " expected " + expected)
  }

  def check(current: PhraseType, expected: PhraseType) = {
    if (current != expected) {
      error(current.toString, expected.toString)
    }
  }

  def setParamType[T1 <: PhraseType, T2 <: PhraseType](p: Phrase[T1 -> T2], t: T1): Unit = {
    p match {
      case l: Lambda[T1, T2] =>
        l.param.t match {
          case null => l.param.t = t // infer the type if not set
          case _ =>
        }

      case app: Apply[a, T1 -> T2] =>
        val fun = Lift.liftFunction(app.fun)
        setParamType(fun(app.arg), t)

      case p1: Proj1[T1 -> T2, b] =>
        val pair = Lift.liftPair(p1.pair)
        setParamType(pair._1, t)

      case p2: Proj2[a, T1 -> T2] =>
        val pair = Lift.liftPair(p2.pair)
        setParamType(pair._2, t)

      case IfThenElse(_, thenP, elseP) =>
        setParamType(thenP, t)
        setParamType(elseP, t)

      case Ident(_) => throw new Exception("This should never happen")
    }
  }

  def apply[T <: PhraseType](p: Phrase[T]): T = {
    val phraseType = (p match {

      case i: Ident[T] =>
        if (i.t == null)
          throw new TypeException("Type error: type not set for " + i)
        i.t

      case Lambda(param, body) =>
        TypeChecker(param) -> TypeChecker(body)

      case Apply(fun, arg) =>
        setParamType(fun, TypeChecker(arg))
        TypeChecker(fun) match {
          case ft: FunctionType[_, _] =>
            check(TypeChecker(arg), ft.inT)
            ft.outT
          case t => error(t.toString, FunctionType.toString)
        }

      case Pair(a, b) => TypeChecker(a) x TypeChecker(b)

      case Proj1(pair) =>
        TypeChecker(pair) match {
          case pt: PairType[_, _] => pt.t1
          case t => error(t.toString, PairType.toString)
        }

      case Proj2(pair) =>
        TypeChecker(pair) match {
          case pt: PairType[_, _] => pt.t2
          case t => error(t.toString, PairType.toString)
        }

      case Record(fields@_*) =>
        ExpType(RecordType( fields.map(f => TypeChecker(f).dataType):_* ))

      case FieldAccess(n, record) =>
        TypeChecker(record) match {
          case ExpType(RecordType(fields@_*)) => ExpType(fields(n))
          case t => error(t.toString, "Something else")
        }

      case LengthPhrase(array) =>
        TypeChecker(array) match {
          case ExpType(ArrayType(n, t)) => ExpType(int)
          case AccType(ArrayType(n, t)) => ExpType(int)
          case t => error(t.toString, "ArrayType")
        }

      case ArrayExpAccessPhrase(array, index) =>
        check(TypeChecker(index), ExpType(int))
        TypeChecker(array) match {
          case ExpType(ArrayType(n, t)) => ExpType(t)
          case t => error(t.toString, "ArrayType")
        }

      case ArrayAccAccessPhrase(array, index) =>
        check(TypeChecker(index), ExpType(int))
        TypeChecker(array) match {
          case AccType(ArrayType(n, t)) => AccType(t)
          case t => error(t.toString, "ArrayType")
        }

      case _: SkipPhrase => CommandType()

      case Seq(c1, c2) =>
        check(TypeChecker(c1), CommandType())
        check(TypeChecker(c2), CommandType())
        CommandType()

      case NewPhrase(f) =>
        TypeChecker(f) match {
          case FunctionType(PairType(ExpType(d1), AccType(d2)), CommandType()) =>
            if (d1 == d2) {
              CommandType()
            } else {
              error(d1.toString + " and " + d2.toString, expected = "them to match")
            }
          case t => error(t.toString, FunctionType.toString + "(" + PairType.toString +
            "(" + ExpType.toString + "(A)," + AccType.toString + "(A))," + CommandType() + ")")
        }

      case Assign(lhs, rhs) =>
        (TypeChecker(lhs), TypeChecker(rhs)) match {
          case (AccType(d1), ExpType(d2)) =>
            if (d1 == d2) {
              CommandType()
            } else {
              error(d1.toString + " and " + d2.toString, expected = "them to match")
            }
          case t => error(t.toString, "(" + AccType.toString() + "(A)," + ExpType.toString() + "(A))")
        }

      case IfThenElse(cond, thenP, elseP) =>
        val condT = TypeChecker(cond)
        if (condT != ExpType(int) && condT != ExpType(bool)) {
          error(condT.toString, expected = "int or boolean")
        }

        val thenPT = TypeChecker(thenP)
        val elsePT = TypeChecker(elseP)
        check(thenPT, elsePT)
        thenPT

      case ForPhrase(upper, body) =>
        check(TypeChecker(upper), ExpType(int))
        check(TypeChecker(body), FunctionType(ExpType(int), CommandType()))
        CommandType()

      case Literal(d) => ExpType(d.dataType)

      case BinOp(op, lhs, rhs) =>
        check(TypeChecker(lhs), ExpType(int))
        check(TypeChecker(rhs), ExpType(int))
        ExpType(int)

      case PatternPhrase(pattern) => pattern.typeCheck()

    }).asInstanceOf[T]
    p.t = phraseType
    p.t
  }

}
