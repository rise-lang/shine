import PhraseExtensions._
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
      case l: Lambda[T1, T2] => setIdentType(l.param, t)
      case i: Ident[T1 -> T2] =>
      case app: Apply[a, T1 -> T2] =>
      //      case ArrayAccess(arrayP, indexP) =>
      case p1: Proj1[T1 -> T2, b] =>
      case p2: Proj2[a, T1 -> T2] =>
      case ifThenElse: IfThenElse[T1 -> T2] =>
      //      case IteratePhrase(n, fP, inP) =>
    }
  }

  def setIdentType[T <: PhraseType](p: Phrase[T], t: PhraseType): Unit = {
    p match {
      case i: Ident[T] =>
        i.t match {
          case null => i.t = t.asInstanceOf[T] // infer the type if not set
          case _ =>
        }
      case Pair(lhs, rhs) =>
        t match {
          case pt: PairType[_, _] =>
            setIdentType(lhs, pt.t1)
            setIdentType(rhs, pt.t2)
          case _ => error(t.toString, "PairType")
        }
      case Proj1(pair) =>
        setIdentType(pair, t)
      case Proj2(pair) =>
        setIdentType(pair, t)

      case app: Apply[a, T] =>
      case ArrayAccAccess(array, index) =>
      case ArrayExpAccess(array, index) =>
      case Assign(lhs, rhs) =>
      case BinOp(lhs, op, rhs) =>
      case FieldAccess(n, record) =>
      case ForPhrase(n, body) =>
      case IfThenElse(cond, theP, elseP) =>
      case IntLiteral(i) =>
      case Lambda(param, body) =>
      case Length(array) =>
      case Literal(l) =>
      case Map(f, array) =>
      case NewPhrase(f) =>
      case Seq(c1, c2) =>
      case SkipPhrase() =>
      case Zip(lhs, rhs) =>
    }
  }

  def apply[T <: PhraseType](p: Phrase[T]): T = {
    val phraseType = p match {

      case i: Ident[T] =>
        if (i.t == null)
          throw new TypeException("Type error: type not set for " + i)
        i.t

      case Lambda(param, body) => TypeChecker(param) -> TypeChecker(body)

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

      case FieldAccess(n, record) =>
        TypeChecker(record) match {
          case ExpType(RecordType(fields@_*)) => ExpType(fields(n))
          case t => error(t.toString, "Something else")
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
        // TODO: Decide on this: the evaluation currently want this to be an int
        //        check (condT, ExpType(bool))
        check(condT, ExpType(int))

        val thenPT = TypeChecker(thenP)
        val elsePT = TypeChecker(elseP)
        check(thenPT, elsePT)
        thenPT

      case ForPhrase(upper, body) =>
        check(TypeChecker(upper), ExpType(int))
        check(TypeChecker(body), FunctionType(ExpType(int), CommandType()))
        CommandType()


      case IntLiteral(i) => ExpType(int)

      case Literal(d) => ExpType(d.dataType)

      case BinOp(op, lhs, rhs) =>
        check(TypeChecker(lhs), ExpType(int))
        check(TypeChecker(rhs), ExpType(int))
        ExpType(int)

      case Map(f, array) =>
        TypeChecker(array) match {
          case ExpType(ArrayType(n, dt)) =>
            setParamType(f, ExpType(dt))
            TypeChecker(f) match {
              case FunctionType(ExpType(t1), ExpType(t2)) =>
                if (dt == t1) ExpType(ArrayType(n, t2))
                else {
                  error(dt.toString + " and " + t1.toString,
                    expected = "them to match")
                }
              case t => error(t.toString, "FunctionType")
            }
          case t => error(t.toString, "ArrayType")
        }

      case Zip(lhs, rhs) =>
        (TypeChecker(lhs), TypeChecker(rhs)) match {
          case (ExpType(ArrayType(n, a)), ExpType(ArrayType(m, b))) if n == m =>
            ExpType(ArrayType(n, RecordType(a, b)))
          case t => error(t.toString(), "PairOfArrayTypes")
        }

      case Length(array) =>
        TypeChecker(array) match {
          case ExpType(ArrayType(n, t)) => ExpType(int)
          case AccType(ArrayType(n, t)) => ExpType(int)
          case t => error(t.toString, "ArrayType")
        }

      case ArrayExpAccess(array, index) =>
        TypeChecker(array) match {
          case ExpType(ArrayType(n, t)) => ExpType(t)
          case t => error(t.toString, "ArrayType")
        }

      case ArrayAccAccess(array, index) =>
        TypeChecker(array) match {
          case AccType(ArrayType(n, t)) => AccType(t)
          case t => error(t.toString, "ArrayType")
        }
    }
    p.t = phraseType.asInstanceOf[T]
    p.t
  }

}
