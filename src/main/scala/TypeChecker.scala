import PhraseExtensions._

class TypeException(msg : String) extends Exception(msg)

object TypeChecker {

  def error(found : String, expected : String) = {
    throw new TypeException("Type error: found "+found+" expected "+expected)
  }

  def check(current : PhraseType, expected : PhraseType) = {
    if (current != expected) {
      error(current.toString, expected.toString)
    }
  }

  def apply[T <: PhraseType](p : Phrase[T]) : PhraseType = p match {

    case i: Ident[T] =>
      if (i.t == null)
        throw new TypeException("Type error: type not set for "+i)
      i.t

    case Lambda(param, body) => TypeChecker(param) -> TypeChecker(body)

    case Apply(fun, arg) =>
      TypeChecker(fun) match {
        case ft: FunctionType[_, _] =>
          check(ft.inT, TypeChecker(arg))
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

    case New(f) =>
      TypeChecker(f) match {
        case funType@FunctionType(PairType(ExpType(d1), AccType(d2)), CommandType()) =>
          if (d1 == d2) {
            CommandType()
          } else {
            error(d1.toString +" and "+d2.toString, expected="them to match")
          }
        case t => error(t.toString, FunctionType.toString+"("+PairType.toString+
          "("+ExpType.toString+"(A),"+AccType.toString+"(A)),"+CommandType()+")")
      }

    case Assign(lhs, rhs) =>
      (TypeChecker(lhs), TypeChecker(rhs)) match {
        case (at@AccType(d1), et@ExpType(d2)) =>
          if (d1 == d2) {
            CommandType()
          } else {
            error(d1.toString +" and "+d2.toString, expected="them to match")
          }
        case t => error(t.toString, "("+AccType.toString()+"(A),"+ExpType.toString()+"(A))")
      }

    case IfThenElse(cond, thenP, elseP) =>
      val condT = TypeChecker(cond)
      // TODO: Decide on this: the evaluation currently want this to be an int
//        check (condT, ExpType(bool))
      check(condT, ExpType(int))

      val thenPT = TypeChecker(thenP)
      val elsePT = TypeChecker(elseP)
      check(thenPT,elsePT)
      thenPT

    case For(upper, body) =>
      check(TypeChecker(upper), ExpType(int))
      check(TypeChecker(body), FunctionType(ExpType(int), CommandType()))
      CommandType()


    case IntLiteral(i) => ExpType(int)

    case BinOp(op, lhs, rhs) =>
      check(TypeChecker(lhs), ExpType(int))
      check(TypeChecker(rhs), ExpType(int))
      ExpType(int)

    case Map(f, in) =>
      TypeChecker(f) match {
        case funType@FunctionType(ExpType(a), ExpType(b)) =>
          check(TypeChecker(in), ExpType(ArrayType(a)))
          ExpType(ArrayType(b))
        case t => error(t.toString, "FunctionType")
      }

    case Zip(lhs, rhs) =>
      (TypeChecker(lhs), TypeChecker(rhs)) match {
        case (ExpType(ArrayType(a)), ExpType(ArrayType(b))) =>
          ExpType(ArrayType(RecordType(a, b)))
        case t => error(t.toString(), "Zip")
      }

  }

}
