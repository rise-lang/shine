
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

  def apply(p : Phrase) : PhraseType = {
    p match {

      case i: Ident =>
        if (i.t == null)
          throw new TypeException("Type error: type not set for "+i)
        i.t

      case l: Lambda => FunctionType(TypeChecker(l.param), TypeChecker(l.body))

      case a: Apply =>
        TypeChecker(a.fun) match {
          case ft: FunctionType =>
            check(ft.inT, TypeChecker(a.arg))
            ft.outT
          case t => error(t.toString, FunctionType.toString)
        }

      case p: Pair => PairType(TypeChecker(p.a), TypeChecker(p.b))

      case proj: Proj0 =>
        TypeChecker(proj.p) match {
          case pt: PairType => pt.t1
          case t => error(t.toString, PairType.toString)
        }

      case proj: Proj1 =>
        TypeChecker(proj.p) match {
          case pt: PairType => pt.t2
          case t => error(t.toString, PairType.toString)
        }

      case _: Skip => Command

      case Seq(c1, c2) =>
        check(TypeChecker(c1), Command)
        check(TypeChecker(c2), Command)
        FunctionType(PairType(Command, Command),Command)

      case New(f : Phrase) =>
        TypeChecker(f) match {
          case funType@FunctionType(PairType(ExpType(d1),AccType(d2)),Command) =>
            if (d1 == d2) {
              PassiveFunctionType(funType, Command)
            } else {
              error(d1.toString +" and "+d2.toString, "them to match")
            }
          case t => error(t.toString, FunctionType.toString+"("+PairType.toString+"("+ExpType.toString+"(A),"+AccType.toString+"(A)),"+Command+")")
        }

      case Assign(lhs, rhs) =>
        (TypeChecker(lhs), TypeChecker(rhs)) match {
          case (at@AccType(d1), et@ExpType(d2)) =>
            if (d1 == d2) {
              PassiveFunctionType(PairType(at, et), Command)
            } else {
              error(d1.toString +" and "+d2.toString, "them to match")
            }
          case t => error(t.toString, "("+AccType.toString()+"(A),"+ExpType.toString()+"(A))")
        }

      case IfThenElse(cond : Phrase, thenP : Phrase, elseP : Phrase) =>
        val condT = TypeChecker(cond)
        check (condT, ExpType(bool))
        val thenPT = TypeChecker(thenP)
        val elsePT = TypeChecker(elseP)
        check(thenPT,elsePT)
        PassiveFunctionType(PairType(ExpType(bool),PairType(thenPT, elsePT)),thenPT)

      case For(upper : Phrase, body: Phrase) =>
        check(TypeChecker(upper), ExpType(int))
        check(TypeChecker(body), Command)
        FunctionType(PairType(ExpType(int), FunctionType(ExpType(int), Command)), Command)


      case IntLiteral(i : Int) => ExpType(int)

      case BinOp(op : BinOp.Op.Value, lhs : Phrase, rhs : Phrase) =>
        check(TypeChecker(lhs), ExpType(int))
        check(TypeChecker(rhs), ExpType(int))
        ExpType(int)
    }
  }


}
