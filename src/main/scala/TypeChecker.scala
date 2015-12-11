
class TypeException(msg : String) extends Exception(msg)

object TypeChecker {


  def error(current : PhraseType, expected : PhraseType) = {
      throw new TypeException("Type error: found "+current+" expected "+expected)
  }

  def check(current : PhraseType, expected : PhraseType) = {
    if (current != expected) {
      error(current, expected)
    }
  }

  def apply(p : Phrase) : PhraseType = {
    p match {

      case i:Ident =>
        if (i.t == null)
          throw new TypeException("Type error: type not set for "+i)
        i.t
      case l:Lambda => Function(TypeChecker(l.param), TypeChecker(l.body))
      case a:Apply =>
        val funType = TypeChecker(a.fun)
        val argType = TypeChecker(a.arg)
        funType match {
          case ft:Function =>
            check(ft.inT, argType)
            ft.outT
          case _ => throw new TypeException("Type error: found "+funType+" expected "+Function.toString())
        }

      case p:Pair => Tuple(TypeChecker(p.a), TypeChecker(p.b))

      case proj:Proj0 =>
        val p0T = TypeChecker(proj.p)
        p0T match {
          case pairT:Tuple => pairT.t1
          case _ => throw new TypeException("Type error: found "+p0T+" expected "+Tuple.toString())
        }
      case proj:Proj1 =>
        val p1T = TypeChecker(proj.p)
        p1T match {
          case pairT:Tuple => pairT.t2
          case _ => throw new TypeException("Type error: found "+p1T+" expected "+Tuple.toString())
        }

        case _:Skip =>
          Command

        case Seq(c1, c2) =>
          check(TypeChecker(c1), Command)
          check(TypeChecker(c2), Command)
          Function(Tuple(Command, Command),Command)

        case New(f : Phrase) =>
          TypeChecker(f) match {
            case funType@Function(Tuple(Exp(d1),Acc(d2)),Command) if d1 == d2 =>
              PassiveFunction(funType, Command)
            case t => throw new TypeException("Type error: found "+t+" expected "+Function.toString+"("+Tuple.toString+"("+Exp.toString+"(A),"+Acc.toString+"(A)),"+Command+")")
          }

      case Assign(lhs, rhs) =>
        (TypeChecker(lhs), TypeChecker(rhs)) match {
          case (at@Acc(d1), et@Exp(d2)) if d1 == d2 =>
            PassiveFunction(Tuple(at, et), Command)
          case t => throw new TypeException("Type error: found "+t+" expected ("+Acc.toString()+"(A),"+Exp.toString()+"(A))")
        }

        case IfThenElse(cond : Phrase, thenP : Phrase, elseP : Phrase) =>
          val condT = TypeChecker(cond)
          check (condT, Exp(bool))
          val thenPT = TypeChecker(thenP)
          val elsePT = TypeChecker(elseP)
          check(thenPT,elsePT)
          PassiveFunction(Tuple(Exp(bool),Tuple(thenPT, elsePT)),thenPT)

//        case For(upper : Phrase, body: Phrase) =>


//        case class IntLitteral(i : Int) extends Phrase
//
//
//        object BinOp {
//          class Op extends Enumeration {
//            val ADD = Value("+")
//            val SUB = Value("-")
//            val MUL = Value("*")
//            val DIV = Value("/")
//            val MOD = Value("%")
//          }
//        }
//
//        case class BinOp(op : BinOp.Op, lhs : Phrase, rhs : Phrase) extends Phrase

    }
  }


}
