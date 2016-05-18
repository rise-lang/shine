package CommandPatterns

import Core._
import Core.OperationalSemantics._

case class Assign(lhs: Phrase[AccType], rhs: Phrase[ExpType]) extends CommandPattern {

  override def typeCheck(): CommandType = {
    import TypeChecker._
    (TypeChecker(lhs), TypeChecker(rhs)) match {
      case (AccType(d1), ExpType(d2)) =>
        if (d1 == d2) {
          CommandType()
        } else {
          error(d1.toString + " and " + d2.toString, expected = "them to match")
        }
      case t => error(t.toString, "(" + AccType.toString() + "(A)," + ExpType.toString() + "(A))")
    }
  }

  override def eval(s: Store): Store = {
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

    evalAssign(s, OperationalSemantics.eval(s, lhs), OperationalSemantics.eval(s, rhs), (s, identifier, value) => {
      s + (identifier -> value)
    })
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): CommandPattern = {
    Assign(
      OperationalSemantics.substitute(phrase, `for`, lhs),
      OperationalSemantics.substitute(phrase, `for`, rhs))
  }

  override def toC = Printer.toC(lhs) + " = " + Printer.toC(rhs) + ";\n"

}
