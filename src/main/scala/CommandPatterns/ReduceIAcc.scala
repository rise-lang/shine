package CommandPatterns

import Core._
import Core.PhraseType._
import Core.OperationalSemantics._
import ExpPatterns.Idx

case class ReduceIAcc(out: Phrase[AccType],
                      f: Phrase[AccType -> (ExpType -> (ExpType -> CommandType))],
                      init: Phrase[ExpType],
                      in: Phrase[ExpType]) extends CommandPattern {

  override def typeCheck(): CommandType = {
    import TypeChecker._
    (TypeChecker(out), TypeChecker(init), TypeChecker(in)) match {
      case (AccType(dt2), ExpType(t), ExpType(ArrayType(n, dt1))) if dt2 == t =>
        setParamType(f, AccType(dt2))
        setSecondParamType(f, ExpType(dt1))
        setThirdParamType(f, ExpType(dt2))
        TypeChecker(f) match {
          case FunctionType(AccType(t1), FunctionType(ExpType(t2), FunctionType(ExpType(t3), CommandType()))) =>
            if (dt2 == t1 && dt1 == t2 && dt2 == t3) CommandType()
            else {
              error(dt2.toString + ", " + t1.toString + " as well as " +
                dt1.toString + ", " + t2.toString + " and " + dt2.toString + ", " + t3.toString,
                expected = "them to match")
            }
          case ty => error(ty.toString, "FunctionType")
        }
      case t => error(t.toString, "(AccType, ExpType, ArrayType)")
    }
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): CommandPattern = {
    ReduceIAcc(
      OperationalSemantics.substitute(phrase, `for`, out),
      OperationalSemantics.substitute(phrase, `for`, f),
      OperationalSemantics.substitute(phrase, `for`, init),
      OperationalSemantics.substitute(phrase, `for`, in))
  }

  override def eval(s: Store): Store = {
    val fE = OperationalSemantics.eval(s, f)(TrinaryFunctionEvaluator)
    val n = TypeChecker(in) match { case ExpType(ArrayType(len, _)) => len }

    (0 until n).foldLeft(s)( (sOld, i) => {
      val comm = fE(out)(Idx(in, LiteralPhrase(i)).asPhrase)(init)
      OperationalSemantics.eval(sOld, comm)
    } )
  }

  override def toC = ???

}
