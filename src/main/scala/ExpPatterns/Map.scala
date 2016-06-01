package ExpPatterns

import CommandPatterns._
import Core._
import Core.PhraseType._
import Core.OperationalSemantics._
import DSL._
import Rewriting.RewriteToImperative

abstract class AbstractMap(f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) extends ExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(array) match {
      case ExpType(ArrayType(n, dt1)) =>
        setParamType(f, ExpType(dt1))
        TypeChecker(f) match {
          case FunctionType(ExpType(t_), ExpType(dt2)) =>
            if (dt1 == t_) {
              outDataType = ArrayType(n, dt2)
              ExpType(outDataType)
            } else {
              error(dt1.toString + " and " + t_.toString, expected = "them to match")
            }
          case t_ => error(t_.toString, "FunctionType")
        }
      case t_ => error(t_.toString, "ArrayType")
    }
  }

  override def eval(s: Store): Data = {
    import OperationalSemantics._
    val fE = OperationalSemantics.eval(s, f)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        ArrayData(xs.map { x => OperationalSemantics.eval(s, fE(LiteralPhrase(x))) })

      case _ => throw new Exception("This should not happen")
    }
  }

  private var outDataType: DataType = null

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    RewriteToImperative.exp(array, λ(array.t) { x =>
      makeMapI(A,
        λ(A.t) { o => λ(array.t) { x => RewriteToImperative.acc(f(x), o) } },
        x
      )
    })
  }

  def makeMapI: (Phrase[AccType], Phrase[AccType -> (ExpType -> CommandType)], Phrase[ExpType]) => AbstractMapI

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    `new`(outDataType, tmp =>
      RewriteToImperative.acc(this, π2(tmp)) `;`
        C(π1(tmp))
    )
  }
}

case class Map(f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) extends AbstractMap(f, array) {

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    Map(OperationalSemantics.substitute(phrase, `for`, f), OperationalSemantics.substitute(phrase, `for`, array))
  }

  override def toC = ???

  override def prettyPrint: String = s"(map ${PrettyPrinter(f)} ${PrettyPrinter(array)})"

  override def makeMapI = MapI
}