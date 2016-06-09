package ExpPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType.->
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.{ArithExpression, Expression, Literal}

case class Length[T <: BasePhraseTypes](array: Phrase[T]) extends ExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(array) match {
      case ExpType(ArrayType(_, _)) => ExpType(int)
      case AccType(ArrayType(_, _)) => ExpType(int)
      case x => error(x.toString, "ArrayType")
    }
  }

  override def eval(s: Store): Data = {
    array.t match {
      case ExpType(ArrayType(n, _)) => IndexData(n)
      case AccType(ArrayType(n, _)) => IndexData(n)
      case _ => throw new Exception(s"This should not happen (${array.t})")
    }
  }

  override def substitute[U <: PhraseType](phrase: Phrase[U], `for`: Phrase[U]): ExpPattern = {
    Length(OperationalSemantics.substitute(phrase, `for`, array))
  }

  override def toC = array.t match {
    case ExpType(ArrayType(n, _)) => n.toString
    case AccType(ArrayType(n, _)) => n.toString
  }

  override def toOpenCL: Expression = {
    ArithExpression( OperationalSemantics.evalIndexExp(new OperationalSemantics.Store(), this) )
  }

  override def toOpenCL(arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): Expression = ???

  override def prettyPrint: String = s"(length ${PrettyPrinter(array)})"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = ???

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = ???
}
