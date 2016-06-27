package ExpPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType.->
import Core.PrettyPrinter.Indent
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.{ArithExpression, Expression, Literal}

case class Length[T <: BasePhraseTypes](array: Phrase[T]) extends ExpPattern with GeneratableExpPattern {

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

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[ExpType] = {
    Length(VisitAndRebuild(array, f))
  }

  override def toOpenCL(ocl: ToOpenCL): Expression = {
    ArithExpression( OperationalSemantics.evalIndexExp(new OperationalSemantics.Store(), this) )
  }

  override def prettyPrint(indent: Indent): String = indent + s"(length ${PrettyPrinter(array)})"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = ???

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = ???
}
