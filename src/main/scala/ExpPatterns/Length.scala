package ExpPatterns

import Core._
import Core.OperationalSemantics._
import opencl.generator.OpenCLAST.{ArithExpression, Expression}

import scala.xml.Elem

case class Length[T <: BasePhraseTypes](array: Phrase[T]) extends ExpPattern with GeneratableExpPattern {

  override lazy val `type` = exp"[$int]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    array.typeCheck()
    array.t match {
      case ExpType(ArrayType(_, _)) =>
      case AccType(ArrayType(_, _)) =>
      case x => error(x.toString, "ArrayType")
    }
  }

  override def inferTypes: Length[T] = Length(TypeInference(array))

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

  override def toOpenCL(env: ToOpenCL.Environment): Expression = {
    ArithExpression( OperationalSemantics.evalIndexExp(new OperationalSemantics.Store(), this) )
  }

  override def prettyPrint: String = s"(length ${PrettyPrinter(array)})"

  override def xmlPrinter: Elem =
    <length>
      {Core.xmlPrinter(array)}
    </length>

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = ???

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = ???
}
