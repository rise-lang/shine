package ExpPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType.->
import apart.arithmetic.{ArithExpr, NamedVar}
import opencl.generator.OpenCLAST.{Expression, VarRef}

case class Idx(array: Phrase[ExpType], index: Phrase[ExpType]) extends ExpPattern {

  private var dt: DataType = null

  override def typeCheck(): ExpType = {
    import TypeChecker._
    check(TypeChecker(index), ExpType(int))
    TypeChecker(array) match {
      case ExpType(ArrayType(_, dt_)) =>
        dt = dt_
        ExpType(dt)
      case x => error(x.toString, "ArrayType")
    }
  }

  override def eval(s: Store): Data = {
    (OperationalSemantics.eval(s, array), OperationalSemantics.eval(s, index)) match {
      case (ArrayData(xs), IntData(i)) => xs(i)
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[ExpType] = {
    val i = Idx(VisitAndRebuild(array, f), VisitAndRebuild(index, f))
    i.dt = dt
    i
  }

  override def toOpenCL: Expression = ToOpenCL.exp(this, List(), List())

  override def toOpenCL(arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): Expression = {
    val idx: ArithExpr = ToOpenCL.exp(index) match {
      case VarRef(name, _, _) => NamedVar(name)
      case _ => throw new Exception("This should not happen")
    }
    val length = DataType.getLengths(dt, tupleAccess, List()).foldLeft(1:ArithExpr)((x,y) => x * y)
    ToOpenCL.exp(array, (idx, length) :: arrayAccess, tupleAccess)
  }

  override def prettyPrint: String = s"(${PrettyPrinter(array)})[${PrettyPrinter(index)}]"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = ???

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = ???
}
