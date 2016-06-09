package AccPatterns

import Core._
import Core.OperationalSemantics._
import apart.arithmetic.{ArithExpr, NamedVar}
import opencl.generator.OpenCLAST.VarRef

case class IdxAcc(array: Phrase[AccType], index: Phrase[ExpType]) extends AccPattern {

  private var dt: DataType = null

  override def typeCheck(): AccType = {
    import TypeChecker._
    check(TypeChecker(index), ExpType(int))
    TypeChecker(array) match {
      case AccType(ArrayType(n, dt_)) =>
        dt = dt_
        AccType(dt)
      case x => error(x.toString, "ArrayType")
    }
  }

  override def eval(s: Store): AccIdentifier = {
    val arrayE = OperationalSemantics.eval(s, array)
    val indexE = OperationalSemantics.eval(s, index) match {
      case IntData(i) => i
      case _ => throw new Exception("This should not happen")
    }
    ArrayAccessIdentifier(arrayE, indexE)
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): AccPattern = {
    val i = IdxAcc(
      OperationalSemantics.substitute(phrase, `for`, array),
      OperationalSemantics.substitute(phrase, `for`, index))
    i.dt = dt
    i
  }

  override def toC = Printer.toC(array) + "[" + Printer.toC(index) + "]"

  override def toOpenCL: VarRef = ToOpenCL.acc(this, List(), List())

  override def toOpenCL(arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): VarRef = {
    val idx: ArithExpr = ToOpenCL.exp(index) match {
      case VarRef(name, _, _) => NamedVar(name)
      case _ => throw new Exception("This should not happen")
    }
    val length = DataType.getLengths(dt, tupleAccess, List()).foldLeft(1: ArithExpr)((x,y) => x * y)
    ToOpenCL.acc(array, (idx, length) :: arrayAccess, tupleAccess)
  }

  override def prettyPrint: String = s"${PrettyPrinter(array)}[${PrettyPrinter(index)}]"

}
