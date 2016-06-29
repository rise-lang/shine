package ExpPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType.->
import apart.arithmetic.{ArithExpr, NamedVar}
import opencl.generator.OpenCLAST.{Expression, VarRef}

import scala.xml.Elem

case class Idx(array: Phrase[ExpType],
               index: Phrase[ExpType]) extends ExpPattern with ViewExpPattern with GeneratableExpPattern {

  private var n: ArithExpr = null
  private var dt: DataType = null

  override def typeCheck(): ExpType = {
    import TypeChecker._
    check(TypeChecker(index), ExpType(int))
    TypeChecker(array) match {
      case ExpType(ArrayType(n_, dt_)) =>
        n = n_; dt = dt_
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
    i.n = f(n)
    i.dt = f(dt)
    i
  }

  override def toOpenCL(env: ToOpenCL.Environment): Expression =
    ToOpenCL.exp(this, env, List(), List(), t.dataType)

  override def toOpenCL(env: ToOpenCL.Environment,
                        arrayAccess: List[(ArithExpr, ArithExpr)],
                        tupleAccess: List[ArithExpr],
                        dt: DataType): Expression = {
    val idx: ArithExpr = ToOpenCL.exp(index, env) match {
      case VarRef(name, _, _) => NamedVar(name, env.ranges(name))
      case _ => throw new Exception("This should not happen")
    }
    val length = DataType.getLengths(dt, tupleAccess, List()).foldLeft(1:ArithExpr)((x,y) => x * y)
    ToOpenCL.exp(array, env, (idx, length) :: arrayAccess, tupleAccess, dt)
  }

  override def prettyPrint: String = s"(${PrettyPrinter(array)})[${PrettyPrinter(index)}]"

  override def xmlPrinter: Elem =
    <idx n={ToString(n)} dt={ToString(dt)}>
      <input type={ToString(ExpType(ArrayType(n, dt)))}>
        {Core.xmlPrinter(array)}
      </input>
      <index type={ToString(ExpType(int))}>
        {Core.xmlPrinter(index)}
      </index>
    </idx>

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = ???

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = ???
}
