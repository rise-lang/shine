package AccPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType._
import apart.arithmetic.{ArithExpr, Cst, NamedVar}
import opencl.generator.OpenCLAST.{Literal, VarRef}

import scala.xml.Elem

case class IdxAcc(n: ArithExpr,
                  dt: DataType,
                  index: Phrase[ExpType],
                  array: Phrase[AccType]) extends AccPattern {

  override def typeCheck(): AccType = {
    import TypeChecker._
    check(TypeChecker(index), ExpType(int))
    TypeChecker(array) match {
      case AccType(ArrayType(n_, dt_)) =>
        if (dt == dt_ && n == n_) {
          AccType(dt)
        } else {
          error(acc"[$n_.$dt]", acc"[$n.$dt]")
        }
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

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[AccType] = {
    IdxAcc(fun(n), fun(dt), VisitAndRebuild(index, fun), VisitAndRebuild(array, fun))
  }

  override def toOpenCL(env: ToOpenCL.Environment): VarRef = ToOpenCL.acc(this, env, List(), List(), t.dataType)

  override def toOpenCL(env: ToOpenCL.Environment,
                        arrayAccess: List[(ArithExpr, ArithExpr)],
                        tupleAccess: List[ArithExpr],
                        dt: DataType): VarRef = {
    val idx: ArithExpr = ToOpenCL.exp(index, env) match {
      case VarRef(name, _, _) => NamedVar(name, env.ranges(name))
      case Literal(i) => Cst(i.toInt)
      case _ => throw new Exception("This should not happen")
    }
    val length = DataType.getLengths(dt, tupleAccess, List()).foldLeft(1: ArithExpr)((x,y) => x * y)
    ToOpenCL.acc(array, env, (idx, length) :: arrayAccess, tupleAccess, dt)
  }

  override def prettyPrint: String = s"${PrettyPrinter(array)}[${PrettyPrinter(index)}]"

  override def xmlPrinter: Elem =
    <idxAcc n={ToString(n)} dt={ToString(dt)}>
      <output>{Core.xmlPrinter(array)}</output>
      <index>{Core.xmlPrinter(index)}</index>
    </idxAcc>
}
