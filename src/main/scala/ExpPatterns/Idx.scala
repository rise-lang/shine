package ExpPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType._
import apart.arithmetic.{ArithExpr, NamedVar}
import opencl.generator.OpenCLAST.{Expression, VarRef}

import scala.xml.Elem

case class Idx(n: ArithExpr,
               dt: DataType,
               index: Phrase[ExpType],
               array: Phrase[ExpType])
  extends ExpPattern with ViewExpPattern with GeneratableExpPattern {

  override lazy val `type` = exp"[$dt]"

  override def typeCheck: Unit = {
    import TypeChecker._
    index checkType exp"[$int]"
    array checkType exp"[$n.$dt]"
  }

  override def inferTypes: Idx = {
    import TypeInference._
    val index_ = TypeInference(index)
    val array_ = TypeInference(array)
    array_.t match {
      case ExpType(ArrayType(n_, dt_)) => Idx(n_, dt_, index_, array_)
      case x => error(x.toString, "ExpType(ArrayType)")
    }
  }

  override def eval(s: Store): Data = {
    (OperationalSemantics.eval(s, array), OperationalSemantics.eval(s, index)) match {
      case (ArrayData(xs), IntData(i)) => xs(i)
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    Idx(fun(n), fun(dt), VisitAndRebuild(index, fun), VisitAndRebuild(array, fun))
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
    val length = DataType.getLengths(dt, tupleAccess, List()).foldLeft(1: ArithExpr)((x, y) => x * y)
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
