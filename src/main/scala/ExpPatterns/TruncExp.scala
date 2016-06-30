package ExpPatterns

import Core._
import Core.OperationalSemantics._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.Expression

import scala.xml.Elem

case class TruncExp(n: ArithExpr,
                    m: ArithExpr,
                    dt: DataType,
                    array: Phrase[ExpType])
  extends ExpPattern with ViewExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    array.t =?= exp"[$n.$dt]"
    exp"[$m.$dt]"
  }

  override def inferTypes(): TruncExp = {
    import TypeInference._
    val array_ = TypeInference(array)
    array_.t match {
      case ExpType(ArrayType(n_, dt_)) =>
        TruncExp(n_, m, dt_, array_)
      case x => error(x.toString, "ArrayType")
    }
  }

  override def eval(s: Store): Data = ???

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    TruncExp(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = ???

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = ???

  override def xmlPrinter: Elem =
    <truncExp n={n.toString} m={m.toString} dt={dt.toString}>
      {Core.xmlPrinter(array)}
    </truncExp>

  override def prettyPrint: String = s"(truncExp $array)"

  override def toOpenCL(env: ToOpenCL.Environment,
                        arrayAccess: List[(ArithExpr, ArithExpr)],
                        tupleAccess: List[ArithExpr],
                        dt: DataType): Expression = {
    ToOpenCL.exp(array, env, arrayAccess, tupleAccess, dt)
  }

}
