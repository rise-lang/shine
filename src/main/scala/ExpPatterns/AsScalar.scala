package ExpPatterns

import AccPatterns.AsScalarAcc
import Core.OperationalSemantics._
import Core._
import Compiling.RewriteToImperative
import DSL._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.Expression

import scala.xml.Elem

case class AsScalar(n: ArithExpr,
                    m: ArithExpr,
                    dt: BasicType,
                    array: Phrase[ExpType])
  extends ExpPattern with ViewExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    array.t =?= exp"[$n.${VectorType(m, dt)}]"
    exp"[${n * m}.$dt]"
  }

  override def inferTypes(): AsScalar = {
    import TypeInference._
    val array_ = TypeInference(array)
    array_.t match {
      case ExpType(ArrayType(n_, VectorType(m_, dt_))) =>
        AsScalar(n_, m_, dt_, array_)
      case x => error(x.toString, "ExpType(ArrayType(VectorType))")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    AsScalar(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = ???

  override def toOpenCL(env: ToOpenCL.Environment,
                        arrayAccess: List[(ArithExpr, ArithExpr)],
                        tupleAccess: List[ArithExpr],
                        dt: DataType): Expression = {
    val top = arrayAccess.head
    val newAAS = ((top._1 /^ n, top._2) :: arrayAccess.tail).map(x => (x._1, x._2 * n))

    ToOpenCL.exp(array, env, newAAS, tupleAccess, dt)
  }

  override def prettyPrint: String = s"(asScalar ${PrettyPrinter(array)})"

  override def xmlPrinter: Elem =
    <asScalar n={ToString(n)}>
      {Core.xmlPrinter(array)}
    </asScalar>

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    acc(array)(AsScalarAcc(n, m, dt, A))
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    import RewriteToImperative._
    exp(array)(λ(array.t) { x =>
      C(AsScalar(n, m, dt, x))
    })
  }
}