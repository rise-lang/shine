package ExpPatterns

import AccPatterns.AsVectorAcc
import Compiling.RewriteToImperative
import Core.OperationalSemantics._
import Core._
import DSL._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.Expression

import scala.xml.Elem

case class AsVector(n: ArithExpr,
                    m: ArithExpr,
                    dt: BasicType,
                    array: Phrase[ExpType])
  extends ExpPattern with ViewExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    array.t =?= exp"[$m.$dt]"
    exp"[${m /^ n}.${VectorType(n, dt)}]"
  }

  override def inferTypes(): AsVector = {
    import TypeInference._
    val array_ = TypeInference(array)
    array_.t match {
      case ExpType(ArrayType(mn_, dt_)) if dt.isInstanceOf[BasicType] =>
        AsVector(n, mn_ /^ n, dt.asInstanceOf[BasicType], array_)
      case x => error(x.toString, "ExpType(ArrayType)")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[ExpType] = {
    AsVector(f(n), f(m), f(dt), VisitAndRebuild(array, f))
  }

  override def eval(s: Store): Data = ???

  override def toOpenCL(env: ToOpenCL.Environment,
                        arrayAccess: List[(ArithExpr, ArithExpr)],
                        tupleAccess: List[ArithExpr],
                        dt: DataType): Expression = {

    val top = arrayAccess.head
    val newAAS = ((top._1 * n, top._2) :: arrayAccess.tail).map(x => (x._1, x._2 /^ n))

    ToOpenCL.exp(array, env, newAAS, tupleAccess, dt)
  }

  override def prettyPrint: String = s"(asVector ${n.toString} ${PrettyPrinter(array)})"

  override def xmlPrinter: Elem =
    <asVector n={ToString(n)}>
      {Core.xmlPrinter(array)}
    </asVector>

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    RewriteToImperative.acc(array)(AsVectorAcc(n, m, dt, A))
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    RewriteToImperative.exp(array)(λ(array.t) { x =>
      C(AsVector(n, m, dt, x))
    })
  }
}