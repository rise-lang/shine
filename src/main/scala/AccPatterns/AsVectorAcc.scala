package AccPatterns

import Core.OperationalSemantics._
import Core._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

import scala.xml.Elem

case class AsVectorAcc(n: ArithExpr,
                       m: ArithExpr,
                       dt: BasicType,
                       array: Phrase[AccType])
  extends AccPattern {

  override lazy val `type` = acc"[${n * m}, dt]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    array checkType acc"[$n.${VectorType(m, dt)}]"
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[AccType] = {
    AsVectorAcc(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def toOpenCL(env: ToOpenCL.Environment): VarRef = ???

  override def toOpenCL(env: ToOpenCL.Environment,
                        arrayAccess: List[(ArithExpr, ArithExpr)],
                        tupleAccess: List[ArithExpr],
                        dt: DataType): VarRef = {
    val top = arrayAccess.head
    val newAAS = ((top._1 /^ n, top._2) :: arrayAccess.tail).map(x => (x._1, x._2 * n))

    ToOpenCL.acc(array, env, newAAS, tupleAccess, dt)
  }

  override def prettyPrint: String = s"(asVectorAcc ${PrettyPrinter(array)})"

  override def xmlPrinter: Elem =
    <asVectorAcc n={ToString(n)}>
      {Core.xmlPrinter(array)}
    </asVectorAcc>
}
