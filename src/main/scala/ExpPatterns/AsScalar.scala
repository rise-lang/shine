package ExpPatterns

import AccPatterns.AsScalarAcc
import Core.OperationalSemantics._
import Core.PhraseType.->
import Core._
import Compiling.RewriteToImperative
import DSL._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.Expression

import scala.xml.Elem

case class AsScalar(array: Phrase[ExpType])
  extends ExpPattern with ViewExpPattern {

  private var n: ArithExpr = null

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(array) match {
      case ExpType(ArrayType(n_, VectorType(m, dt))) =>
        n = n_
        ExpType(ArrayType(n*m, dt))
      case x => error(x.toString, "ArrayType(VectorType)")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[ExpType] = {
    val as = AsScalar(VisitAndRebuild(array, f))
    as.n = n
    as
  }

  override def eval(s: Store): Data = ???

  override def toOpenCL(ocl: ToOpenCL,
                        arrayAccess: List[(ArithExpr, ArithExpr)],
                        tupleAccess: List[ArithExpr],
                        dt: DataType): Expression = {
    val top = arrayAccess.head
    val newAAS = ((top._1 /^ n, top._2) :: arrayAccess.tail).map(x => (x._1, x._2 * n))

    ToOpenCL.exp(array, ocl, newAAS, tupleAccess, dt)
  }

  override def prettyPrint: String = s"(asScalar ${PrettyPrinter(array)})"

  override def xmlPrinter: Elem =
    <asScalar n={ToString(n)}>
      {Core.xmlPrinter(array)}
    </asScalar>

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    assert(n != null)
    import RewriteToImperative._
    acc(array)(AsScalarAcc(n, A))
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    import RewriteToImperative._
    exp(array)(λ(array.t) { x =>
      C(AsScalar(x))
    })
  }
}