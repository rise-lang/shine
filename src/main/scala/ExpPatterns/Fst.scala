package ExpPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType.->
import Compiling.RewriteToImperative
import DSL._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.{Expression, Literal}

import scala.xml.Elem

case class Fst(record: Phrase[ExpType])
  extends ExpPattern with ViewExpPattern with GeneratableExpPattern {

  override def typeCheck(): ExpType = {
    TypeChecker(record) match {
      case ExpType(RecordType(fst, snd)) => ExpType(fst)
      case x => TypeChecker.error(x.toString, "Something else")
    }
  }

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, record) match {
      case r: RecordData => r.fst
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[ExpType] = {
    Fst(VisitAndRebuild(record, f))
  }

  override def toOpenCL(ocl: ToOpenCL): Expression = ToOpenCL.exp(this, ocl, List(), List(), t.dataType)

  override def toOpenCL(ocl: ToOpenCL,
                        arrayAccess: List[(ArithExpr, ArithExpr)],
                        tupleAccess: List[ArithExpr],
                        dt: DataType): Expression = {
    ToOpenCL.exp(record, ocl, arrayAccess, 1 :: tupleAccess, dt)
  }

  override def prettyPrint: String = s"${PrettyPrinter(record)}._1"

  override def xmlPrinter: Elem = <fst>{Core.xmlPrinter(record)}</fst>

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] =
    RewriteToImperative.exp(this)(λ(this.t) {
      this.t.dataType match {
        case _: BasicType | _: VectorType => x => A `:=` x
        case _: ArrayType => throw new Exception("This should not happen")
        case _: RecordType => throw new Exception("This should not happen")
      }
    })

  override def rewriteToImperativeExp(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = C(this)
}
