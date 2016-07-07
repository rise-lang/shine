package LowLevelCombinators

import Compiling.RewriteToImperative
import Core.OperationalSemantics._
import Core._
import DSL.typed._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.Expression

import scala.xml.Elem

case class Fst(dt1: DataType,
               dt2: DataType,
               record: Phrase[ExpType])
  extends LowLevelExpCombinator with ViewExp with GeneratableExp {

  override lazy val `type` = exp"[$dt1]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    record checkType exp"[$dt1 x $dt2]"
  }

  override def inferTypes: Fst = {
    import TypeInference._
    val record_ = TypeInference(record)
    record_.t match {
      case ExpType(RecordType(dt1_, dt2_)) => Fst(dt1_, dt2_, record_)
      case x => error(x.toString, "ExpType(RecordType)")
    }
  }

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, record) match {
      case r: RecordData => r.fst
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    Fst(fun(dt1), fun(dt2), VisitAndRebuild(record, fun))
  }

  override def toOpenCL(env: ToOpenCL.Environment): Expression = ToOpenCL.exp(this, env, List(), List(), t.dataType)

  override def toOpenCL(env: ToOpenCL.Environment,
                        arrayAccess: List[(ArithExpr, ArithExpr)],
                        tupleAccess: List[ArithExpr],
                        dt: DataType): Expression = {
    ToOpenCL.exp(record, env, arrayAccess, 1 :: tupleAccess, dt)
  }

  override def prettyPrint: String = s"${PrettyPrinter(record)}._1"

  override def xmlPrinter: Elem = <fst>
    {Core.xmlPrinter(record)}
  </fst>

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
