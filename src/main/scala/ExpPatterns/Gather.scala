package ExpPatterns

import Core.OperationalSemantics._
import Core.PhraseType._
import Core.VisitAndRebuild.fun
import Core._
import Compiling.RewriteToImperative
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.Expression
import DSL._

import scala.xml.Elem

case class Gather(n: ArithExpr,
                  dt: DataType,
                  idxF: (ArithExpr, DataType) => ArithExpr,
                  array: Phrase[ExpType])
  extends ExpPattern with ViewExpPattern {

  override lazy val `type` = exp"[$n.$dt]"

  override def typeCheck: Unit = {
    import TypeChecker._
    array checkType exp"[$n.$dt]"
  }

  override def inferTypes(): Gather = {
    import TypeInference._
    val array_ = TypeInference(array)
    array_.t match {
      case ExpType(ArrayType(n_, dt_)) => Gather(n_, dt_, idxF, array_)
      case x => error(x.toString, "ExpType(ArrayType)")
    }
  }

  override def eval(s: Store): Data = {
    import OperationalSemantics._
    OperationalSemantics.eval(s, array) match {
      case ArrayData(a) =>
        val res = Array[Data](a.length)
        for (i <- a.indices) {
          res(i) =  a( idxF(i, array.t.dataType).eval )
        }
        ArrayData(res.toVector)
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(fun: fun): Phrase[ExpType] =
    Gather(fun(n), fun(dt), idxF, VisitAndRebuild(array, fun))

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    exp(this)(λ(array.t) { x =>
      acc(x)(A)
    })
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    import RewriteToImperative._
    exp(array)(λ(array.t) { x =>
      C(Gather(n, dt, idxF, x))
    })
  }

  override def toOpenCL(env: ToOpenCL.Environment,
                        arrayAccess: List[(ArithExpr, ArithExpr)],
                        tupleAccess: List[ArithExpr],
                        dt: DataType): Expression = {

    val idx = arrayAccess.head
    val stack = arrayAccess.tail

    val newIdx = idxF(idx._1, array.t.dataType)

    ToOpenCL.exp(array, env, (newIdx, idx._2) :: stack, tupleAccess, dt)

  }

  override def prettyPrint: String = s"(gather idxF ${PrettyPrinter(array)})"

  override def xmlPrinter: Elem =
    <gather ixdF={ToString(idxF)}>
      {Core.xmlPrinter(array)}
    </gather>
}
