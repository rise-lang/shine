package OpenCL.HighLevelCombinators

import Compiling.RewriteToImperative
import Core.OperationalSemantics._
import Core._
import DSL.typed._
import OpenCL.LowLevelCombinators.AsVectorAcc

import opencl.generator.OpenCLAST.Expression
import OpenCL.Core.{ToOpenCL, ViewExp}

import scala.xml.Elem

final case class AsVector(n: Nat,
                          m: Nat,
                          dt: BasicType,
                          array: Phrase[ExpType])
  extends HighLevelCombinator with ViewExp {

  override lazy val `type` = exp"[$m.${VectorType(n, dt)}]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (m: Nat) -> (dt: DataType) ->
      (array `:` exp"[${m * n}.$dt]") ->
      `type`
  }

  override def inferTypes: AsVector = {
    import TypeInference._
    val array_ = TypeInference(array)
    array_.t match {
      case ExpType(ArrayType(mn_, dt_)) if dt_.isInstanceOf[BasicType] =>
        AsVector(n, mn_ /^ n, dt_.asInstanceOf[BasicType], array_)
      case x => error(x.toString, "ExpType(ArrayType)")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[ExpType] = {
    AsVector(f(n), f(m), f(dt), VisitAndRebuild(array, f))
  }

  override def eval(s: Store): Data = ???

  override def toOpenCL(env: ToOpenCL.Environment,
                        arrayAccess: List[(Nat, Nat)],
                        tupleAccess: List[Nat],
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