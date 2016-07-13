package OpenCL.LowLevelCombinators

import Core.OperationalSemantics._
import Core._

import opencl.generator.OpenCLAST.VarRef
import OpenCL.Core.{ToOpenCL, ViewAcc}

import scala.xml.Elem

final case class AsScalarAcc(n: Nat,
                             m: Nat,
                             dt: BasicType,
                             array: Phrase[AccType])
  extends LowLevelAccCombinator with ViewAcc {

  override lazy val `type` = acc"[$n.${VectorType(m, dt)}]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (m: Nat) -> (dt: BasicType) ->
      (array `:`  acc"[${m * n}.$dt]") -> `type`
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[AccType] = {
    AsScalarAcc(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def toOpenCL(env: ToOpenCL.Environment,
                        arrayAccess: List[(Nat, Nat)],
                        tupleAccess: List[Nat],
                        dt: DataType): VarRef = {

    val top = arrayAccess.head
    val newAAS = ((top._1 * n, top._2) :: arrayAccess.tail).map(x => (x._1, x._2 /^ n))

    ToOpenCL.acc(array, env, newAAS, tupleAccess, dt)
  }

  override def prettyPrint = s"(asScalarAcc $n ${PrettyPrinter(array)})"

  override def xmlPrinter: Elem =
    <asScalarAcc n={ToString(n)}>
      {Core.xmlPrinter(array)}
    </asScalarAcc>
}
