package idealised.OpenCL.LowLevelCombinators

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._

import opencl.generator.OpenCLAST.{Expression, VarRef}
import idealised.OpenCL.Core.{ToOpenCL, ViewAcc}

import scala.xml.Elem

final case class AsVectorAcc(n: Nat,
                             m: Nat,
                             dt: ScalarType,
                             array: Phrase[AccType])
  extends LowLevelAccCombinator with ViewAcc {

  override lazy val `type` = acc"[${n * m}, dt]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (m: Nat) -> (dt: ScalarType) ->
      (array :: acc"[$n.${VectorType(m, dt)}]") -> `type`
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    AsVectorAcc(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def toOpenCL(env: ToOpenCL.Environment,
                        value: Expression,
                        dt: DataType): ((List[(Nat, Nat)], List[Nat]) => Expression, List[(Nat, Nat)], List[Nat]) = {
    val (exp, arrayAccess, tupleAccess) = ToOpenCL.acc(array, value, env, dt)

    val top = arrayAccess.head
    val newAAS = ((top._1 /^ n, top._2) :: arrayAccess.tail).map(x => (x._1, x._2 * n))

    (exp, newAAS, tupleAccess)
  }

  override def prettyPrint: String = s"(asVectorAcc ${PrettyPrinter(array)})"

  override def xmlPrinter: Elem =
    <asVectorAcc n={ToString(n)}>
      {Core.xmlPrinter(array)}
    </asVectorAcc>
}
