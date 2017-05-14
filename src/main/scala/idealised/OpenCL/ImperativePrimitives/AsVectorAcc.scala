package idealised.OpenCL.ImperativePrimitives

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._
import idealised.OpenCL.CodeGenerator
import idealised.OpenCL.Core.ViewAcc
import opencl.generator.OpenCLAST.Expression
import ir.Type

import scala.xml.Elem

final case class AsVectorAcc(n: Nat,
                             m: Nat,
                             dt: ScalarType,
                             array: Phrase[AccType])
  extends AccPrimitive with ViewAcc {

  override lazy val `type` = acc"[${n * m}.$dt]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (m: Nat) -> (dt: ScalarType) ->
      (array :: acc"[$n.${VectorType(m, dt)}]") -> `type`
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    AsVectorAcc(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def toOpenCL(env: CodeGenerator.Environment,
                        value: Expression,
                        dt: DataType,
                        arrayAccess: List[(Nat, Nat)],
                        tupleAccess: List[Nat]): Expression = {
    // Similar to Join
    val idx = arrayAccess.head
    val stack = arrayAccess.tail

    val chunkId = idx._1 / n
    // we want to access element 0 ...
    val chunkElemId: Nat = 0 //idx._1 % n
    // ... and there is 1 of it.
    val l = Type.getLengths(DataType.toType(t.dataType)).reduce(_ * _)
    assert(l == (1: Nat))

    val newAs = (chunkId, l * n) ::(chunkElemId, l) :: stack

    CodeGenerator.acc(array, value, env, dt, newAs, tupleAccess)

//    val top = arrayAccess.head
//    val newAAS = ((top._1 /^ n, top._2) :: arrayAccess.tail).map(x => (x._1, x._2 * n))
//
//    ToOpenCL.acc(array, value, env, dt, newAAS, tupleAccess)
  }

  override def prettyPrint: String = s"(asVectorAcc ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <asVectorAcc n={ToString(n)}>
      {Core.xmlPrinter(array)}
    </asVectorAcc>
}
