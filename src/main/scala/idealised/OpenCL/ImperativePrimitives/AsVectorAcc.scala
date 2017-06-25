package idealised.OpenCL.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.CodeGeneration.CodeGenerator
import idealised.OpenCL.ViewAcc
import ir.Type
import lift.arithmetic.Cst
import opencl.generator.OpenCLAST.Expression

import scala.xml.Elem

final case class AsVectorAcc(n: Nat,
                             m: Nat,
                             dt: ScalarType,
                             array: Phrase[AccType])
  extends AccPrimitive with ViewAcc {

  override lazy val `type`: AccType =
    (n: Nat) -> (m: Nat) -> (dt: ScalarType) ->
      (array :: acc"[$n.${VectorType(m, dt)}]") ->
        acc"[${n * m}.$dt]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    AsVectorAcc(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def toOpenCL(env: CodeGenerator.Environment,
                        value: Expression,
                        dt: DataType,
                        arrayAccess: List[Nat],
                        tupleAccess: List[Nat]): Expression = {
    val i :: is = arrayAccess
    CodeGenerator.acc(array, value, env, dt, (i / n) :: is, tupleAccess)
//    // Similar to Join
//    val idx = arrayAccess.head
//    val stack = arrayAccess.tail
//
//    val chunkId = idx._1 / n
//    // we want to access element 0 ...
//    val chunkElemId: Nat = 0 //idx._1 % n
//    // ... and there is 1 of it.
//    val l = Type.getLengths(DataType.toType(t.dataType)).reduce(_ * _)
//    assert(l == (1: Nat))
//
//    val newAs = (chunkId, l * n) ::(chunkElemId, l) :: stack
//
//    CodeGenerator.acc(array, value, env, dt, newAs, tupleAccess)
  }

  override def prettyPrint: String = s"(asVectorAcc ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <asVectorAcc n={ToString(n)}>
      {Phrases.xmlPrinter(array)}
    </asVectorAcc>
}
