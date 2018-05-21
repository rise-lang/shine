package idealised.OpenCL.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.CodeGeneration.OpenCLOldCodeGenerator
import idealised.OpenCL.ViewAcc
import opencl.generator.OpenCLAST.Expression

import scala.xml.Elem

final case class AsScalarAcc(n: Nat,
                             m: Nat,
                             dt: ScalarType,
                             array: Phrase[AccType])
  extends AccPrimitive with ViewAcc {

  override lazy val `type`: AccType =
    (n: Nat) -> (m: Nat) -> (dt: ScalarType) ->
      (array :: acc"[${m * n}.$dt]") ->
        acc"[$n.${VectorType(m, dt)}]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    AsScalarAcc(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def toOpenCL(env: OpenCLOldCodeGenerator.Environment,
                        value: Expression,
                        dt: DataType,
                        arrayAccess: List[Nat],
                        tupleAccess: List[Nat]): Expression = {
    val i :: is = arrayAccess

    OpenCLOldCodeGenerator.acc(array, value, env, dt, (i * m + 0) :: is, tupleAccess)
//    // similar to Split
//    val chunkId = arrayAccess.head
//    val chunkElemId: (Nat, Nat) = (0, 1) // we want to access element 0 and there is only one of it
//    val rest = arrayAccess.tail
//
//    val newIdx = chunkId._1 * m + chunkElemId._1
//
//    CodeGenerator.acc(array, value, env, dt, (newIdx, chunkElemId._2) :: rest, tupleAccess)
  }

  override def prettyPrint = s"(asScalarAcc $n ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <asScalarAcc n={ToString(n)} m={ToString(m)}>
      {Phrases.xmlPrinter(array)}
    </asScalarAcc>
}
