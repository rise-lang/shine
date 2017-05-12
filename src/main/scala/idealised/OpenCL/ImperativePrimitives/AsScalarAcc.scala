package idealised.OpenCL.ImperativePrimitives

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._
import opencl.generator.OpenCLAST.Expression
import idealised.OpenCL.Core.{ToOpenCL, ViewAcc}

import scala.xml.Elem

final case class AsScalarAcc(n: Nat,
                             m: Nat,
                             dt: ScalarType,
                             array: Phrase[AccType])
  extends AccPrimitive with ViewAcc {

  override lazy val `type` = acc"[$n.${VectorType(m, dt)}]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (m: Nat) -> (dt: ScalarType) ->
      (array :: acc"[${m * n}.$dt]") -> `type`
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    AsScalarAcc(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def toOpenCL(env: ToOpenCL.Environment,
                        value: Expression,
                        dt: DataType,
                        arrayAccess: List[(Nat, Nat)],
                        tupleAccess: List[Nat]): Expression = {
    // similar to Split
    val chunkId = arrayAccess.head
    val chunkElemId: (Nat, Nat) = (0, 1) // we want to access element 0 and there is only one of it
    val rest = arrayAccess.tail

    val newIdx = chunkId._1 * m + chunkElemId._1

    ToOpenCL.acc(array, value, env, dt, (newIdx, chunkElemId._2) :: rest, tupleAccess)

//    val newAAS = arrayAccess.map(x => (x._1, x._2 /^ m))
//
//    ToOpenCL.acc(array, value, env, dt, newAAS, tupleAccess)
  }

  override def prettyPrint = s"(asScalarAcc $n ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <asScalarAcc n={ToString(n)} m={ToString(m)}>
      {Core.xmlPrinter(array)}
    </asScalarAcc>
}
