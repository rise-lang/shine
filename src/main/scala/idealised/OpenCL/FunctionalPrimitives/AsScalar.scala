package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Compilation.RewriteToImperative
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.CodeGeneration.OpenCLOldCodeGenerator
import idealised.OpenCL.ImperativePrimitives.AsScalarAcc
import idealised.OpenCL.ViewExp
import ir.Type
import lift.arithmetic.Cst
import opencl.generator.OpenCLAST.Expression

import scala.xml.Elem

final case class AsScalar(n: Nat,
                          m: Nat,
                          dt: ScalarType,
                          array: Phrase[ExpType])
  extends ExpPrimitive with ViewExp {

  override lazy val `type`: ExpType =
    (n: Nat) -> (m: Nat) -> (dt: ScalarType) ->
      (array :: exp"[$n.${VectorType(m, dt)}]") ->
        exp"[${n * m}.$dt]"


  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    AsScalar(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = ???

  override def toOpenCL(env: OpenCLOldCodeGenerator.Environment,
                        arrayAccess: List[Nat],
                        tupleAccess: List[Nat],
                        dt: DataType): Expression = {
    val i :: is = arrayAccess
    println(s"asSclar: ${(i /^ n)}")
    OpenCLOldCodeGenerator.exp(array, env, dt, (i /^ n) :: is, tupleAccess)
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
//    val newAs = (chunkId, l * n) :: (chunkElemId, l) :: stack
  }

  override def prettyPrint: String = s"(asScalar ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <asScalar n={ToString(n)}>
      {Phrases.xmlPrinter(array)}
    </asScalar>

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    acc(array)(AsScalarAcc(n, m, dt, A))
  }

  override def continuationTranslation(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    import RewriteToImperative._
    con(array)(λ(array.t)(x => C(AsScalar(n, m, dt, x)) ))
  }
}