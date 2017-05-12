package idealised.OpenCL.FunctionalPrimitives

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._
import idealised.Compiling.RewriteToImperative
import idealised.DSL.typed._
import idealised.OpenCL.ImperativePrimitives.AsVectorAcc

import opencl.generator.OpenCLAST.Expression
import idealised.OpenCL.Core.{ToOpenCL, ViewExp}

import scala.xml.Elem

final case class AsVector(n: Nat,
                          m: Nat,
                          dt: ScalarType,
                          array: Phrase[ExpType])
  extends ExpPrimitive with ViewExp {

  override lazy val `type` = exp"[$m.${VectorType(n, dt)}]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (m: Nat) -> (dt: ScalarType) ->
      (array :: exp"[${m * n}.$dt]") -> `type`
  }

  override def inferTypes: AsVector = {
    import TypeInference._
    val array_ = TypeInference(array)
    array_.t match {
      case ExpType(ArrayType(mn_, dt_)) if dt_.isInstanceOf[ScalarType] =>
        AsVector(n, mn_ /^ n, dt_.asInstanceOf[ScalarType], array_)
      case x => error(x.toString, "ExpType(ArrayType)")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    AsVector(f(n), f(m), f(dt), VisitAndRebuild(array, f))
  }

  override def eval(s: Store): Data = ???

  override def toOpenCL(env: ToOpenCL.Environment,
                        arrayAccess: List[(Nat, Nat)],
                        tupleAccess: List[Nat],
                        dt: DataType): Expression = {
    // similar to Split
    val chunkId = arrayAccess.head
    // we want to access element 0 and there is only one of it
    val chunkElemId: (Nat, Nat) = (0, 1)
    val rest = arrayAccess.tail

    val newIdx = chunkId._1 * n + chunkElemId._1

    ToOpenCL.exp(array, env, dt, (newIdx, chunkElemId._2) :: rest, tupleAccess)
  }

  override def prettyPrint: String = s"(asVector ${n.toString} ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <asVector n={ToString(n)}>
      {Core.xmlPrinter(array)}
    </asVector>

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = {
    RewriteToImperative.acc(array)(AsVectorAcc(n, m, dt, A))
  }

  override def continuationTranslation(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    RewriteToImperative.con(array)(λ(array.t)(x => C(AsVector(n, m, dt, x)) ))
  }
}