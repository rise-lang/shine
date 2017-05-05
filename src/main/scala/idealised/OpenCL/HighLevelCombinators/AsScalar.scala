package idealised.OpenCL.HighLevelCombinators

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._
import idealised.Compiling.RewriteToImperative
import idealised.DSL.typed._
import idealised.OpenCL.LowLevelCombinators.AsScalarAcc
import opencl.generator.OpenCLAST.Expression
import idealised.OpenCL.Core.{ToOpenCL, ViewExp}
import ir.Type

import scala.xml.Elem

final case class AsScalar(n: Nat,
                          m: Nat,
                          dt: ScalarType,
                          array: Phrase[ExpType])
  extends HighLevelCombinator with ViewExp {

  override lazy val `type` = exp"[${n * m}.$dt]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (m: Nat) -> (dt: ScalarType) ->
      (array :: exp"[$n.${VectorType(m, dt)}]") -> `type`
  }

  override def inferTypes: AsScalar = {
    import TypeInference._
    val array_ = TypeInference(array)
    array_.t match {
      case ExpType(ArrayType(n_, VectorType(m_, dt_))) =>
        AsScalar(n_, m_, dt_, array_)
      case x => error(x.toString, "ExpType(ArrayType(VectorType))")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    AsScalar(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = ???

  override def toOpenCL(env: ToOpenCL.Environment,
                        arrayAccess: List[(Nat, Nat)],
                        tupleAccess: List[Nat],
                        dt: DataType): Expression = {
    // Similar to Join
    val idx = arrayAccess.head
    val stack = arrayAccess.tail

    val chunkId = idx._1 / n
    // we want to access element 0 ...
    val chunkElemId: Nat = 0 //idx._1 % n
    // ... and there is 1 of it.
    val l = Type.getLengths(DataType.toType(t.dataType)).reduce(_ * _)
    assert(l == (1: Nat))

    val newAs = (chunkId, l * n) :: (chunkElemId, l) :: stack

    ToOpenCL.exp(array, env, dt, newAs, tupleAccess)

    //    val top = arrayAccess.head
    //    val newAAS = ((top._1 /^ n, top._2) :: arrayAccess.tail).map(x => (x._1, x._2 * n))
    //
    //    ToOpenCL.exp(array, env, dt, newAAS, tupleAccess)
  }

  override def prettyPrint: String = s"(asScalar ${PrettyPrinter(array)})"

  override def xmlPrinter: Elem =
    <asScalar n={ToString(n)}>
      {Core.xmlPrinter(array)}
    </asScalar>

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    acc(array)(AsScalarAcc(n, m, dt, A))
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    import RewriteToImperative._
    exp(array)(λ(array.t) { x =>
      C(AsScalar(n, m, dt, x))
    })
  }
}