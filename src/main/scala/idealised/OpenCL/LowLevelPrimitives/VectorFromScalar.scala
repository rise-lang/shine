package idealised.OpenCL.LowLevelPrimitives

import idealised.Compiling.RewriteToImperative
import idealised.Core
import idealised.Core.OperationalSemantics.{Data, Store}
import idealised.Core.VisitAndRebuild.Visitor
import idealised.Core.{ExpType, Phrase, ScalarType, _}
import idealised.DSL.typed._
import idealised.OpenCL.Core.ToOpenCL.Environment
import idealised.OpenCL.Core.{GeneratableExp, ToOpenCL}
import opencl.generator.OpenCLAST.{Expression, VectorLiteral}

import scala.xml.Elem

final case class VectorFromScalar(n: Nat,
                                  dt: ScalarType,
                                  arg: Phrase[ExpType])
  extends ExpPrimitive with GeneratableExp {

  override lazy val `type` = exp"[${VectorType(n, dt)}]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (dt: ScalarType) ->
      (arg :: exp"[$dt]") -> `type`
  }

  override def inferTypes: VectorFromScalar = {
    import TypeInference._
    val arg_ = TypeInference(arg)
    arg_.t match {
      case ExpType(dt_) if dt_.isInstanceOf[ScalarType] =>
        VectorFromScalar(n, dt_.asInstanceOf[ScalarType], arg_)
      case x => error(x.toString, "ExpType(ScalarType)")
    }
  }

  override def visitAndRebuild(f: Visitor): Phrase[ExpType] = {
    VectorFromScalar(f(n), f(dt), VisitAndRebuild(arg, f))
  }

  override def toOpenCL(env: Environment): Expression = {
    VectorLiteral(
      DataType.toVectorType(VectorType(n, dt)),
      ToOpenCL.exp(arg, env))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String = s"(makeVector ${n.toString} ${PrettyPhrasePrinter(arg)})"

  override def xmlPrinter: Elem =
    <makeVector n={ToString(n)}>
      {Core.xmlPrinter(arg)}
    </makeVector>

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    exp(arg)(λ(exp"[$dt]")(e =>
      A `:=` VectorFromScalar(n, dt, e)
    ))
  }

  override def rewriteToImperativeExp(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    RewriteToImperative.exp(arg)(λ(exp"[$dt]")(e =>
      C(VectorFromScalar(n, dt, e))
    ))
  }
}
