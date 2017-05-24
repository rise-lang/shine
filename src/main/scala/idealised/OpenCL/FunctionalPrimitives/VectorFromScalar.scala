package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Compilation.RewriteToImperative
import idealised.DPIA.Phrases.{VisitAndRebuild, _}
import idealised.DPIA.Semantics.OperationalSemantics.{Data, Store}
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.DPIA.DSL._
import idealised.OpenCL.{CodeGenerator, GeneratableExp}
import idealised.OpenCL.CodeGenerator.Environment
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

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    VectorFromScalar(f(n), f(dt), VisitAndRebuild(arg, f))
  }

  override def codeGenExp(env: Environment): Expression = {
    VectorLiteral(
      DataType.toVectorType(VectorType(n, dt)),
      CodeGenerator.exp(arg, env))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String = s"(makeVector ${n.toString} ${PrettyPhrasePrinter(arg)})"

  override def xmlPrinter: Elem =
    <makeVector n={ToString(n)}>
      {Phrases.xmlPrinter(arg)}
    </makeVector>

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    con(arg)(λ(exp"[$dt]")(e => A := VectorFromScalar(n, dt, e) ))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    import RewriteToImperative._
    con(arg)(λ(exp"[$dt]")(e => C(VectorFromScalar(n, dt, e)) ))
  }
}
