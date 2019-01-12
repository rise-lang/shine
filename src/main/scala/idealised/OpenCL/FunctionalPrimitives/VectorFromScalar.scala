package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Compilation.RewriteToImperative
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases.{VisitAndRebuild, _}
import idealised.DPIA.Semantics.OperationalSemantics.{Data, Store}
import idealised.DPIA.Types._
import idealised.DPIA._
import opencl.generator.OpenCLAST.{Expression, VectorLiteral}

import scala.xml.Elem

final case class VectorFromScalar(n: Nat,
                                  dt: ScalarType,
                                  arg: Phrase[ExpType])
  extends ExpPrimitive {

  override lazy val `type`: ExpType =
    (n: Nat) -> (dt: ScalarType) ->
      (arg :: exp"[$dt]") ->
        exp"[${VectorType(n, dt)}]"

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    VectorFromScalar(f(n), f(dt), VisitAndRebuild(arg, f))
  }

//  override def codeGenExp(env: Environment): Expression = {
//    VectorLiteral(
//      DataType.toVectorType(VectorType(n, dt)),
//      OpenCLOldCodeGenerator.exp(arg, env))
//  }

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
