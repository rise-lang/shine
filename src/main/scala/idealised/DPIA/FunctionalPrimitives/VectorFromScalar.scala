package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.language.reflectiveCalls

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

  override def eval(s: Store): Data = ???

  override def prettyPrint: String = s"(makeVector ${n.toString} ${PrettyPhrasePrinter(arg)})"

  override def xmlPrinter: Elem =
    <makeVector n={ToString(n)}>
      {Phrases.xmlPrinter(arg)}
    </makeVector>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._
    con(arg)(λ(exp"[$dt]")(e => A :=|dt| VectorFromScalar(n, dt, e) ))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._
    con(arg)(λ(exp"[$dt]")(e => C(VectorFromScalar(n, dt, e)) ))
  }
}
