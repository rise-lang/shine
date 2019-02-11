package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Types._
import idealised.DPIA.DSL._
import idealised.DPIA._

import scala.xml.Elem

final case class Generate(n: Nat,
                          dt: DataType,
                          f : Phrase[`(nat)->`[ExpType -> ExpType]])
  extends ExpPrimitive {

  override val `type`: ExpType = {
    val l = f.t.x
    (n: Nat) -> (dt: DataType) ->
      (f :: t"($l : nat) -> exp[${IndexType(n)}] -> exp[$dt]") ->
        exp"[$n.$dt]"
  }

  def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(f)})"

  override def xmlPrinter: Elem = {
    val k = f match {
      case NatDependentLambda(k_, _) => k_
      case _ => throw new Exception("This should not happen")
    }
    <generate n={ToString(n)} dt={ToString(dt)}>
      <f type={ToString(k -> (ExpType(IndexType(k)) -> ExpType(dt)))}>
       {Phrases.xmlPrinter(f)}
      </f>
    </generate>
  }

  def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] =
    Generate(fun(n), fun(dt), VisitAndRebuild(f, fun))

  def eval(s: OperationalSemantics.Store): OperationalSemantics.Data = ???

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommandType] = ???

  def continuationTranslation(C: Phrase[ExpType -> CommandType])
                             (implicit context: TranslationContext): Phrase[CommandType] = {
    C(this)
  }
}
