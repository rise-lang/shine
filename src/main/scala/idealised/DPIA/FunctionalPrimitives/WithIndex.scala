package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.TranslationToImperative.con
import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

case class WithIndex(n:Nat, dt:DataType, array:Phrase[ExpType]) extends ExpPrimitive {


  override val `type`: ExpType = {
    (n: Nat) -> (dt: DataType) ->
      (array :: exp"[$n.$dt]") -> exp"[${DepArrayType(n, _ => dt)}]"
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    WithIndex(f(n), f(dt), VisitAndRebuild(array, f))
  }

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommandType] = {
    con(this)(λ(exp"[${DepArrayType(n, _ => dt)}]")(x => A :=|dt"[${DepArrayType(n, _ => dt)}]"| x ))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType])(implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._
    
    con(array)(λ(exp"[$n.$dt]")(x => C(WithIndex(n, dt, x))))
  }

  override def xmlPrinter: Elem = {
    <withIndex n={ToString(n)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </withIndex>
  }

  override def prettyPrint: String = s"(withIndex $n $dt ${PrettyPhrasePrinter(array)}"

  override def eval(s: Store): Data = ???
}
