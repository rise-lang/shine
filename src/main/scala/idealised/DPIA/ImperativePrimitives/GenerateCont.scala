package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA._
import idealised.DPIA.Types._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._

import scala.xml.Elem

// note: would not be necessary if generate was defined as indices + map
final case class GenerateCont(n: Nat,
                              dt: DataType,
                              f: Phrase[ExpType -> (ExpType -> CommandType -> CommandType)])
  extends ExpPrimitive
{
  override val t: ExpType =
    (n: Nat) -> (dt: DataType) ->
      (f :: exp"[idx($n), $read]" -> (t"exp[$dt, $read] -> comm" -> comm)) ->
      exp"[$n.$dt, $read]"

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    GenerateCont(v(n), v(dt), VisitAndRebuild(f, v))
  }

  override def eval(s: Store): Data = ???

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] =
    throw new Exception("This should not happen")


  override def mapAcceptorTranslation(f: Phrase[ExpType -> ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommandType] =
    throw new Exception("This should not happen")

  override def continuationTranslation(C: Phrase[->[ExpType, CommandType]])
                                      (implicit context: TranslationContext): Phrase[CommandType] =
    throw new Exception("This should not happen")

  override def prettyPrint: String = s"(generateCont $n $f)"

  override def xmlPrinter: Elem =
    <generateCont n={ToString(n)} dt={ToString(dt)}>
      <f>
        {Phrases.xmlPrinter(f)}
      </f>
    </generateCont>
}
