package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA._
import idealised.DPIA.DSL._
import idealised.DPIA.Types._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.{ArrayData, Data, Store}

// cycles on the m elements of an array (modulo indexing) to produce an array of n elements
final case class Cycle(n: Nat,
                       m: Nat,
                       dt: DataType,
                       input: Phrase[ExpType])
  extends ExpPrimitive
{
  override val t: ExpType =
    (n: Nat) -> (m: Nat) -> (dt: DataType) -> (input :: exp"[$m.$dt]") -> exp"[$n.$dt]"

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, input) match {
      case ArrayData(a) =>
        val N = n.eval
        ArrayData(Stream.continually(a).flatten.take(N).toVector)
      case _ => throw new Exception("this should not happen")
    }
  }

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] =
    Cycle(v(n), v(m), v(dt), VisitAndRebuild(input, v))

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] =
    ???


  override def mapAcceptorTranslation(f: Phrase[ExpType -> ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommandType] =
    ???

  override def continuationTranslation(C: Phrase[->[ExpType, CommandType]])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._
    con(input)(fun(exp"[$m.$dt")(x => C(Cycle(n, m, dt, x))))
  }

  override def xmlPrinter: xml.Elem =
    <cycle n={ToString(n)} m={ToString(m)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(input)}
    </cycle>

  override def prettyPrint: String = s"(cycle $input)"
}
