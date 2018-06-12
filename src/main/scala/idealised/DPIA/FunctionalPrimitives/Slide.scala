package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.RewriteToImperative
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.SlideAcc
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.Store
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class Slide(n: Nat,
                       s1: Nat,
                       s2: Nat,
                       dt: DataType,
                       array: Phrase[ExpType])
  extends ExpPrimitive
{
  override def `type`: ExpType =
    (n: Nat) -> (s1: Nat) -> (s2: Nat) -> (dt: DataType) ->
      (array :: exp"[${s2 * n + s1 - s2}.$dt]") ->
        exp"[$n.$s1.$dt]"

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Slide(f(n), f(s1), f(s2), f(dt), VisitAndRebuild(array, f))
  }

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"(slide $s1 $s2 ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <slide n={ToString(n)} s1={ToString(s1)} s2={ToString(s2)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </slide>

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._

    acc(array)(SlideAcc(n, s1, s2, dt, A))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    import RewriteToImperative._

    con(array)(λ(exp"[${s2 * n + s1 - s2}.$dt]")(x => C(Slide(n, s1, s2, dt, x)) ))
  }
}
