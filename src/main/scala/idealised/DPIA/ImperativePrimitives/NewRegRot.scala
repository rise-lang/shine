package idealised.DPIA.ImperativePrimitives

import idealised.DPIA._
import idealised.DPIA.Types._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics.Store

final case class NewRegRot(n: Nat,
                           dt: DataType,
                           f: Phrase[VarType -> (CommType -> CommType)])
  extends CommandPrimitive
{
  override val t: CommType =
    (n: Nat) -> (dt: DataType) -> (f :: t"var[$n.$dt] -> comm -> comm") -> comm

  override def eval(s: Store): Store = ???

  override def prettyPrint: String = s"(newRegRot $f)"

  override def xmlPrinter: xml.Elem =
    <newCircularBuffer n={ToString(n)} dt={ToString(dt)}>
      <f>{Phrases.xmlPrinter(f)}</f>
    </newCircularBuffer>

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[CommType] = {
    NewRegRot(v(n), v(dt), VisitAndRebuild(f, v))
  }
}
