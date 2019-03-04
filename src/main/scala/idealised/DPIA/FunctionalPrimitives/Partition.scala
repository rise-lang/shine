package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._
import lift.arithmetic.ArithExpr

import scala.xml.Elem

final case class Partition(
                            n: Nat,
                            m: Nat,
                            lenID:NatIdentifier,
                            lenBody:Nat,
                            dt: DataType,
                            array: Phrase[ExpType])
  extends ExpPrimitive {

  val lenF:Nat => Nat = (x:Nat) => ArithExpr.substitute(lenBody, scala.collection.Map((lenID, x)))

  override val `type`: ExpType =
    (n: Nat) -> (m: Nat) -> (dt: DataType) ->
      (array :: exp"[$n.$dt]") -> exp"[${DepArrayType(m, i => ArrayType(lenF(i), dt))}]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Partition(fun(n), fun(m), fun(lenID).asInstanceOf[NatIdentifier], fun(lenBody), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String = s"(partition $n $m ($lenID => $lenBody) ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <partition n={ToString(n)} m={ToString(m)} lenID={ToString(lenID)} lenBody={ToString(lenBody)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </partition>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] = {
    ???
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    con(array)(λ(exp"[$n.$dt]")(x => C(Partition(n, m, lenID, lenBody, dt, x)) ))
  }
}