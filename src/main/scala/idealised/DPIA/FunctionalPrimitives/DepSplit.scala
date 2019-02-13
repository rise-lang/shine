package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.SplitAcc
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class DepSplit(n: Nat,
                          m: Nat,
                          dt_i:NatIdentifier, dt: DataType,
                          array: Phrase[ExpType])
  extends ExpPrimitive {

  private def makeDt(x:Nat):DataType = DataType.substitute(x, `for`=dt_i, `in`=dt)

  private def inputType = DepArrayType(m * n, i => makeDt(i))
  private def outputArrayType = {
    DepArrayType(m, row => DepArrayType(n, col => makeDt(row * n + col)))
  }

  override val `type`: ExpType =
    (n: Nat) -> (m: Nat) -> (dt_i: Nat) -> (dt: DataType) ->
      (array :: exp"[$inputType]") -> exp"[$outputArrayType]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    DepSplit(fun(n), fun(m), fun(dt_i).asInstanceOf[NatIdentifier], fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String = s"(depSplit $n ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <depSplit n={ToString(n)} m={ToString(m)} dt_i = {ToString(dt_i)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </depSplit>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    //acc(array)(SplitAcc(n, m, dt, A))
    ???
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    con(array)(λ(exp"[$inputType]")(x => C(DepSplit(n, m, dt_i, dt, x)) ))
  }
}