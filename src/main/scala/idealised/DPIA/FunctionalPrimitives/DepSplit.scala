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
                          ft:NatDataTypeFunction,
                          array: Phrase[ExpType])
  extends ExpPrimitive {


  private def inputType = DepArrayType(m * n, ft)
  private def outputArrayType = {
    DepArrayType(m, row => DepArrayType(n, col => ft(row * n + col)))
  }

  override val `type`: ExpType =
    (n: Nat) -> (m: Nat) -> (ft:NatDataTypeFunction) ->
      (array :: exp"[$inputType]") -> exp"[$outputArrayType]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    DepSplit(fun(n), fun(m), ft, VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String = s"(depSplit $n ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <depSplit n={ToString(n)} m={ToString(m)} ft={ToString(ft)}>
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

    con(array)(λ(exp"[$inputType]")(x => C(DepSplit(n, m, ft, x)) ))
  }
}