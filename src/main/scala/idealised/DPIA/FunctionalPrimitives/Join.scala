package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.JoinAcc
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class Join(n: Nat,
                      m: Nat,
                      dt: DataType,
                      array: Phrase[ExpType])
  extends ExpPrimitive {

  override val `type`: ExpType =
    (n: Nat) -> (m: Nat) -> (dt: DataType) ->
      (array :: exp"[$n.$m.$dt]") -> exp"[${n * m}.$dt]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Join(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, array) match {
      case ArrayData(outer) =>
        val arrays = outer.map {
          case ArrayData(inner) => inner
          case _ => throw new Exception("This should not happen")
        }
        ArrayData(arrays.flatten)

      case _ => throw new Exception("This should not happen")
    }
  }

  override def prettyPrint: String = s"(join ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <join n={ToString(n)} m={ToString(m)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </join>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    acc(array)(JoinAcc(n, m, dt, A))
  }

  override def mapAcceptorTranslation(g: Phrase[ExpType -> ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    mapAcc(fun(exp"[$m.$dt]")(x => Map(m, dt, g.t.outT.dataType, g, x)), array)(
      JoinAcc(n, m, dt, A))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    con(array)(λ(exp"[$n.$m.$dt]")(x => C(Join(n, m, dt, x)) ))
  }
}