package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.DepJoinAcc
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._
import lift.arithmetic.{ArithExpr, BigSum}

import scala.xml.Elem

final case class DepJoin(n: Nat,
                         lenF: NatNatTypeFunction,
                         dt: DataType,
                         array: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType = {
    (n: Nat) -> (lenF: NatNatTypeFunction) -> (dt: DataType) ->
      (array :: exp"[$n.${NatDataTypeFunction(n, (i:NatIdentifier) => ArrayType(lenF(i), dt))}, $read]") ->
      exp"[${BigSum(from = 0, upTo = n - 1, `for` = lenF.x, lenF.body)}.$dt, $read]"
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    DepJoin(fun(n), fun(lenF), fun(dt), VisitAndRebuild(array, fun))
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

  override def prettyPrint: String = s"(depJoin ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <join n={ToString(n)} lenF={ToString(lenF)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </join>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    acc(array)(DepJoinAcc(n, lenF, dt, A))
  }

  override def mapAcceptorTranslation(f: Phrase[ExpType -> ExpType], A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommandType] = ???

  override def continuationTranslation(C: Phrase[ExpType -> CommandType])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    con(array)(λ(exp"[$n.${NatDataTypeFunction(n, (i:NatIdentifier) => ArrayType(lenF(i), dt))}, $read]")(x =>
      C(DepJoin(n, lenF, dt, x)) ))
  }
}