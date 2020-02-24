package shine.DPIA.FunctionalPrimitives

import arithexpr.arithmetic.BigSum
import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.DepJoinAcc
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class DepJoin(n: Nat,
                         lenF: NatToNat,
                         dt: DataType,
                         array: Phrase[ExpType])
  extends ExpPrimitive {

  array :: expT(n`.d`{ i => lenF(i)`.`dt }, read)
  override val t: ExpType =
    expT(BigSum(from = 0, upTo = n - 1, i => lenF(i))`.`dt, read)

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    DepJoin(fun.nat(n), fun.natToNat(lenF), fun.data(dt), VisitAndRebuild(array, fun))
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
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    acc(array)(DepJoinAcc(n, lenF, dt, A))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(array)(λ(expT(n`.d`{ i => lenF(i)`.`dt }, read))(x =>
      C(DepJoin(n, lenF, dt, x)) ))
  }
}