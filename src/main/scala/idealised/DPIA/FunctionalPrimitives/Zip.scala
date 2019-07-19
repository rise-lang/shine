package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{CodeGenerator, TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.{ZipAcc1, ZipAcc2}
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}

import scala.xml.Elem

final case class Zip(n: Nat,
                     dt1: DataType,
                     dt2: DataType,
                     e1: Phrase[ExpType],
                     e2: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (e1 :: exp"[$n.$dt1]") ->
       (e2 :: exp"[$n.$dt2]") -> exp"[$n.($dt1 x $dt2)]"

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Zip(f.nat(n), f.data(dt1), f.data(dt2), VisitAndRebuild(e1, f), VisitAndRebuild(e2, f))
  }

  override def eval(s: Store): Data = {
    (OperationalSemantics.eval(s, e1), OperationalSemantics.eval(s, e2)) match {
      case (ArrayData(lhsE), ArrayData(rhsE)) =>
        ArrayData((lhsE zip rhsE) map { p =>
          RecordData(p._1, p._2)
        })

      case _ => throw new Exception("This should not happen")
    }
  }

  override def prettyPrint: String =
    s"(zip ${PrettyPhrasePrinter(e1)} ${PrettyPhrasePrinter(e2)})"

  override def xmlPrinter: Elem =
    <zip n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <lhs type={ToString(ExpType(ArrayType(n, dt1)))}>
        {Phrases.xmlPrinter(e1)}
      </lhs>
      <rhs type={ToString(ExpType(ArrayType(n, dt2)))}>
        {Phrases.xmlPrinter(e2)}
      </rhs>
    </zip>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    acc(e1)(ZipAcc1(n, dt1, dt2, A)) `;`
      acc(e2)(ZipAcc2(n, dt1, dt2, A))
  }

  // TODO?
  override def mapAcceptorTranslation(f: Phrase[ExpType ->: ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] =
    ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(e1)(λ(exp"[$n.$dt1]")(x =>
      con(e2)(λ(exp"[$n.$dt2]")(y =>
        C(Zip(n, dt1, dt2, x, y)) )) ))
  }
}