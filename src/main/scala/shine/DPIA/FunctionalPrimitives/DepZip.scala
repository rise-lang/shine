package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA.{Phrases, _}

import scala.xml.Elem

final case class DepZip(n: Nat,
                        ft1: NatToData,
                        ft2: NatToData,
                        e1: Phrase[ExpType],
                        e2: Phrase[ExpType])
  extends ExpPrimitive {

  e1 :: expT(n`.d`ft1, read)
  e2 :: expT(n`.d`ft2, read)
  override val t: ExpType = expT(n`.d`{ i => PairType(ft1(i), ft2(i)) }, read)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    DepZip(f.nat(n), f.natToData(ft1), f.natToData(ft2), VisitAndRebuild(e1, f), VisitAndRebuild(e2, f))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String =
    s"(depZip ${PrettyPhrasePrinter(e1)} ${PrettyPhrasePrinter(e2)})"

  override def xmlPrinter: Elem =
    <depZip n={ToString(n)} dt1={ToString(ft1)} dt2={ToString(ft2)}>
      <lhs type={ToString(ExpType(DepArrayType(n, ft1), read))}>
        {Phrases.xmlPrinter(e1)}
      </lhs>
      <rhs type={ToString(ExpType(DepArrayType(n, ft2), read))}>
        {Phrases.xmlPrinter(e2)}
      </rhs>
    </depZip>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    ???
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(e1)(λ(ExpType(DepArrayType(n, ft1), read))(x =>
      con(e2)(λ(ExpType(DepArrayType(n, ft2), read))(y =>
        C(DepZip(n, ft1, ft2, x, y)) )) ))
  }
}