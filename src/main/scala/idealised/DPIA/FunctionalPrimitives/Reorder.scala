package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.{JoinAcc, ReorderAcc}
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}

import scala.xml.Elem

final case class Reorder(n: Nat,
                         dt: DataType,
                         idxF: Phrase[ExpType ->: ExpType],
                         idxFinv: Phrase[ExpType ->: ExpType],
                         input: Phrase[ExpType])
  extends ExpPrimitive
{
  override val t: ExpType =
    (n: Nat) ->: (dt: DataType) ->:
      (idxF :: t"exp[idx($n), $read] -> exp[idx($n), $read]") ->:
        (idxFinv :: t"exp[idx($n), $read] -> exp[idx($n), $read]") ->:
          (input :: exp"[$n.$dt, $read]") ->:
            exp"[$n.$dt, $read]"

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Reorder(f.nat(n), f.data(dt),
      VisitAndRebuild(idxF, f),
      VisitAndRebuild(idxFinv, f),
      VisitAndRebuild(input, f))
  }

  override def eval(s: Store): Data = {
    import idealised.DPIA.Semantics.OperationalSemantics._
    val idxFE = OperationalSemantics.eval(s, idxF)
    OperationalSemantics.eval(s, input) match {
      case ArrayData(a) =>
        val res = new Array[Data](a.length)
        for (i <- a.indices) {
          res(i) = a(OperationalSemantics.evalIndexExp(s, idxFE(i)).eval)
        }
        ArrayData(res.toVector)
      case _ => throw new Exception("This should not happen")
    }
  }

  override def fedeTranslation(env: scala.Predef.Map[Identifier[ExpType], Identifier[AccType]])
                     (C: Phrase[AccType ->: AccType]) : Phrase[AccType] = {
    import TranslationToImperative._

    val otype = C.t.inT.dataType
    fedAcc(env)(input)(λ(acc"[$otype]")(o => ReorderAcc(n, dt, idxFinv, C(o))))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    acc(input)(ReorderAcc(n, dt, idxFinv, A))
  }

  override def mapAcceptorTranslation(g: Phrase[ExpType ->: ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    mapAcc(g, input)(ReorderAcc(n, dt, idxFinv, A))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(input)(λ(exp"[$n.$dt, $read]")(x => C(Reorder(n, dt, idxF, idxFinv, x)) ))
  }

  override def prettyPrint: String = s"(reorder idxF ${PrettyPhrasePrinter(input)})"

  override def xmlPrinter: Elem =
    <reorder>
      <idxF>{Phrases.xmlPrinter(idxF)}</idxF>
      <idxFinv>{Phrases.xmlPrinter(idxFinv)}</idxFinv>
      <input>{Phrases.xmlPrinter(input)}</input>
    </reorder>
}
