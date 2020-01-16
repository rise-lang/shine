package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.ReorderAcc
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.{Phrases, _}

import scala.xml.Elem

final case class Reorder(n: Nat,
                         dt: DataType,
                         access: AccessType,
                         idxF: Phrase[ExpType ->: ExpType],
                         idxFinv: Phrase[ExpType ->: ExpType],
                         input: Phrase[ExpType])
  extends ExpPrimitive
{
  override val t: ExpType =
    (n: Nat) ->: (dt: DataType) ->:
      (idxF :: t"exp[idx($n), $read] -> exp[idx($n), $read]") ->:
        (idxFinv :: t"exp[idx($n), $read] -> exp[idx($n), $read]") ->:
          (input :: exp"[$n.$dt, $access]") ->:
            exp"[$n.$dt, $access]"

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Reorder(f.nat(n), f.data(dt), f.access(access),
      VisitAndRebuild(idxF, f),
      VisitAndRebuild(idxFinv, f),
      VisitAndRebuild(input, f))
  }

  override def eval(s: Store): Data = {
    import shine.DPIA.Semantics.OperationalSemantics._
    val idxFE = OperationalSemantics.eval(s, idxF)
    OperationalSemantics.eval(s, input) match {
      case ArrayData(a) =>
        val res = new scala.Array[Data](a.length)
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

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(input)(λ(exp"[$n.$dt, $read]")(x => C(Reorder(n, dt, access, idxF, idxFinv, x)) ))
  }

  override def prettyPrint: String = s"(reorder idxF ${PrettyPhrasePrinter(input)})"

  override def xmlPrinter: Elem =
    <reorder access={ToString(access)}>
      <idxF>{Phrases.xmlPrinter(idxF)}</idxF>
      <idxFinv>{Phrases.xmlPrinter(idxFinv)}</idxFinv>
      <input>{Phrases.xmlPrinter(input)}</input>
    </reorder>
}
