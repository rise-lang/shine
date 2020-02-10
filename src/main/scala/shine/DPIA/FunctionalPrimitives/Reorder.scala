package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.ReorderAcc
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA.{Phrases, _}

import scala.xml.Elem

final case class Reorder(n: Nat,
                         dt: DataType,
                         idxF: Phrase[ExpType ->: ExpType],
                         idxFinv: Phrase[ExpType ->: ExpType],
                         input: Phrase[ExpType])
  extends ExpPrimitive
{
  idxF :: expT(idx(n), read) ->: expT(idx(n), read)
  idxFinv :: expT(idx(n), read) ->: expT(idx(n), read)
  input :: expT(n`.`dt, read)
  override val t: ExpType = expT(n`.`dt, read)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Reorder(f.nat(n), f.data(dt),
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
    fedAcc(env)(input)(λ(accT(otype))(o => ReorderAcc(n, dt, idxFinv, C(o))))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    acc(input)(ReorderAcc(n, dt, idxFinv, A))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(input)(λ(expT(n`.`dt, read))(x => C(Reorder(n, dt, idxF, idxFinv, x)) ))
  }

  override def prettyPrint: String = s"(reorder idxF ${PrettyPhrasePrinter(input)})"

  override def xmlPrinter: Elem =
    <reorder>
      <idxF>{Phrases.xmlPrinter(idxF)}</idxF>
      <idxFinv>{Phrases.xmlPrinter(idxFinv)}</idxFinv>
      <input>{Phrases.xmlPrinter(input)}</input>
    </reorder>
}
