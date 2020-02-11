package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class TransposeDepArray(n:Nat,
                                   m:Nat,
                                   f:NatToData,
                                   array:Phrase[ExpType])
  extends ExpPrimitive {

  array :: expT(n`.`(m`.d`f), read)
  override val t: ExpType = expT(m`.d`{ k => n`.`f(k) }, read)

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    TransposeDepArray(v.nat(n), v.nat(m), v.natToData(f), VisitAndRebuild(array, v))
  }

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    ???
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(array)(λ(expT(n`.`(m`.d`f), read))(x => C(TransposeDepArray(n, m, f, x))))
  }

  override def xmlPrinter: Elem = {
    <transposeArrayDep n={ToString(n)} m={ToString(m)} f={ToString(f)}>
      {Phrases.xmlPrinter(array)}
    </transposeArrayDep>
  }

  override def prettyPrint: String = s"(transposeArrayDep $n $m $f ${PrettyPhrasePrinter(array)}"

  override def eval(s: Store): Data = ???
}
