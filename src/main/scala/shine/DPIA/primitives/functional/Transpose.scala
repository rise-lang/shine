package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.DSL._
import shine.DPIA.primitives.imperative.TransposeAcc
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class Transpose(
  n: Nat,
  m: Nat,
  dt: DataType,
  access: AccessType,
  array: Phrase[ExpType]
) extends ExpPrimitive {

  array :: expT(n`.`(m`.`dt), access)
  override val t: ExpType = expT(m`.`(n`.`dt), access)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] =
    Transpose(f.nat(n), f.nat(m), f.data(dt), f.access(access),
      VisitAndRebuild(array, f))

  override def fedeTranslation(
    env: Predef.Map[Identifier[ExpType], Identifier[AccType]]
  )(
    C: Phrase[AccType ->: AccType]
  ): Phrase[AccType] = {
    import shine.DPIA.Compilation.TranslationToImperative._
    fedAcc(env)(array)(fun(AccType(C.t.inT.dataType))(o =>
      TransposeAcc(n, m, dt, C(o))))
  }

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import shine.DPIA.Compilation.TranslationToImperative._
    acc(array)(TransposeAcc(n, m, dt, A))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import shine.DPIA.Compilation.TranslationToImperative._
    con(array)(fun(array.t)(x => C(Transpose(n, m, dt, access, x))))
  }

  override def xmlPrinter: Elem =
    <transpose>
      {Phrases.xmlPrinter(array)}
    </transpose>

  override def prettyPrint: String =
    s"(transpose $n $m $dt ${PrettyPhrasePrinter(array)})"

  override def eval(s: Store): Data = ???
}
