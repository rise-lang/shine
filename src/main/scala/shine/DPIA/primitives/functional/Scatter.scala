package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.DSL._
import shine.DPIA.primitives.imperative.ScatterAcc
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class Scatter(n: Nat, m: Nat, dt: DataType,
                         indices: Phrase[ExpType],
                         input: Phrase[ExpType])
  extends ExpPrimitive {
  indices :: expT(n `.` idx(m), read)
  input :: expT(n `.` dt, write)
  override val t: ExpType = expT(m `.` dt, write)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] =
    Scatter(f.nat(n), f.nat(m), f.data(dt),
      VisitAndRebuild(indices, f),
      VisitAndRebuild(input, f))

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import shine.DPIA.Compilation.TranslationToImperative._

    con(indices)(fun(expT(m `.` idx(n), read))(y =>
      acc(input)(ScatterAcc(n, m, dt, y, A))
    ))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] =
    throw new Exception("this should not happen")

  override def prettyPrint: String =
    s"(scatter ${PrettyPhrasePrinter(indices)} ${PrettyPhrasePrinter(input)})"

  override def xmlPrinter: Elem =
    <scatter>
      <indices>
        {Phrases.xmlPrinter(indices)}
      </indices>
      <input>
        {Phrases.xmlPrinter(input)}
      </input>
    </scatter>
}
