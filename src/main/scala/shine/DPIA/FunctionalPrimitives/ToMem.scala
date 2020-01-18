package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL.{`new` => _, _}
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.{Data, Store}
import shine.DPIA.Types._
import shine.DPIA.DSL._
import shine.DPIA._

import scala.xml.Elem

final case class ToMem(dt: DataType,
                       input: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (dt: DataType) ->:
      (input :: exp"[$dt, $write]") ->: exp"[$dt, $read]"

  override def eval(s: Store): Data = OperationalSemantics.eval(s, input)

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    ToMem(fun.data(dt), VisitAndRebuild(input, fun))
  }

  override def prettyPrint: String =
    s"(toMem ${PrettyPhrasePrinter(input)})"

  override def xmlPrinter: Elem =
    <to dt={ToString(dt)}>
      <input type={ToString(ExpType(dt, write))}>
        {Phrases.xmlPrinter(input)}
      </input>
    </to>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    ???
  }

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    `new` (dt, tmp => acc(input)(tmp.wr) `;` C(tmp.rd))
  }
}
