package shine.OpenCL.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL.{`new` => _, _}
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.{Data, Store}
import shine.DPIA.Types._
import shine.DPIA.{Phrases, _}
import shine.OpenCL.AdjustArraySizesForAllocations
import shine.OpenCL.DSL.`new`

import scala.xml.Elem

final case class To(addrSpace: AddressSpace,
                    dt: DataType,
                    input: Phrase[ExpType])
  extends ExpPrimitive {

  input :: expT(dt, write)
  override val t: ExpType = expT(dt, read)

  override def eval(s: Store): Data = OperationalSemantics.eval(s, input)

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    To(fun.addressSpace(addrSpace), fun.data(dt), VisitAndRebuild(input, fun))
  }

  override def prettyPrint: String =
    s"(to$addrSpace ${PrettyPhrasePrinter(input)})"

  override def xmlPrinter: Elem =
    <to addrSpace={ToString(addrSpace)} dt={ToString(dt)}>
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

    val adj = AdjustArraySizesForAllocations(input, dt, addrSpace)
    `new` (addrSpace) (adj.dt, tmp => acc(input)(adj.accF(tmp.wr)) `;` C(adj.exprF(tmp.rd)))
  }
}
