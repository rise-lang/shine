package shine.cuda.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.{->:, Phrases}

import scala.xml.Elem

final case class ForFragmentElements(fragType: FragmentType,
                                     in: Phrase[ExpType],
                                     out: Phrase[AccType],
                                     fun: Phrase[ExpType ->: AccType ->: CommType],
                                    ) extends CommandPrimitive {
  in :: ExpType(fragType, read)
  out :: AccType(fragType)
  fun :: FunType(ExpType(fragType.dataType, read), FunType(AccType(fragType.dataType), comm))

  override def eval(s: Store): Store = ???

  override def prettyPrint: String = s"ForFragmentElements(${PrettyPhrasePrinter(in)}, ${PrettyPhrasePrinter(out)}, ${PrettyPhrasePrinter(fun)})"

  override def xmlPrinter: Elem =
    <ForFragmentElements fragmentType={ToString(fragType)}>
      <f>
        {Phrases.xmlPrinter(fun)}
      </f>
      <fragment>
        {Phrases.xmlPrinter(in)}
      </fragment>
      <fragment>
        {Phrases.xmlPrinter(out)}
      </fragment>
    </ForFragmentElements>

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[CommType] = {
    ForFragmentElements(f.data(fragType), VisitAndRebuild(in, f), VisitAndRebuild(out, f), VisitAndRebuild(fun, f))
  }
}
