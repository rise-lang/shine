package shine.cuda.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.{acc, con}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.{->:, Phrases, VarType, expT, accT}
import shine.OpenCL.ImperativePrimitives.OpenCLNew

import scala.xml.Elem

case class MapFragmentElements(fragType: WmmaFragment,
                               fragment: Phrase[ExpType],
                               fun: Phrase[ExpType ->: ExpType],
                              ) extends ExpPrimitive {

  fragment :: ExpType(fragType, read)
  fun :: ExpType(fragType.dataType, read) ->: ExpType(fragType.dataType, write)

  override val t: ExpType = ExpType(fragType, write)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    MapFragmentElements(f.data(fragType), VisitAndRebuild(fragment, f), VisitAndRebuild(fun, f))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    val dt = fragType.dataType

    //TODO what about continuationTranslation of a fragment with accesType write???
    //maybe if accesType write allocate Memory with new?
    con(fragment)(λ(expT(fragType, read))(input =>
      shine.cuda.primitives.imperative.ForFragmentElements(fragType, input, A,
        λ(expT(dt, read))(x =>
          λ(accT(dt))(o =>
            acc(fun(x))(o))))))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    val dt = fragType.dataType

    OpenCLNew(AddressSpace.Private, fragType,
      λ(VarType(fragType))(fragmentAcc =>
        (if (fragment.t.accessType.toString == write.toString)
          acc(fragment)(fragmentAcc.wr) `;`
            shine.cuda.primitives.imperative.ForFragmentElements(fragType, fragmentAcc.rd, fragmentAcc.wr,
              λ(expT(dt, read))(x =>
                λ(accT(dt))(o =>
                  acc(fun(x))(o))))
        else
          acceptorTranslation(fragmentAcc.wr)) `;`
        C(fragmentAcc.rd)))
  }

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"MapFragmentElements(${PrettyPhrasePrinter(fragment)}, ${PrettyPhrasePrinter(fun)})"

  override def xmlPrinter: Elem =
    <MapFragmentElements fragmentType={ToString(fragType)}>
      <f>
        {Phrases.xmlPrinter(fun)}
      </f>
      <fragment>
        {Phrases.xmlPrinter(fragment)}
      </fragment>
    </MapFragmentElements>
}
