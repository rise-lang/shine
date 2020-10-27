package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL.{λ, _}
import shine.DPIA.ImperativePrimitives.DMatchI
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.{ImperativePrimitives, _}

import scala.xml.Elem

final case class DMatch(x: NatIdentifier,
                        elemT: DataType,
                        outT: DataType,
                        aIn: AccessType,
                        aOut: AccessType,
                        f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                        input: Phrase[ExpType]
                       ) extends ExpPrimitive {
  override val t: ExpType = expT(outT, aOut)

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    // Turn the f imperative by means of forwarding the continuation translation
    con(input)(λ(expT(DepPairType(x, elemT), aIn))(pair =>
        DMatchI(x, elemT, outT,
          _Λ_[NatKind]()((fst: NatIdentifier) => λ(expT(DataType.substitute(fst, x, elemT), aIn))(snd =>
            con(f(fst)(snd))(C)
          )), pair)))
  }

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    // Turn the f imperative by means of forwarding the acceptor translation
    con(input)(λ(expT(DepPairType(x, elemT), aIn))(pair => ImperativePrimitives.DMatchI(x, elemT, outT,
      _Λ_[NatKind]()((fst: NatIdentifier) => λ(expT(DataType.substitute(fst, x, elemT), aIn))(snd =>
        acc(f(fst)(snd))(A)
      )), pair)))
  }


  override def xmlPrinter: Elem = <DMatch x={ToString(x)} elemT={ToString(elemT)} outT={ToString(outT)}>
    <f type={ToString(f.t.x ->: ExpType(elemT, read) ->: ExpType(outT, write))}>
      {Phrases.xmlPrinter(f)}
    </f>
    <input type={ToString(ExpType(DepPairType(x, elemT), read))}>
      {Phrases.xmlPrinter(input)}
    </input>
  </DMatch>.copy(label = {
    val name = this.getClass.getSimpleName
    s"${Character.toLowerCase(name.charAt(0))}${name.substring(1)}"
  })
  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String =  s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(f)}) (${PrettyPhrasePrinter(input)})"

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = DMatch(
    v.nat(x),
    v.data(elemT),
    v.data(outT),
    v.access(aIn),
    v.access(aOut),
    VisitAndRebuild(f, v),
    VisitAndRebuild(input, v)
  )
}






