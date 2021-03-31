package shine.DPIA.FunctionalPrimitives


import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.{MkDPairFstI, MkDPairSndAcc, Skip}
import shine.DPIA.Phrases.VisitAndRebuild.KindVisitable
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.{IndexData, Store, U32Data}
import shine.DPIA.Types._
import shine.DPIA.{ImperativePrimitives, _}

import scala.xml.Elem

final case class MkDPair[K <: Kind: KindName: KindReified:KindVisitable](a: AccessType, fst: K#I, sndT: DataType, snd: Phrase[ExpType])
  extends ExpPrimitive {
  override val t: ExpType = expT(DepPairType[K](fst, sndT), a)

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    // Allocate for the resulting dependent pair,
    // then imperatively write the first element,
    // acc-translate and write the second element
    // and call the continuation on the result
    // TODO(federico) - This is allocating eagerly. Make it allocate lazily by adding a suitable primitive:
    //  ideally Dmatch(..,..., MkDPair(x, y))
    // should not allocate

    con(snd)(λ(expT(sndT, read))(snd => `new`(t.dataType, outVar => {
      MkDPairFstI(fst, outVar.wr) `;`
        acc(snd)(MkDPairSndAcc(fst, sndT, outVar.wr)) `;`
        C(outVar.rd)
    })))
    //con(snd)(λ(expT(sndT, read))(snd => C(MkDPair(a, fst, sndT, snd))))
  }

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    // We have the acceptor already, so simply write the first element and then
    // the second element in sequentially

    ImperativePrimitives.MkDPairFstI(fst, A) `;`
      acc(snd)(ImperativePrimitives.MkDPairSndAcc(fst, sndT, A))
  }

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"${this.getClass.getSimpleName} (${fst}) (${PrettyPhrasePrinter(snd)})"

  override def xmlPrinter: Elem = <MkDPair a={ToString(a)} sndT={ToString(sndT)}>
    <fst>
      {ToString(fst)}
    </fst>
    <snd type={ToString(sndT)}>
      {Phrases.xmlPrinter(snd)}
    </snd>
  </MkDPair>.copy(label = {
    val name = this.getClass.getSimpleName
    s"${Character.toLowerCase(name.charAt(0))}${name.substring(1)}"
  })

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = MkDPair(
    v.access(a),
    implicitly[KindVisitable[K]].visit(v, fst),
    v.data(sndT),
    VisitAndRebuild(snd, v),
  )
}
