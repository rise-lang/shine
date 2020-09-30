package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL.{λ, _}
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem


final case class DMatchI(x:NatIdentifier,
                        elemT: DataType,
                        outT: DataType,
                        f: Phrase[`(nat)->:`[ExpType ->: CommType]],
                        dPair:Phrase[ExpType]) extends CommandPrimitive {
  override val t: CommType = comm

  override def eval(s: Store): Store = ???

  override def prettyPrint: String = "dMapI"

  override def xmlPrinter = <dMapI></dMapI>

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[CommType] =
      DMatchI(v.nat(x), v.data(elemT), v.data(outT), VisitAndRebuild(f, v), VisitAndRebuild(dPair, v))
}

final case class DMatch(x: NatIdentifier,
                        elemT: DataType,
                        outT: DataType,
                        f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                        dPair: Phrase[ExpType]
                       ) extends ExpPrimitive {
  override val t: ExpType = expT(outT, `write`)

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(dPair)(λ(expT(DepPairType(x, elemT), read))(pair => C(DMatch(x, elemT, outT, f, pair))))

  }
  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(dPair)(λ(expT(DepPairType(x, elemT), read))(pair => DMatchI(x, elemT, outT,
      _Λ_[NatKind]()((fst: NatIdentifier) => λ(expT(DataType.substitute(fst, x, elemT), read))(snd =>
        acc(f(fst)(snd))(A)
      )), pair)))
  }


  override def xmlPrinter = <DMatch x={ToString(x)} elemT={ToString(elemT)} outT={ToString(outT)}>
    <f type={ToString(f.t.x ->: ExpType(elemT, read) ->: ExpType(outT, write))}>
      {Phrases.xmlPrinter(f)}
    </f>
    <input type={ToString(ExpType(DepPairType(x, elemT), read))}>
      {Phrases.xmlPrinter(dPair)}
    </input>
  </DMatch>.copy(label = {
    val name = this.getClass.getSimpleName
    Character.toLowerCase(name.charAt(0)) + name.substring(1)
  })
  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String =  s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(f)}) (${PrettyPhrasePrinter(dPair)})"

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = DMatch(
    v.nat(x),
    v.data(elemT),
    v.data(outT),
    VisitAndRebuild(f, v),
    VisitAndRebuild(dPair, v)
  )
}

final case class MkDPair(a:AccessType, id:NatIdentifier, sndT:DataType, snd: Phrase[ExpType])
  extends ExpPrimitive {
  override val t = expT(DepPairType(id, sndT), a)

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = ???
  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = ???

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = "mkDPair"
  override def xmlPrinter: Elem = <mkDPair></mkDPair>

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = MkDPair(
    v.access(a),
    v.nat(id),
    v.data(sndT),
    VisitAndRebuild(snd, v),
  )
}
