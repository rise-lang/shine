package shine.DPIA.primitives.functional
import shine.DPIA.Phrases._
import shine.DPIA.Types.Kind.{Identifier => _}
import shine.DPIA.Types._
import shine.DPIA._
//noinspection ZeroIndexToHead
final case class MakeProduct(n: Int)(val ais: Seq[AccessType],
                                     val inTs: Seq[DataType],
                                     val args: Seq[Phrase[ExpType]]) extends ProductPrimitive {
  assert {
    {
      typeAssert(args.length == n, "elements" + ".length == " + "n" + " is not true")
      typeAssert(inTs.length == n, "inTs" + ".length == " + "n" + " is not true")
      typeAssert(ais.length == n, "ais" + ".length == " + "n" + " is not true")
      args.zip(inTs).zip(ais).foreach({
        case ((arg, inT), ai) => arg :: expT(inT, ai)
      })

    }
    true
  }
  override val t: PhrasePairType[PhraseType, PhraseType] = {
    assert(inTs.size >= 2)
    inTs.drop(2).foldLeft(PhrasePairType(ExpType(inTs(0), read), ExpType(inTs(1), read)): PhrasePairType[PhraseType, PhraseType]) {
      case (pt, dt) => PhrasePairType(pt: PhraseType, ExpType(dt, read): PhraseType)
    }
  }
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): MakeProduct =
    new MakeProduct(n)(ais.map(v.access), inTs.map(v.data), args.map(VisitAndRebuild(_, v)))
  def unwrap: (Seq[DataType], Seq[Phrase[ExpType]]) = (inTs, args)
}
