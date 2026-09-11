package shine.DPIA.Compilation

import rise.core.types.{Kind, read}
import rise.core.types.DataType._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

import scala.annotation.tailrec

class FunDef(val name: String,
             val definition: Phrase[_ <: PhraseType]) {
  val ( body: Phrase[ExpType],
        params: Seq[Identifier[ExpType]],
        topLevelLetNats: Seq[(LetNatIdentifier, Phrase[ExpType])]
      ) = FunDef.splitBodyAndParams(definition)

  val returnType: ExpType = body.t
  val paramTypes: Seq[ExpType] = params.map(_.t)
}

object FunDef {
  def apply(name: String, definition: Phrase[_ <: PhraseType]): FunDef =
    new FunDef(name, definition)

  def splitBodyAndParams(p: Phrase[_]): (
    Phrase[ExpType],
    Seq[Identifier[ExpType]],
    Seq[(LetNatIdentifier, Phrase[ExpType])]
  ) =
    splitBodyAndParamsRec(p, Seq(), Seq())

  @tailrec
  private def splitBodyAndParamsRec(
    p: Phrase[_],
    ps: Seq[Identifier[ExpType]],
    defs: Seq[(LetNatIdentifier, Phrase[ExpType])]
  ): (
    Phrase[ExpType],
    Seq[Identifier[ExpType]],
    Seq[(LetNatIdentifier, Phrase[ExpType])]
  ) =  p match {
    case Apply(f, a) =>
      splitBodyAndParamsRec(Lifting.liftFunction(f).reducing(a), ps, defs)
    case DepApply(_, f, a) =>
      splitBodyAndParamsRec(Lifting.liftDependentFunction(f)(a), ps, defs)
    case l: Lambda[ExpType, _]@unchecked =>
      splitBodyAndParamsRec(l.body, l.param +: ps, defs)
    case ndl: DepLambda[_, _, _] =>
      // TODO: check that this is a NatKind, and why not use NatType ?
      splitBodyAndParamsRec(ndl.body,
        Identifier(Kind.idName(ndl.kind, ndl.x), ExpType(int, read)) +: ps, defs)
    case ln:LetNat[ExpType, _]@unchecked =>
      splitBodyAndParamsRec(ln.body, ps, (ln.binder, ln.defn) +: defs)
    case ep: Phrase[ExpType]@unchecked => (ep, ps.reverse, defs.reverse)
  }
}
