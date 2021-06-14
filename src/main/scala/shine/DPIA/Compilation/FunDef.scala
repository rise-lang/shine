package shine.DPIA.Compilation

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

import scala.annotation.tailrec

class FunDef(val name: String,
             val definition: Phrase[_ <: PhraseType]) {
  val ( body: Phrase[ExpType],
        params: Seq[Identifier[ExpType]],
        topLevelLetNats: Seq[(LetNatIdentifier, Phrase[ExpType])]
      ) = splitBodyAndParams(definition, Seq(), Seq())

  val returnType: ExpType = body.t
  val paramTypes: Seq[ExpType] = params.map(_.t)

  @tailrec
  private def splitBodyAndParams(p: Phrase[_],
                                 ps: Seq[Identifier[ExpType]],
                                 defs: Seq[(LetNatIdentifier, Phrase[ExpType])])
  : ( Phrase[ExpType],
      Seq[Identifier[ExpType]],
      Seq[(LetNatIdentifier, Phrase[ExpType])]
    ) = p match {
      case Apply(f, a) =>
        splitBodyAndParams(Lifting.liftFunction(f).reducing(a), ps, defs)
      case DepApply(_, f, a) =>
        splitBodyAndParams(Lifting.liftDependentFunction(f)(a), ps, defs)
      case l: Lambda[ExpType, _]@unchecked =>
        splitBodyAndParams(l.body, l.param +: ps, defs)
      case ndl: DepLambda[_, _, _] =>
        splitBodyAndParams(ndl.body,
          Identifier(ndl.x.name, ExpType(int, read)) +: ps, defs)
      case ln:LetNat[ExpType, _]@unchecked =>
        splitBodyAndParams(ln.body, ps, (ln.binder, ln.defn) +: defs)
      case ep: Phrase[ExpType]@unchecked => (ep, ps.reverse, defs.reverse)
    }
}

object FunDef {
  def apply(name: String, definition: Phrase[_ <: PhraseType]): FunDef =
    new FunDef(name, definition)
}
