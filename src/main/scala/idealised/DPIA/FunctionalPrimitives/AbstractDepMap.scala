package idealised.DPIA.FunctionalPrimitives


import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem


abstract class AbstractDepMap(n: Nat,
                              ft1: NatToData,
                              ft2: NatToData,
                              f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                              array: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType = {
    val k = f.t.x
    (n: Nat) ->: (ft1: NatToData) ->: (ft2: NatToData) ->:
      (f :: t"($k : nat) -> exp[${ ft1(k) }, $read] -> exp[${ ft2(k) }, $write]") ->:
        (array :: ExpType(DepArrayType(n, ft1), read)) ->: // (array :: exp"[$n.$ft1, $read]") ->:
          ExpType(DepArrayType(n, ft2), write) // exp"[$n.$ft2, $write]"
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import idealised.DPIA.Compilation.TranslationToImperative._
    import idealised.DPIA._

    con(array)(λ(exp"[$n.$ft1, $read]")(x =>
      makeMapI(n, ft1, ft2, _Λ_[NatKind]((k: NatIdentifier) => λ(exp"[${ft1(k)}, $read]")(x => λ(acc"[${ft2(k)}]")(o => {
        acc(f(k)(x))(o)
      }))), x, A)))
  }

  override def mapAcceptorTranslation(g: Phrase[ExpType ->: ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] = {
    import idealised.DPIA.Compilation.TranslationToImperative._
    import idealised.DPIA._

    con(array)(λ(exp"[$n.$ft1, $read]")(x =>
      makeMapI(n, ft1, ft2, _Λ_[NatKind]((k: NatIdentifier) => λ(exp"[${ft1(k)}, $read]")(x => λ(acc"[${ft2(k)}]")(o => {
        acc(f(k)(x))(o)
      }))), x, A)))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    `new`(dt"[$n.$ft2]", λ(exp"[$n.$ft2, $read]" x acc"[$n.$ft2]")(tmp =>
      acc(this)(tmp.wr) `;` C(tmp.rd) ))
  }


  def makeMap: (Nat, NatToData, NatToData, Phrase[`(nat)->:`[ExpType ->: ExpType]], Phrase[ExpType]) => AbstractDepMap

  def makeMapI(n: Nat,
               ft1:NatToData,
               ft2:NatToData,
               f: Phrase[`(nat)->:`[ExpType ->: AccType ->: CommType]],
               array: Phrase[ExpType],
               out: Phrase[AccType])
              (implicit context: TranslationContext): Phrase[CommType]

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    val nArray = VisitAndRebuild(array, fun)
    val nF = VisitAndRebuild(f, fun)
    val nFt1 = fun.natToData(ft1)
    val nFt2 = fun.natToData(ft2)
    makeMap(fun.nat(n), nFt1, nFt2, nF, nArray)
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(f)}) (${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem = {
    val k = f match {
      case DepLambda(k_ : NatIdentifier, _) => k_
      case _ => throw new Exception("This should not happen")
    }
    <map n={ToString(n)} ft1={ToString(ft1)} ft2={ToString(ft2)}>
      <f type={ToString(k ->: ExpType(ft1(k), read) ->: ExpType(ft2(k), write))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(DepArrayType(n, ft1), read))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </map>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })
  }
}
