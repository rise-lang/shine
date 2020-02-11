package shine.DPIA.FunctionalPrimitives


import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem


abstract class AbstractDepMap(n: Nat,
                              ft1: NatToData,
                              ft2: NatToData,
                              f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                              array: Phrase[ExpType])
  extends ExpPrimitive {
  {
    val k = f.t.x
    f :: k ->: expT(ft1(k), read) ->: expT(ft2(k), write)
    array :: expT(n `.d` ft1, read)
  }
  override val t: ExpType = expT(n`.d`ft2, write)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import shine.DPIA.Compilation.TranslationToImperative._
    import shine.DPIA._

    con(array)(λ(expT(n`.d`ft1, read))(x =>
      makeMapI(n, ft1, ft2, _Λ_[NatKind]()((k: NatIdentifier) => λ(expT(ft1(k), read))(x => λ(accT(ft2(k)))(o => {
        acc(f(k)(x))(o)
      }))), x, A)))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    `new`(n`.d`ft2, λ(varT(n`.d`ft2))(tmp =>
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
      case _ =>
        println("WARNING: this should not happen?")
        return <map>???</map>
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
