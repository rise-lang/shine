package idealised.DPIA.FunctionalPrimitives


import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem


abstract class AbstractDepMap(n: Nat,
                              ft1:NatDataTypeFunction,
                              ft2: NatDataTypeFunction,
                              f: Phrase[`(nat)->`[ExpType -> ExpType]],
                              array: Phrase[ExpType])
  extends ExpPrimitive {


  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] = {
    import idealised.DPIA.Compilation.TranslationToImperative._
    import idealised.DPIA._

    con(array)(λ(exp"[$n.$ft1]")(x =>
      makeMapI(n, ft1, ft2, _Λ_((k: NatIdentifier) => λ(exp"[${ft1(k)}]")(x => λ(acc"[${ft2(k)}]")(o => {
        acc(f(k)(x))(o)
      }))), x, A)))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    `new`(dt"[$n.$ft2]", λ(exp"[$n.$ft2]" x acc"[$n.$ft2]")(tmp =>
      acc(this)(tmp.wr) `;` C(tmp.rd) ))
  }


  def makeMap: (Nat, NatDataTypeFunction, NatDataTypeFunction, Phrase[`(nat)->`[ExpType -> ExpType]], Phrase[ExpType]) => AbstractDepMap

  def makeMapI(n: Nat,
               ft1:NatDataTypeFunction,
               ft2:NatDataTypeFunction,
               f: Phrase[`(nat)->`[ExpType -> (AccType -> CommandType)]],
               array: Phrase[ExpType],
               out: Phrase[AccType])
              (implicit context: TranslationContext): Phrase[CommandType]

  override val `type`: ExpType = {
    val k = f.t.x
    (n: Nat) -> (ft1:NatDataTypeFunction) -> (ft2:NatDataTypeFunction) ->
      (f :: t"($k : nat) -> exp[${ ft1(k) }] -> exp[${ ft2(k) }]")
    (array :: exp"[$n.$ft1]") ->
      exp"[$n.$ft2]"
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    makeMap(fun(n), fun(ft1), fun(ft2), VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(f)}) (${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem = {
    val k = f match {
      case NatDependentLambda(k_, _) => k_
      case _ => throw new Exception("This should not happen")
    }
    <map n={ToString(n)} ft1={ToString(ft1)} ft2={ToString(ft2)}>
      <f type={ToString(k -> (ExpType(ft1(k)) -> ExpType(ft2(k))))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(DepArrayType(n, ft1)))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </map>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })
  }
}
