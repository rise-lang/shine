package idealised.DPIA.FunctionalPrimitives


import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem


abstract class AbstractDepMap(n: Nat,
                              i1: NatIdentifier, dt1: DataType,
                              i2: NatIdentifier, dt2: DataType,
                              f: Phrase[`(nat)->`[ExpType -> ExpType]],
                              array: Phrase[ExpType])
  extends ExpPrimitive {

  private def makeDt1(x:Nat):DataType = DataType.substitute(x, `for`=i1, `in`=dt1)
  private def makeDt2(x:Nat):DataType = DataType.substitute(x, `for`=i2, `in`=dt2)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] = {
    import idealised.DPIA.Compilation.TranslationToImperative._
    import idealised.DPIA._

    con(array)(λ(exp"[${DepArrayType(n, makeDt1)}]")(x =>
      makeMapI(n, i1, dt1, i2, dt2, _Λ_((k: NatIdentifier) => λ(exp"[${makeDt1(k)}]")(x => λ(acc"[${makeDt2(k)}]")(o => {
        acc(f(k)(x))(AccExt(o))
      }))), x, A)))
  }

  override def mapAcceptorTranslation(A: Phrase[AccType], g: Phrase[ExpType -> ExpType])
                                     (implicit context: TranslationContext): Phrase[CommandType] = {
    import idealised.DPIA.Compilation.TranslationToImperative._
    import idealised.DPIA._

    con(array)(λ(exp"[${DepArrayType(n, makeDt1)}]")(x =>
      makeMapI(n, i1, dt1, i2, dt2, _Λ_((k: NatIdentifier) => λ(exp"[${makeDt1(k)}]")(x => λ(acc"[${makeDt2(k)}]")(o => {
        acc(g(f(k)(x)))(AccExt(o))
      }))), x, A)))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    `new`(dt"[${DepArrayType(n, makeDt2)}]", λ(exp"[${DepArrayType(n, makeDt2)}]" x acc"[${DepArrayType(n, makeDt2)}]")(tmp =>
      acc(this)(AccExt(tmp.wr)) `;` C(tmp.rd) ))
  }


  def makeMap: (Nat, NatIdentifier, DataType, NatIdentifier, DataType, Phrase[`(nat)->`[ExpType -> ExpType]], Phrase[ExpType]) => AbstractDepMap

  def makeMapI(n: Nat,
               i1: NatIdentifier, dt1: DataType,
               i2: NatIdentifier, dt2: DataType,
               f: Phrase[`(nat)->`[ExpType -> (AccType -> CommandType)]],
               array: Phrase[ExpType],
               out: Phrase[AccType])
              (implicit context: TranslationContext): Phrase[CommandType]

  override val `type`: ExpType = {
    val k = f.t.x
    (n: Nat) -> (i1: Nat) -> (dt1: DataType) -> (i2: Nat) -> (dt2: DataType) ->
      (f :: t"($k : nat) -> exp[${ makeDt1(k) }] -> exp[${ makeDt2(k) }]")
    (array :: exp"[${DepArrayType(n, makeDt1)}]") ->
      exp"[${DepArrayType(n, makeDt2)}]"
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    makeMap(fun(n), fun(i1).asInstanceOf[NatIdentifier], fun(dt1), fun(i2).asInstanceOf[NatIdentifier], fun(dt2), VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(f)}) (${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem = {
    val k = f match {
      case NatDependentLambda(k_, _) => k_
      case _ => throw new Exception("This should not happen")
    }
    <map n={ToString(n)} i1={ToString(i1)} dt1={ToString(dt1)} i2={ToString(i2)} dt2={ToString(dt2)}>
      <f type={ToString(k -> (ExpType(DataType.substitute(k, `for` = i1, in = dt1)) -> ExpType(DataType.substitute(k, `for` = i2, in = dt2))))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(DepArrayType(n, DataType.substitute(_, `for` = i1, in = dt1))))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </map>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })
  }
}
