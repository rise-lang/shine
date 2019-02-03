package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.IntermediatePrimitives.AbstractMapI
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

abstract class AbstractMap(n: Nat,
                           dt1: DataType,
                           dt2: DataType,
                           f: Phrase[ExpType -> ExpType],
                           array: Phrase[ExpType])
  extends ExpPrimitive {

  def makeMap: (Nat, DataType, DataType, Phrase[ExpType -> ExpType], Phrase[ExpType]) => AbstractMap

  def makeMapI: (Nat, DataType, DataType,
    Phrase[ExpType -> (AccType -> CommandType)], Phrase[ExpType], Phrase[AccType]) => AbstractMapI


  override val `type`: ExpType =
    (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (f :: t"exp[$dt1] -> exp[$dt2]") ->
      (array :: exp"[$n.$dt1]") -> exp"[$n.$dt2]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    makeMap(fun(n), fun(dt1), fun(dt2), VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = {
    import idealised.DPIA.Semantics.OperationalSemantics._
    val fE = OperationalSemantics.eval(s, f)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        ArrayData(xs.map { x =>
          OperationalSemantics.eval(s, fE(Literal(x)))
        })

      case _ => throw new Exception("This should not happen")
    }
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    con(array)(λ(exp"[$n.$dt1]")(x =>
      makeMapI(n, dt1, dt2, λ(exp"[$dt1]")(x => λ(acc"[$dt2]")(o => acc(f(x))(o))), x, A)))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    `new`(dt"[$n.$dt2]", idealised.OpenCL.GlobalMemory, λ(exp"[$n.$dt2]" x acc"[$n.$dt2]")(tmp =>
      acc(this)(tmp.wr) `;` C(tmp.rd) ))
  }

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(f)}) (${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <map n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f type={ToString(ExpType(dt1) -> ExpType(dt2))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(ExpType(ArrayType(n, dt1)))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </map>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })
}
