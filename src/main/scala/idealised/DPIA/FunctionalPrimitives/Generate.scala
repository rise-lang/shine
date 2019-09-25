package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Types._
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.GenerateCont
import idealised.DPIA._

import scala.xml.Elem

final case class Generate(n: Nat,
                          dt: DataType,
                          f : Phrase[ExpType ->: ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (n: Nat) ->: (dt: DataType) ->:
      (f :: t"exp[idx($n), $read] -> exp[$dt, $read]") ->:
        exp"[$n.$dt, $read]"

  def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${PrettyPhrasePrinter(f)})"

  override def xmlPrinter: Elem =
    <generate n={ToString(n)} dt={ToString(dt)}>
      <f type={ToString(ExpType(IndexType(n), read) ->: ExpType(dt, read))}>
       {Phrases.xmlPrinter(f)}
      </f>
    </generate>

  def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] =
    Generate(fun.nat(n), fun.data(dt), VisitAndRebuild(f, fun))

  def eval(s: OperationalSemantics.Store): OperationalSemantics.Data = ???

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] = {
//    import T
//    acc()
    ???
  }

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    // note: would not be necessary if generate was defined as indices + map
    C(GenerateCont(n, dt,
      fun(exp"[idx($n), $read]")(i =>
        fun(exp"[$dt, $read]" ->: (comm: CommType))(cont =>
          con(f(i))(fun(exp"[$dt, $read]")(g => Apply(cont, g)))
        ))
    ))
  }
}
