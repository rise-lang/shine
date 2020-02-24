package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.GenerateCont
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class Generate(n: Nat,
                          dt: DataType,
                          f : Phrase[ExpType ->: ExpType])
  extends ExpPrimitive {

  f :: expT(idx(n), read) ->: expT(dt, read)
  override val t: ExpType = expT(n`.`dt, read)

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
      fun(expT(idx(n), read))(i =>
        fun(expT(dt, read) ->: (comm: CommType))(cont =>
          con(f(i))(fun(expT(dt, read))(g => Apply(cont, g)))
        ))
    ))
  }
}
