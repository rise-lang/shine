package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.TransposeAcc
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class Transpose(n: Nat, m: Nat, dt: DataType,
                           array: Phrase[ExpType])
  extends ExpPrimitive
{
  override val t: ExpType =
    (n: Nat) ->: (m: Nat) ->: (dt: DataType) ->:
      (array :: exp"[$n.$m.$dt, $read]") ->: exp"[$m.$n.$dt, $read]"

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] =
    Transpose(f.nat(n), f.nat(m), f.data(dt), VisitAndRebuild(array, f))

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    import idealised.DPIA.Compilation.TranslationToImperative._
    acc(array)(TransposeAcc(n, m, dt, A))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
    import idealised.DPIA.Compilation.TranslationToImperative._
    con(array)(fun(array.t)(x => C(Transpose(n, m, dt, x))))
  }

  override def fedeTranslation(env: Predef.Map[Identifier[ExpType], Identifier[AccType]])(C: Phrase[AccType ->: AccType]): Phrase[AccType] = {
    import idealised.DPIA.Compilation.TranslationToImperative._
    fedAcc(env)(array)(fun(AccType(C.t.inT.dataType))(o => TransposeAcc(n, m, dt, C(o))))
  }

  override def xmlPrinter: Elem =
    <transpose>{Phrases.xmlPrinter(array)}</transpose>

  override def prettyPrint: String = s"(transpose $n $m $dt ${PrettyPhrasePrinter(array)})"

  override def eval(s: Store): Data = ???
}
