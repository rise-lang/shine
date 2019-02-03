package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.{ArrayData, Store}
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.language.{postfixOps, reflectiveCalls}
import scala.xml.Elem

final case class Slide(n: Nat,
                       sz: Nat,
                       sp: Nat,
                       dt: DataType,
                       array: Phrase[ExpType])
  extends ExpPrimitive
{
  private val inputSize = sp * n + sz - sp

  override def `type`: ExpType =
    (n: Nat) -> (sz: Nat) -> (sp: Nat) -> (dt: DataType) ->
      (array :: exp"[$inputSize.$dt]") ->
        exp"[$n.$sz.$dt]"

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Slide(f(n), f(sz), f(sp), f(dt), VisitAndRebuild(array, f))
  }

  override def eval(s: Store): OperationalSemantics.Data = {
    OperationalSemantics.eval(s, array) match {
      case ArrayData(arrayE) =>

        def slide[T](sz: Int, sp: Int, vector: Vector[T]): Vector[Vector[T]] = {
          val builder = Vector.newBuilder[Vector[T]]
          var vec = vector
          while (vec.nonEmpty) {
            builder += vec.take(sz)
            vec = vec.drop(sp)
          }
          builder.result()
        }

        ArrayData(slide(sz.eval, sp.eval, arrayE).map(ArrayData))

      case _ => throw new Exception("This should not happen")
    }
  }

  override def prettyPrint: String = s"(slide $sz $sp ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <slide n={ToString(n)} s1={ToString(sz)} s2={ToString(sp)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </slide>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    con(this)(λ(exp"[$inputSize.$dt]")(x => A :=|dt"[$n.$sz.$dt]"| x ))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    con(array)(λ(exp"[$inputSize.$dt]")(x => C(Slide(n, sz, sp, dt, x)) ))
  }
}
