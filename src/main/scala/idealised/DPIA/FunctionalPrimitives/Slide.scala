package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.language.{postfixOps, reflectiveCalls}

final case class Slide(n: Nat,
                       sz: Nat,
                       sp: Nat,
                       dt: DataType,
                       input: Phrase[ExpType])
  extends AbstractSlide(n, sz, sp, dt, input)
{
  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Slide(f(n), f(sz), f(sp), f(dt), VisitAndRebuild(input, f))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    con(this)(λ(exp"[$inputSize.$dt]")(x => A :=|dt"[$n.$sz.$dt]"| x ))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    con(input)(λ(exp"[$inputSize.$dt]")(x => C(Slide(n, sz, sp, dt, x)) ))
  }
}
