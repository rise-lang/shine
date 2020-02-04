package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.language.reflectiveCalls

final case class Slide(n: Nat,
                       sz: Nat,
                       sp: Nat,
                       dt: DataType,
                       input: Phrase[ExpType])
  extends AbstractSlide(n, sz, sp, dt, input)
{
  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Slide(f.nat(n), f.nat(sz), f.nat(sp), f.data(dt), VisitAndRebuild(input, f))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(this)(λ(expT(n`.`(sz`.`dt), read))(x => A :=|(n`.`(sz`.`dt))| x ))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(input)(λ(expT(inputSize`.`dt, read))(x => C(Slide(n, sz, sp, dt, x)) ))
  }
}
