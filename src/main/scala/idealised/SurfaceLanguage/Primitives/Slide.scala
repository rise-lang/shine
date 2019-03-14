package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

final case class Slide(sz: Nat, sp: Nat, input: Expr,
                       override val t: Option[DataType])
  extends AbstractSlide(sz, sp, input, t)
{
  def makeDPIA(n: Nat,
               sz: Nat,
               sp: Nat,
               dt: DataType,
               input: DPIA.Phrases.Phrase[DPIA.Types.ExpType]
              ): DPIA.Phrases.Phrase[DPIA.Types.ExpType] =
    DPIA.FunctionalPrimitives.Slide(n, sz, sp, dt, input)

  def make(sz: Nat, sp: Nat, input: Expr, t: Option[DataType]) =
    Slide(sz, sp, input, t)
}
