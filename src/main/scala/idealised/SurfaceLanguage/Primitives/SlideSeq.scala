package idealised.SurfaceLanguage.Primitives

import idealised.{DPIA, SurfaceLanguage}
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import TypeInference._

final case class SlideSeq(sz: Nat, sp: Nat, input: DataExpr,
                          override val t: Option[DataType])
  extends AbstractSlide(sz, sp, input, t)
{
  def makeDPIA(n: Nat,
               sz: Nat,
               sp: Nat,
               dt: DataType,
               input: DPIA.Phrases.Phrase[DPIA.Types.ExpType]) =
    DPIA.FunctionalPrimitives.SlideSeq(n, sz, sp, dt, input)

  def make(sz: Nat, sp: Nat, input: DataExpr, t: Option[DataType]) =
    SlideSeq(sz, sp, input, t)
}
