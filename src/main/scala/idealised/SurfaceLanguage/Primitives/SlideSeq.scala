package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

final case class SlideSeq(override val sz: Nat, override val sp: Nat, override val input: Expr,
                          override val t: Option[DataType])
  extends AbstractSlide(sz, sp, input, t) {
  
  def make(sz: Nat, sp: Nat, input: Expr, t: Option[DataType]) =
    SlideSeq(sz, sp, input, t)
}
