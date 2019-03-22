package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Types._
import idealised._

final case class ToLocal(override val f: Expr, override val input: Expr,
                          override val t: Option[DataType] = None)
  extends To(f, input, OpenCL.LocalMemory, ToLocal)
