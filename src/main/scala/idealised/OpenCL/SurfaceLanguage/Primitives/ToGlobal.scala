package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Types._
import idealised.OpenCL.AddressSpace

final case class ToGlobal(override val f: Expr, override val input: Expr,
                          override val t: Option[DataType] = None)
  extends To(f, input, AddressSpace.Global, ToGlobal)
