package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import idealised._

final case class ToGlobal(f: Expr[DataType -> DataType], input: DataExpr,
                          override val `type`: Option[DataType] = None)
  extends To(f, input, OpenCL.GlobalMemory, ToGlobal, OpenCL.FunctionalPrimitives.ToGlobal)
