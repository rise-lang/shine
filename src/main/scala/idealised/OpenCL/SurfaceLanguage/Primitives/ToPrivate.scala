package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._
import idealised._

final case class ToPrivate(f: Expr[DataType -> DataType], input: DataExpr,
                         override val t: Option[DataType] = None)
  extends To(f, input, OpenCL.PrivateMemory, ToPrivate, OpenCL.FunctionalPrimitives.ToPrivate)
