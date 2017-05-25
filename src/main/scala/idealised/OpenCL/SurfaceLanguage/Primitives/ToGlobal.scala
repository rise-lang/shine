package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Expr
import idealised._

final case class ToGlobal(f: Expr[ExpType -> ExpType], input: DataExpr)
  extends To(f, input, OpenCL.GlobalMemory, ToGlobal, OpenCL.FunctionalPrimitives.ToGlobal)
