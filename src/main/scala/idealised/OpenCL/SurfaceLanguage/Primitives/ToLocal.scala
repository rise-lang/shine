package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.DPIA.Types.ExpType
import idealised.DPIA._
import idealised.OpenCL
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Expr

final case class ToLocal(f: Expr[ExpType -> ExpType], input: DataExpr)
  extends To(f, input, OpenCL.LocalMemory, ToLocal, OpenCL.FunctionalPrimitives.ToLocal)
