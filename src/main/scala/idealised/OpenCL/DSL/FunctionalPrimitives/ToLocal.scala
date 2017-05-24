package idealised.OpenCL.DSL.FunctionalPrimitives

import idealised._
import idealised.Core._
import idealised.DSL.untyped._

final case class ToLocal(f: Expr[ExpType -> ExpType], input: DataExpr)
  extends To(f, input, OpenCL.LocalMemory, ToLocal, OpenCL.FunctionalPrimitives.ToLocal)
