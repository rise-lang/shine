package idealised.OpenCL.DSL.FunctionalPrimitives

import idealised._
import idealised.Core._
import idealised.DSL.untyped._

final case class ToGlobal(f: Expr[ExpType -> ExpType], input: DataExpr)
  extends To(f, input, OpenCL.GlobalMemory, ToGlobal, OpenCL.FunctionalPrimitives.ToGlobal)
