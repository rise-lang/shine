package rise.GAP8

import rise.core.DSL.ToBeTyped
import rise.core.Expr
import shine.DPIA.Nat

object DSL {
  def gap8Run(cores: Nat): ToBeTyped[Expr] = primitives.gap8RunPrimitive(cores)
  def hwce(): ToBeTyped[Expr] = ???
}
