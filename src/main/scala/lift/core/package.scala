package lift

import lift.arithmetic._
import lift.core.types.Kind

package object core {
  object freshName {
    private var counter = 0

    def apply(prefix: String): String = {
      counter += 1
      prefix + counter
    }
  }

  type Nat = ArithExpr
  //type NatIdentifier = NamedVar with types.Kind.Identifier with types.Kind.Binder
}
