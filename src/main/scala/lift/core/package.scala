package lift

import lift.arithmetic._

package object core {
  object freshName {
    private var counter = 0

    def apply(prefix: String): String = {
      counter += 1
      prefix + counter
    }
  }

  type Nat = ArithExpr
  type NatIdentifier = NamedVar with types.Kind.Identifier

  object NatIdentifier {
    def apply(name: String): NatIdentifier = new NamedVar(name) with types.Kind.Identifier
    def apply(name: String, range: Range): NatIdentifier = new NamedVar(name, range) with types.Kind.Identifier
    def apply(nv: NamedVar): NatIdentifier = new NamedVar(nv.name, nv.range) with types.Kind.Identifier
  }

}
