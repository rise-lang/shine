package rise.core.types

import rise.arithmetic._
import rise.core.types

class NatIdentifier(override val name: String,
                    override val range: Range = RangeUnknown,
                    override val isExplicit: Boolean = false)
  extends NamedVar(name, range) with types.Kind.Identifier with types.Kind.Explicitness {

  override lazy val toString: String = if (isExplicit) name else "_" + name

  override def copy(r: Range): NatIdentifier = new NatIdentifier(name, r, isExplicit)

  override def asExplicit: NatIdentifier = new NatIdentifier(name, range, true)

  override def asImplicit: NatIdentifier = new NatIdentifier(name, range, false)

  override def visitAndRebuild(f: (ArithExpr) => ArithExpr): ArithExpr = {
    f(new NatIdentifier(name, range.visitAndRebuild(f), isExplicit))
  }

  override def cloneSimplified() = new NatIdentifier(name, range, isExplicit) with SimplifiedExpr
}


object NatIdentifier {
  def apply(name: String, isExplicit: Boolean): NatIdentifier = new NatIdentifier(name, isExplicit = isExplicit)

  def apply(name: String): NatIdentifier = apply(name, isExplicit = false)

  def apply(name: String, range: Range, isExplicit: Boolean): NatIdentifier = new NatIdentifier(name, range, isExplicit = isExplicit)

  def apply(name: String, range: Range): NatIdentifier = apply(name, range, isExplicit = false)

  def apply(nv: NamedVar, isExplicit: Boolean): NatIdentifier = new NatIdentifier(nv.name, nv.range, isExplicit = isExplicit)

  def apply(nv: NamedVar): NatIdentifier = apply(nv, isExplicit = false)
}
