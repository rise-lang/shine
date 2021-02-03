package rise.core

import arithexpr.arithmetic.NamedVar
import rise.core.Traverse._
import rise.core.traversal.{Continue, Result}
import rise.core.types._

object IsClosedForm {
  case class Visitor (var boundV: Set[Identifier], var boundT: Set[Kind.Identifier]) extends Traversal[Option] {
    override implicit def monad = OptionMonad

    override def nat: Nat => Option[Nat] = n => {
      val closed = n.varList.foldLeft(true) {
        case (c, v: NamedVar) => c && boundT(NatIdentifier(v))
        case (c, _) => c
      }
      if (closed) Some(n) else None
    }

    override def binding[I <: Identifier]: I => Option[I] = i => {
      this.boundV = this.boundV + i
      Some(i)
    }

    override def depBinding[I <: Kind.Identifier]: I => Option[I] = i => {
      this.boundT = this.boundT + i
      Some(i)
    }

    override def reference[I <: Identifier]: I => Option[I] = i => {
      if (boundV(i)) Some(i) else None
    }

    override def depReference[I <: Kind.Identifier]: I => Option[I] = i => {
      if (boundT(i)) Some(i) else None
    }
  }

  def apply(expr: Expr): Boolean = {
    Traverse(expr, Visitor(Set(), Set())) match {
      case None => false
      case Some(e) => true
    }
  }
}