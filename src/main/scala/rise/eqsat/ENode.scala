package rise.eqsat

import rise.core.types._
import rise.core.semantics

// Rise with DeBruijn indexing
sealed trait ENode {
  def countChildren(): Int = {
    var n = 0
    forEachChildren(_ => n += 1)
    n
  }

  def mapChildren(fc: EClassId => EClassId): ENode = this match {
    case Var(_) | Literal(_) | Primitive(_) => this
    case App(f, e) => App(fc(f), fc(e))
    case Lambda(e) => Lambda(fc(e))
    case DepApp(k, f, e) => DepApp(k, fc(f), fc(e))
    case DepLambda(k, e) => DepLambda(k, fc(e))
  }
  def forEachChildren(fc: EClassId => ()): () = this match {
    case Var(_) | Literal(_) | Primitive(_) => ()
    case App(f, e) => fc(f); fc(e)
    case Lambda(e) => fc(e)
    case DepApp(_, f, e) => fc(f); fc(e)
    case DepLambda(_, e) => fc(e)
  }

  def matchHash(): Int = this match {
    case Var(i) => 7 * i
    case App(_, _) => 1
    case Lambda(_) => 2
    case DepApp(_, _, _) => 3
    case DepLambda(_, _) => 4
    case Literal(d) => 13 * d.hashCode()
    case Primitive(p) => 17 * p.hashCode()
  }

  // Returns true if this enode matches another enode.
  // This should only consider the operator, not the children ids.
  def matches(other: ENode): Boolean = (this, other) match {
    case (Var(i1), Var(i2)) => i1 == i2
    case (App(_, _), App(_, _)) => true
    case (Lambda(_), Lambda(_)) => true
    case (DepApp(k1, _, _), DepApp(k2, _, _)) => k1 == k2
    case (DepLambda(k1, _), DepLambda(k2, _)) => k1 == k2
    case (Literal(d1), Literal(d2)) => d1 == d2
    case (Primitive(p1), Primitive(p2)) => p1 == p2
    case _ => false
  }
}
case class Var(index: Int) extends ENode
case class App(f: EClassId, e: EClassId) extends ENode
case class Lambda(e: EClassId) extends ENode
case class DepApp(kind: Kind, f: EClassId, e: EClassId) extends ENode
case class DepLambda(kind: Kind, e: EClassId) extends ENode
case class Literal(d: semantics.Data) extends ENode
case class Primitive(p: rise.core.Primitive) extends ENode
