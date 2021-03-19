package rise.eqsat

import rise.core.types._
import rise.core.semantics

// Rise with DeBruijn indexing
sealed trait Node[+T] {
  def mapChildren[O](fc: T => O): Node[O] = this match {
    case v @ Var(_) => v
    case l @ Literal(_) => l
    case p @ Primitive(_) => p
    case App(f, e) => App(fc(f), fc(e))
    case Lambda(e) => Lambda(fc(e))
    case DepApp(f, x) => DepApp(fc(f), x)
    case DepLambda(k, e) => DepLambda(k, fc(e))
  }
  def children(): Iterator[T] = this match {
    case Var(_) | Literal(_) | Primitive(_) => Iterator()
    case App(f, e) => Iterator(f, e)
    case Lambda(e) => Iterator(e)
    case DepApp(f, _) => Iterator(f)
    case DepLambda(_, e) => Iterator(e)
  }
  def childrenCount(): Int = {
    // FIXME: O(N) length computation, O(1) in egg
    children().length
  }

  // FIXME: match hashing is bugged
  def matchHash(): Int = 0/* this match {
    case Var(i) => 7 * i
    case App(_, _) => 1
    case Lambda(_) => 2
    case DepApp(_, x) => 5// * x.hashCode()
    case DepLambda(k, _) => 3// * k.hashCode()
    case Literal(d) => 13// * d.hashCode()
    case Primitive(p) => 17// * p.hashCode()
  }*/

  // Returns true if this enode matches another enode.
  // This should only consider the operator, not the children ids.
  def matches(other: Node[_]): Boolean = (this, other) match {
    case (Var(i1), Var(i2)) => i1 == i2
    case (App(_, _), App(_, _)) => true
    case (Lambda(_), Lambda(_)) => true
    case (DepApp(_, x1), DepApp(_, x2)) => x1 == x2
    case (DepLambda(k1, _), DepLambda(k2, _)) => k1 == k2
    case (Literal(d1), Literal(d2)) => d1 == d2
    case (Primitive(p1), Primitive(p2)) => p1 == p2
    case _ => false
  }
}

case class Var(index: Int) extends Node[Nothing]
case class App[T](f: T, e: T) extends Node[T]
case class Lambda[T](e: T) extends Node[T]
case class DepApp[K <: Kind, T](f: T, x: K#T) extends Node[T]
case class DepLambda[T](kind: Kind, e: T) extends Node[T]
case class Literal(d: semantics.Data) extends Node[Nothing]
case class Primitive(p: rise.core.Primitive) extends Node[Nothing]
