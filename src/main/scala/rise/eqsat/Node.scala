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
  def childrenCount(): Int =
    children().length

  def mapNats(fn: Nat => Nat): Node[T] = this match {
    case DepApp(f, n: Nat) => DepApp[NatKind, T](f, fn(n))
    case _ => this
  }
  def nats(): Iterator[Nat] = this match {
    case DepApp(_, n: Nat) => Iterator(n)
    case _ => Iterator()
  }
  def natsCount(): Int = nats().length

  def matchHash(): Int = this match {
    case Var(i) => 7 * i
    case App(_, _) => 1
    case Lambda(_) => 2
    case DepApp(_, _) => 4
    case DepLambda(k, _) => 3 * k.hashCode()
    case Literal(d) => 13 * d.hashCode()
    case Primitive(p) => 17 * p.hashCode()
  }

  // Returns true if this enode matches another enode.
  // This should only consider the operator, not the children ids or nats.
  def matches(other: Node[_]): Boolean = (this, other) match {
    case (Var(i1), Var(i2)) => i1 == i2
    case (App(_, _), App(_, _)) => true
    case (Lambda(_), Lambda(_)) => true
    case (DepApp(_, _: Nat), DepApp(_, _: Nat)) => true
    case (DepApp(_, _: DataType), DepApp(_, _: DataType)) => true
    // TODO: other Kinds
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

object Node {
  import math.Ordering.Implicits.seqOrdering

  implicit val natOrdering: Ordering[Nat] = new Ordering[Nat] {
    import arithexpr.arithmetic._
    def compare(n1: Nat, n2: Nat): Int = {
      implicit val ord: Ordering[Nat] = this
      (n1, n2) match {
        case (Cst(c1), Cst(c2)) => c1 compare c2
        /*case (Sum(ts1), Sum(ts2)) =>
          implicitly[Ordering[Seq[Nat]]].compare(ts1, ts2)
        case (Prod(fs1), Prod(fs2)) =>
          implicitly[Ordering[Seq[Nat]]].compare(fs1, fs2)*/
        case _ => ???
      }
    }
  }

  implicit val dataTypeOrdering: Ordering[DataType] = new Ordering[DataType] {
    def compare(dt1: DataType, dt2: DataType): Int =
      ???
  }

  implicit val depValOrdering: Ordering[Kind#T] = new Ordering[Kind#T] {
    def compare(v1: Kind#T, v2: Kind#T): Int =
      (v1, v2) match {
        case (n1: Nat, n2: Nat) => natOrdering.compare(n1, n2)
        case (_: Nat, _) => -1
        case (_, _: Nat) => 1
        case (dt1: DataType, dt2: DataType) => dataTypeOrdering.compare(dt1, dt2)
        case (_: DataType, _) => -1
        case (_, _: DataType) => 1
        case _ => ???
      }
  }

  implicit val scalarDataOrdering: Ordering[semantics.ScalarData] = new Ordering[semantics.ScalarData] {
    import semantics._

    def compare(sd1: ScalarData, sd2: ScalarData): Int =
      (sd1, sd2) match {
        case (BoolData(b1), BoolData(b2)) => b1 compare b2
        case (BoolData(_), _) => -1
        case (_, BoolData(_)) => 1
        case (IntData(i1), IntData(i2)) => i1 compare i2
        case (IntData(_), _) => -1
        case (_, IntData(_)) => 1
        case (FloatData(f1), FloatData(f2)) => f1 compare f2
        case (FloatData(_), _) => -1
        case (_, FloatData(_)) => 1
        case (DoubleData(d1), DoubleData(d2)) => d1 compare d2
        case (DoubleData(_), _) => -1
        case (_, DoubleData(_)) => 1
      }
  }

  implicit val dataOrdering: Ordering[semantics.Data] = new Ordering[semantics.Data] {
    import semantics._

    def compare(d1: Data, d2: Data): Int =
      (d1, d2) match {
        case (NatData(n1), NatData(n2)) => natOrdering.compare(n1, n2)
        case (NatData(_), _) => -1
        case (_, NatData(_)) => 1
        case (IndexData(i1, n1), IndexData(i2, n2)) =>
          implicitly[Ordering[(Nat, Nat)]].compare((i1, n1), (i2, n2))
        case (IndexData(_, _), _) => -1
        case (_, IndexData(_, _)) => 1
        case (sd1: ScalarData, sd2: ScalarData) => scalarDataOrdering.compare(sd1, sd2)
        case (_: ScalarData, _) => -1
        case (_, _: ScalarData) => 1
        case (VectorData(v1), VectorData(v2)) =>
          implicitly[Ordering[Seq[ScalarData]]].compare(v1, v2)
        case (VectorData(_), _) => -1
        case (_, VectorData(_)) => 1
        case (ArrayData(a1), ArrayData(a2)) =>
          implicitly[Ordering[Seq[Data]]].compare(a1, a2)
        case (ArrayData(_), _) => -1
        case (_, ArrayData(_)) => 1
        case (PairData(a1, b1), PairData(a2, b2)) =>
          implicitly[Ordering[(Data, Data)]].compare((a1, b1), (a2, b2))
        case (PairData(_, _), _) => -1
        case (_, PairData(_, _)) => 1
      }
  }

  implicit val eclassIdOrdering: Ordering[EClassId] = new Ordering[EClassId] {
    def compare(id1: EClassId, id2: EClassId): Int =
      id1.i compare id2.i
  }

  implicit def ordering[T](implicit tOrd: Ordering[T]): Ordering[Node[T]] = new Ordering[Node[T]] {
    def compare(n1: Node[T], n2: Node[T]): Int =
      (n1, n2) match {
        case (Var(i1), Var(i2)) => i1 compare i2
        case (Var(_), _) => -1
        case (_, Var(_)) => 1
        case (App(f1, e1), App(f2, e2)) =>
          implicitly[Ordering[(T, T)]].compare((f1, e1), (f2, e2))
        case (App(_, _), _) => -1
        case (_, App(_, _)) => 1
        case (Lambda(e1), Lambda(e2)) => tOrd.compare(e1, e2)
        case (Lambda(_), _) => -1
        case (_, Lambda(_)) => 1
        case (DepApp(f1, x1), DepApp(f2, x2)) =>
          implicitly[Ordering[(T, Kind#T)]].compare((f1, x1), (f2, x2))
        case (DepApp(_, _), _) => -1
        case (_, DepApp(_, _)) => 1
        case (DepLambda(k1, e1), DepLambda(k2, e2)) => ???
          // implicitly[Ordering[(Kind, T)].compare((k1, e1), (k2, e2))
        case (DepLambda(_, _), _) => -1
        case (_, DepLambda(_, _)) => 1
        case (Literal(d1), Literal(d2)) => dataOrdering.compare(d1, d2)
        case (Literal(_), _) => -1
        case (_, Literal(_)) => 1
        case (Primitive(p1), Primitive(p2)) => p1.name compare p2.name
      }
  }
}