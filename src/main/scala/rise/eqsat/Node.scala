package rise.eqsat

import rise.core.{types => rct}
import rise.core.semantics

/** A term node for the Rise language with DeBruijn indexing.
  * @tparam E abstracts over children nodes, and differs for [[Expr]], [[ENode]], [[Pattern]], etc
  * @tparam N abstracts over contained nats
  * @tparam DT abstracts over contained data types
  * @tparam A abstracts over contained address spaces
  */
sealed trait Node[+E, +N, +DT, +A] {
  def map[OE, ON, ODT, OA](fe: E => OE,
                           fn: N => ON,
                           fdt: DT => ODT,
                           fa: A => OA): Node[OE, ON, ODT, OA] = this match {
    case v @ Var(_) => v
    case l @ Literal(_) => l
    case p @ Primitive(_) => p
    case App(f, e) => App(fe(f), fe(e))
    case Lambda(e) => Lambda(fe(e))
    case NatApp(f, x) => NatApp(fe(f), fn(x))
    case NatLambda(e) => NatLambda(fe(e))
    case DataApp(f, x) => DataApp(fe(f), fdt(x))
    case DataLambda(e) => DataLambda(fe(e))
    case AddrApp(f, x) => AddrApp(fe(f), fa(x))
    case AddrLambda(e) => AddrLambda(fe(e))
    case NatLiteral(n) => NatLiteral(fn(n))
    case IndexLiteral(i, n) => IndexLiteral(fn(i), fn(n))

    case Composition(f, g) => Composition(fe(f), fe(g))
  }

  def mapChildren[OE](fc: E => OE): Node[OE, N, DT, A] =
    map(fc, n => n, dt => dt, a => a)
  def children(): Iterator[E] = this match {
    case Var(_) | Literal(_) | NatLiteral(_) | IndexLiteral(_, _) |
     Primitive(_) => Iterator()
    case App(f, e) => Iterator(f, e)
    case Lambda(e) => Iterator(e)
    case NatApp(f, _) => Iterator(f)
    case NatLambda(e) => Iterator(e)
    case DataApp(f, _) => Iterator(f)
    case DataLambda(e) => Iterator(e)
    case AddrApp(f, _) => Iterator(f)
    case AddrLambda(e) => Iterator(e)

    case Composition(f, g) => Iterator(f, g)
  }
  def childrenCount(): Int =
    children().length

  def nats(): Iterator[N] = this match {
    case NatApp(_, n) => Iterator(n)
    case NatLiteral(n) => Iterator(n)
    case IndexLiteral(i, n) => Iterator(i, n)
    case _ => Iterator()
  }
  def natsCount(): Int = nats().length

  def dataTypes(): Iterator[DT] = this match {
    case DataApp(_, dt) => Iterator(dt)
    case _ => Iterator()
  }
  def dataTypesCount(): Int = dataTypes().length

  def addresses(): Iterator[A] = this match {
    case AddrApp(_, a) => Iterator(a)
    case _ => Iterator()
  }
  def addressCount(): Int = addresses().length

  def matchHash(): Int = this match {
    case Var(i) => 13 * i
    case App(_, _) => 1
    case Lambda(_) => 2
    case NatApp(_, _) => 3
    case NatLambda(_) => 4
    case DataApp(_, _) => 5
    case DataLambda(_) => 6
    case AddrApp(_, _) => 7
    case AddrLambda(_) => 8
    case Literal(d) => 17 * d.hashCode()
    case NatLiteral(n) => 23 * n.hashCode()
    case IndexLiteral(i, n) => 29 * (i, n).hashCode()
    case Primitive(p) => 19 * p.setType(rct.TypePlaceholder).hashCode()

    case Composition(_, _) => 9
  }

  // Returns true if this enode matches another enode.
  // This should only consider the operator, not the children ids or nats.
  def matches(other: Node[_, _, _, _]): Boolean = (this, other) match {
    case (Var(i1), Var(i2)) => i1 == i2
    case (App(_, _), App(_, _)) => true
    case (Lambda(_), Lambda(_)) => true
    case (NatApp(_, _), NatApp(_, _)) => true
    case (DataApp(_, _), DataApp(_, _)) => true
    case (AddrApp(_, _), AddrApp(_, _)) => true
    case (NatLambda(_), NatLambda(_)) => true
    case (DataLambda(_), DataLambda(_)) => true
    case (AddrLambda(_), AddrLambda(_)) => true
    case (Literal(d1), Literal(d2)) => d1 == d2
    case (NatLiteral(n1), NatLiteral(n2)) => n1 == n2
    case (IndexLiteral(i1, n1), IndexLiteral(i2, n2)) => i1 == i2 && n1 == n2
    case (Primitive(p1), Primitive(p2)) =>
      // TODO: type should not be inside the primitive?
      p1.setType(rct.TypePlaceholder) == p2.setType(rct.TypePlaceholder)

    case (Composition(_, _), Composition(_, _)) => true
    case _ => false
  }
}

case class Var(index: Int) extends Node[Nothing, Nothing, Nothing, Nothing] {
  override def toString: String = s"%$index"
}
case class App[E](f: E, e: E) extends Node[E, Nothing, Nothing, Nothing]
case class Lambda[E](e: E) extends Node[E, Nothing, Nothing, Nothing]
case class NatApp[E, N](f: E, x: N) extends Node[E, N, Nothing, Nothing]
case class DataApp[E, DT](f: E, x: DT) extends Node[E, Nothing, DT, Nothing]
case class AddrApp[E, A](f: E, x: A) extends Node[E, Nothing, Nothing, A]
case class NatLambda[E](e: E) extends Node[E, Nothing, Nothing, Nothing]
case class DataLambda[E](e: E) extends Node[E, Nothing, Nothing, Nothing]
case class AddrLambda[E](e: E) extends Node[E, Nothing, Nothing, Nothing]
case class Literal(d: semantics.Data) extends Node[Nothing, Nothing, Nothing, Nothing] {
  override def toString: String = d.toString
}
case class NatLiteral[N](n: N) extends Node[Nothing, N, Nothing, Nothing]
case class IndexLiteral[N](i: N, n: N) extends Node[Nothing, N, Nothing, Nothing]
case class Primitive(p: rise.core.Primitive) extends Node[Nothing, Nothing, Nothing, Nothing] {
  override def toString: String = p.toString.trim
}

case class Composition[E](f: E, g: E) extends Node[E, Nothing, Nothing, Nothing] {
  override def toString: String = s"$f >> $g"
}

object Node {
  import math.Ordering.Implicits.seqOrdering

  def collect[T](n: Node[T, T, T, T]): Seq[T] = n match {
    case Var(_) => Seq()
    case App(f, e) => Seq(f, e)
    case Lambda(e) => Seq(e)
    case NatApp(f, x) => Seq(f, x)
    case DataApp(f, x) => Seq(f, x)
    case AddrApp(f, x) => Seq(f, x)
    case NatLambda(e) => Seq(e)
    case DataLambda(e) => Seq(e)
    case AddrLambda(e) => Seq(e)
    case Literal(_) => Seq()
    case NatLiteral(n) => Seq(n)
    case IndexLiteral(i, n) => Seq(i, n)
    case Primitive(_) => Seq()

    case Composition(f, g) => Seq(f, g)
  }

  implicit val natIdOrdering: Ordering[NatId] = new Ordering[NatId] {
    def compare(id1: NatId, id2: NatId): Int =
      id1.i compare id2.i
  }
/*
  implicit val natOrdering: Ordering[NatNode[NatId]] = new Ordering[NatNode[NatId]] {
    def compare(n1: NatNode[NatId], n2: NatNode[NatId]): Int = {
      implicit val ord: Ordering[Nat] = this
      (n1, n2) match {
        case (NatCst(c1), NatCst(c2)) => c1 compare c2
        /* case (Sum(ts1), Sum(ts2)) =>
          implicitly[Ordering[Seq[Nat]]].compare(ts1, ts2)
        case (Prod(fs1), Prod(fs2)) =>
          implicitly[Ordering[Seq[Nat]]].compare(fs1, fs2) */
        case _ => ???
      }
    }
  }
*/
  implicit val dataTypeIdOrdering: Ordering[DataTypeId] = new Ordering[DataTypeId] {
    def compare(id1: DataTypeId, id2: DataTypeId): Int =
      id1.i compare id2.i
  }
/*
  implicit val dataTypeOrdering: Ordering[DataTypeNode[NatId, DataTypeId]] =
    new Ordering[DataTypeNode[NatId, DataTypeId]] {
    def compare(dt1: DataTypeNode[NatId, DataTypeId], dt2: DataTypeNode[NatId, DataTypeId]): Int =
      ???
  }
*/
  implicit val scalarDataOrdering: Ordering[semantics.ScalarData] =
    new Ordering[semantics.ScalarData] {
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

  // TODO: hash-cons literals data?
  implicit val dataOrdering: Ordering[semantics.Data] = new Ordering[semantics.Data] {
    import semantics._

    def compare(d1: Data, d2: Data): Int =
      (d1, d2) match {
        // case (NatData(n1), NatData(n2)) => ??? // natOrdering.compare(n1, n2)
        case (NatData(_), _) | (_, NatData(_)) => throw new Exception("should not compare 'NatData'")
        case (IndexData(i1, n1), IndexData(i2, n2)) => ???
          // implicitly[Ordering[(Nat, Nat)]].compare((i1, n1), (i2, n2))
        case (IndexData(_, _), _) | (_, IndexData(_, _)) => throw new Exception("should not compare 'IndexData'")
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

  implicit val addressOrdering: Ordering[Address] = new Ordering[Address] {
    def compare(id1: Address, id2: Address): Int = {
      (id1, id2) match {
        case (AddressVar(i1), AddressVar(i2)) => i1 compare i2
        case (AddressVar(_), _) => -1
        case (_, AddressVar(_)) => 1
        case (Global, Global) => 0
        case (Global, _) => -1
        case (_, Global) => 1
        case (Local, Local) => 0
        case (Local, _) => -1
        case (_, Local) => 1
        case (Private, Private) => 0
        case (Private, _) => -1
        case (_, Private) => 1
        case (Constant, Constant) => 0
      }
    }
  }

  implicit val eclassIdOrdering: Ordering[EClassId] = new Ordering[EClassId] {
    def compare(id1: EClassId, id2: EClassId): Int =
      id1.i compare id2.i
  }

  implicit def ordering[E, N, DT, A](implicit
                                     eOrd: Ordering[E],
                                     nOrd: Ordering[N],
                                     dtOrd: Ordering[DT],
                                     aOrd: Ordering[A],
                                    ): Ordering[Node[E, N, DT, A]] = new Ordering[Node[E, N, DT, A]] {
    def compare(n1: Node[E, N, DT, A], n2: Node[E, N, DT, A]): Int =
      (n1, n2) match {
        case (Var(i1), Var(i2)) => i1 compare i2
        case (Var(_), _) => -1
        case (_, Var(_)) => 1
        case (App(f1, e1), App(f2, e2)) =>
          implicitly[Ordering[(E, E)]].compare((f1, e1), (f2, e2))
        case (App(_, _), _) => -1
        case (_, App(_, _)) => 1
        case (Lambda(e1), Lambda(e2)) => eOrd.compare(e1, e2)
        case (Lambda(_), _) => -1
        case (_, Lambda(_)) => 1
        case (NatApp(f1, x1), NatApp(f2, x2)) =>
          implicitly[Ordering[(E, N)]].compare((f1, x1), (f2, x2))
        case (NatApp(_, _), _) => -1
        case (_, NatApp(_, _)) => 1
        case (DataApp(f1, x1), DataApp(f2, x2)) =>
          implicitly[Ordering[(E, DT)]].compare((f1, x1), (f2, x2))
        case (DataApp(_, _), _) => -1
        case (_, DataApp(_, _)) => 1
        case (AddrApp(f1, x1), AddrApp(f2, x2)) =>
          implicitly[Ordering[(E, A)]].compare((f1, x1), (f2, x2))
        case (AddrApp(_, _), _) => -1
        case (_, AddrApp(_, _)) => 1
        case (NatLambda(e1), NatLambda(e2)) => eOrd.compare(e1, e2)
        case (NatLambda(_), _) => -1
        case (_, NatLambda(_)) => 1
        case (DataLambda(e1), DataLambda(e2)) => eOrd.compare(e1, e2)
        case (DataLambda(_), _) => -1
        case (_, DataLambda(_)) => 1
        case (AddrLambda(e1), AddrLambda(e2)) => eOrd.compare(e1, e2)
        case (AddrLambda(_), _) => -1
        case (_, AddrLambda(_)) => 1
        case (Literal(d1), Literal(d2)) => dataOrdering.compare(d1, d2)
        case (Literal(_), _) => -1
        case (_, Literal(_)) => 1
        case (NatLiteral(n1), NatLiteral(n2)) => nOrd.compare(n1, n2)
        case (NatLiteral(_), _) => -1
        case (_, NatLiteral(_)) => 1
        case (IndexLiteral(i1, n1), IndexLiteral(i2, n2)) =>
          implicitly[Ordering[(N, N)]].compare((i1, n1), (i2, n2))
        case (IndexLiteral(_, _), _) => -1
        case (_, IndexLiteral(_, _)) => 1

        case (Composition(f1, g1), Composition(f2, g2)) =>
          implicitly[Ordering[(E, E)]].compare((f1, g1), (f2, g2))
        case (Composition(_, _), _) => -1
        case (_, Composition(_, _)) => 1

        // FIXME: does not work for mapGlobal(dim)
        case (Primitive(p1), Primitive(p2)) => p1.name compare p2.name
        case _ => ???
      }
  }
}