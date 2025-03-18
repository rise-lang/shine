package rise.core

import util.monads._
import arithexpr.arithmetic.NamedVar
import rise.core.traverse._
import rise.core.types._
import rise.core.types.DataType._

object IsClosedForm {
  case class OrderedSet[T](seq : Seq[T], set : Set[T])
  object OrderedSet {
    def empty[T] : OrderedSet[T] = OrderedSet(Seq(), Set())
    def add[T] : T => OrderedSet[T] => OrderedSet[T] = t => ts =>
      if (ts.set.contains(t)) ts else OrderedSet(t +: ts.seq, ts.set + t)
    def one[T] : T => OrderedSet[T] = add(_)(empty)
    def append[T] : OrderedSet[T] => OrderedSet[T] => OrderedSet[T] = x => y => {
      val ordered = x.seq.filter(!y.set.contains(_)) ++ y.seq
      val unique = x.set ++ y.set
      OrderedSet(ordered, unique)
    }
  }
  implicit def OrderedSetMonoid[T] : Monoid[OrderedSet[T]] = new Monoid[OrderedSet[T]] {
    def empty : OrderedSet[T] = OrderedSet.empty
    def append : OrderedSet[T] => OrderedSet[T] => OrderedSet[T] = OrderedSet.append
  }

  case class Visitor(boundV: Set[Identifier], boundT: Set[Kind.Identifier])
    extends PureAccumulatorTraversal[(OrderedSet[Identifier], OrderedSet[Kind.Identifier])]
  {
    override val accumulator: Monoid[(OrderedSet[Identifier], OrderedSet[Kind.Identifier])] =
      PairMonoid(OrderedSetMonoid, OrderedSetMonoid)

    override def identifier[I <: Identifier]: VarType => I => Pair[I] = vt => i => {
      for { t2 <- `type`(i.t);
            i2 <- if (vt == Reference && !boundV(i)) {
                    accumulate((OrderedSet.one(i : Identifier), OrderedSet.empty))(i)
                  } else {
                    return_(i)
                  }}
        yield i2
    }

    override def typeIdentifier[I <: Kind.Identifier]: VarType => I => Pair[I] = {
      case Reference => i =>
        if (boundT(i)) return_(i) else accumulate((OrderedSet.empty, OrderedSet.one(i : Kind.Identifier)))(i)
      case _ => return_
    }

    override def addressSpace: AddressSpace => Pair[AddressSpace] = {
      // FIXME: unclear whether this is needed or not
      case i: AddressSpaceIdentifier =>
        val iw: Kind.Identifier = AddressSpaceKind.IDWrapper(i)
        if (boundT(iw)) return_(i: AddressSpace) else accumulate((OrderedSet.empty, OrderedSet.one(iw)))(i: AddressSpace)
      case x => return_(x)
    }

    override def nat: Nat => Pair[Nat] = n => {
      val free = n.varList.foldLeft(OrderedSet.empty[Kind.Identifier]) {
        case (free, v: NamedVar) if !boundT(NatKind.IDWrapper(v)) => OrderedSet.add(NatKind.IDWrapper(v) : Kind.Identifier)(free)
        case (free, _) => free
      }
      accumulate((OrderedSet.empty, free))(n)
    }

    override def expr: Expr => Pair[Expr] = {
      case l@Lambda(x, e) =>
        // The binder's type itself might contain free type variables
        val ((fVx, fTx), x1) = identifier(Binding)(x).unwrap
        val ((fVe, fTe), e1) = this.copy(boundV = boundV + x1).expr(e).unwrap
        val ((fVt, fTt), t1) = `type`(l.t).unwrap
        val fV = OrderedSet.append(OrderedSet.append(fVx)(fVe))(fVt)
        val fT = OrderedSet.append(OrderedSet.append(fTx)(fTe))(fTt)
        accumulate((fV, fT))(Lambda(x1, e1)(t1): Expr)
      case DepLambda(k, x, b) => this.copy(boundT = boundT + Kind.toIdentifier(k, x)).expr(b)
      case e => super.expr(e)
    }

    override def natToData: NatToData => Pair[NatToData] = {
      case NatToDataLambda(x, e) =>
//        for { p <- this.copy(boundT = boundT + NatKind.IDWrapper(x)).`type`(e)}
//          yield (p._1, NatToDataLambda(x, e))
        val pp: Pure[((OrderedSet[Identifier], OrderedSet[Kind.Identifier]), DataType)] =
          this.copy(boundT = boundT + NatKind.IDWrapper(x)).`type`(e)
        pp.map(p => (p._1, NatToDataLambda(x, e)))
      case t => super.natToData(t)
    }

    override def natToNat: NatToNat => Pair[NatToNat] = {
      case NatToNatLambda(x, n) =>
//        for { p <- this.copy(boundT = boundT + NatKind.IDWrapper(x)).nat(n)}
//          yield (p._1, NatToNatLambda(x, n))
        val pp: Pure[((OrderedSet[Identifier], OrderedSet[Kind.Identifier]), Nat)] =
          this.copy(boundT = boundT + NatKind.IDWrapper(x)).nat(n)
        pp.map(p => (p._1, NatToNatLambda(x, n)))
      case n => super.natToNat(n)
    }

    override def `type`[T <: ExprType]: T => Pair[T] = {
      case d@DepFunType(k, x, t) =>
//        for { p <- this.copy(boundT = boundT + Kind.toIdentifier(k, x)).`type`(t) }
//          yield (p._1, d.asInstanceOf[T])
        val pp: Pure[((OrderedSet[Identifier], OrderedSet[Kind.Identifier]), ExprType)] =
          this.copy(boundT = boundT + Kind.toIdentifier(k, x)).`type`(t)
        pp.map(p => (p._1, d.asInstanceOf[T]))
      case d@DepPairType(k, x, dt) =>
//        for { p <- this.copy(boundT = boundT + Kind.toIdentifier(k, x)).datatype(dt) }
//          yield (p._1, d.asInstanceOf[T])
        val pp: Pure[((OrderedSet[Identifier], OrderedSet[Kind.Identifier]), DataType)] =
          this.copy(boundT = boundT + Kind.toIdentifier(k, x)).datatype(dt)
        pp.map(p => (p._1, d.asInstanceOf[T]))
      case t => super.`type`(t)
    }
  }

  def freeVars(expr: Expr): (OrderedSet[Identifier], OrderedSet[Kind.Identifier]) = {
    val ((fV, fT), _) = traverse(expr, Visitor(Set(), Set()))
    (fV, fT)
  }

  def freeVars(t: ExprType): OrderedSet[Kind.Identifier] = {
    val ((_, ftv), _) = traverse(t, Visitor(Set(), Set()))
    ftv
  }

  // Exclude matrix layout and fragment kind identifiers, since they cannot currently be bound
  def needsClosing : Seq[Kind.Identifier] => Seq[Kind.Identifier] = _.flatMap {
    case MatrixLayoutKind.IDWrapper(i) => Seq()
    case FragmentKind.IDWrapper(i) => Seq()
    case e => Seq(e)
  }

  def varsToClose(expr : Expr): (Seq[Identifier], Seq[Kind.Identifier]) = {
    val (fV, fT) = freeVars(expr)
    (fV.seq, needsClosing(fT.seq))
  }

  def varsToClose(t : ExprType): Seq[Kind.Identifier] = needsClosing(freeVars(t).seq)

  def apply(expr: Expr): Boolean = {
    val (freeV, freeT) = varsToClose(expr)
    freeV.isEmpty && freeT.isEmpty
  }
}