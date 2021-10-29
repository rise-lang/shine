package rise.core

import util.monads._
import arithexpr.arithmetic.NamedVar
import rise.core.traverse._
import rise.core.types._

object IsClosedForm {
  case class Visitor(boundV: Set[Identifier], boundT: Set[Kind.Identifier])
    extends PureAccumulatorTraversal[(Set[Identifier], Set[Kind.Identifier])]
  {
    override val accumulator = PairMonoid(SetMonoid, SetMonoid)

    override def identifier[I <: Identifier]: VarType => I => Pair[I] = vt => i => {
      for { t2 <- `type`(i.t);
            i2 <- if (vt == Reference && !boundV(i)) {
                    accumulate((Set(i), Set()))(i)
                  } else {
                    return_(i)
                  }}
        yield i2
    }

    override def typeIdentifier[I <: Kind.Identifier]: VarType => I => Pair[I] = {
      case Reference => i =>
        if (boundT(i)) return_(i) else accumulate((Set(), Set(i)))(i)
      case _ => return_
    }

    override def addressSpace: AddressSpace => Pair[AddressSpace] = {
      // FIXME: this should be dispatched to `typeIdentifier` ???
      case i: AddressSpaceIdentifier =>
        if (boundT(i)) return_(i: AddressSpace) else accumulate((Set(), Set(i)))(i: AddressSpace)
      case x => return_(x)
    }

    override def nat: Nat => Pair[Nat] = n => {
      val free = n.varList.foldLeft(Set[Kind.Identifier]()) {
        case (free, v: NamedVar) if !boundT(NatIdentifier(v)) => free + NatIdentifier(v)
        case (free, _) => free
      }
      accumulate((Set(), free))(n)
    }

    override def expr: Expr => Pair[Expr] = {
      case Lambda(x, b) => this.copy(boundV = boundV + x).expr(b)
      case DepLambda(x, b) => this.copy(boundT = boundT + x).expr(b)
      case e => super.expr(e)
    }

    override def natToData: NatToData => Pair[NatToData] = {
      case NatToDataLambda(x, e) =>
        for { p <- this.copy(boundT = boundT + x).`type`(e) }
          yield (p._1, NatToDataLambda(x, e))
      case t => super.natToData(t)
    }

    override def natToNat: NatToNat => Pair[NatToNat] = {
      case NatToNatLambda(x, n) =>
        for { p <- this.copy(boundT = boundT + x).nat(n) }
          yield (p._1, NatToNatLambda(x, n))
      case n => super.natToNat(n)
    }

    override def `type`[T <: Type]: T => Pair[T] = {
      case d@DepFunType(x, t) =>
        for { p <- this.copy(boundT = boundT + x).`type`(t) }
          yield (p._1, d.asInstanceOf[T])
      case d@DepPairType(x, dt) =>
        for { p <- this.copy(boundT = boundT + x).datatype(dt) }
          yield (p._1, d.asInstanceOf[T])
      case t => super.`type`(t)
    }
  }

  def freeVars(expr: Expr): (Set[Identifier], Set[Kind.Identifier]) =
    traverse(expr, Visitor(Set(), Set()))._1

  def apply(expr: Expr): Boolean = {
    val (freeV, freeT) = freeVars(expr)
    freeV.isEmpty && freeT.isEmpty
  }
}