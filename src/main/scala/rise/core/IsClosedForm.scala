package rise.core

import util.monads._
import arithexpr.arithmetic.NamedVar
import rise.core.traverse._
import rise.core.types._

object IsClosedForm {
  case class Visitor(boundV: Set[Identifier], boundT: Set[Kind.Identifier]) extends Traversal[Option] {
    override implicit def monad = OptionMonad

    override def identifier[I <: Identifier] : VarType => I => Option[I] = {
      case Reference => i => if (boundV(i)) Some(i) else None
      case _ => return_
    }

    override def typeIdentifier[I <: Kind.Identifier]: VarType => I => Option[I] = {
      case Reference => i => if (boundT(i)) Some(i) else None
      case _ => return_
    }

    override def nat: Nat => Option[Nat] = n => {
      val closed = n.varList.foldLeft(true) {
        case (c, v: NamedVar) => c && boundT(NatIdentifier(v, isExplicit = false, isTuningParam = false))
        case (c, _) => c
      }
      if (closed) Some(n) else None
    }

    override def expr: Expr => Option[Expr] = {
      case Lambda(x, b) => this.copy(boundV = boundV + x).expr(b)
      case DepLambda(x, b) => this.copy(boundT = boundT + x).expr(b)
      case e => super.expr(e)
    }

    override def natToData : NatToData => Option[NatToData] = {
      case d@NatToDataLambda(x, e) =>
        for { _ <- this.copy(boundT = boundT + x).`type`(e) }
          yield d
      case t => super.natToData(t)
    }

    override def natToNat: NatToNat => Option[NatToNat] = {
      case d@NatToNatLambda(x, n) =>
        for { _ <- this.copy(boundT = boundT + x).nat(n) }
          yield d
      case n => super.natToNat(n)
    }

    override def `type`[T <: Type]: T => Option[T] = {
      case d@DepFunType(x, t) =>
        for { _ <- this.copy(boundT = boundT + x).`type`(t) }
          yield d.asInstanceOf[T]
      case d@DepPairType(x, dt) =>
        for { _ <- this.copy(boundT = boundT + x).datatype(dt) }
          yield d.asInstanceOf[T]
      case t => super.`type`(t)
    }
  }

  def apply(expr: Expr): Boolean = {
    traverse(expr, Visitor(Set(), Set())) match {
      case None => false
      case Some(e) => true
    }
  }
}