package rise.core.types

import arithexpr.arithmetic.{ArithExpr, ArithExprFunctionCall, SimplifiedExpr, Var}


final class NatCollectionIndexing(val collection: NatCollection, val idxs: Seq[Nat])
  extends ArithExprFunctionCall(s"($collection)[${idxs.map(_.toString).mkString(",")}]") {
  override lazy val toString: String = this.name

  override def exposedArgs: Seq[Nat] = idxs

  override def substituteExposedArgs(subMap: Map[Nat, SimplifiedExpr]): ArithExprFunctionCall =
    new NatCollectionIndexing(collection, idxs.map(idx => subMap.getOrElse(idx, idx)))

  override def visitAndRebuild(f: Nat => Nat): Nat =
    f(new NatCollectionIndexing(collection, idxs.map(idx => f(idx))))

  override def substitute(subs: scala.collection.Map[Nat, Nat]): Option[Nat] = {
    Some(new NatCollectionIndexing(this.collection, idxs.map(n => ArithExpr.substitute(n, subs))))
  }

  override def freeVariables: Set[Var] = idxs.map(n => ArithExpr.freeVariables(n)).foldLeft(Set[Var]())(_.union(_))
}

object NatCollectionIndexing {
    def apply(ns: NatCollection, idxs:Seq[Nat]): NatCollectionIndexing = new NatCollectionIndexing(ns, idxs)
    def unapply(arg: NatCollectionIndexing): Option[(NatCollection, Seq[Nat])] =
      Some((arg.collection, arg.idxs))
}

sealed abstract class NatCollection {
  def `@`(idxs: Nat*): Nat = new NatCollectionIndexing(this, idxs)
}

final case class NatCollectionIdentifier(
    name: String,
   override val isExplicit: Boolean = false
 ) extends NatCollection
  with Kind.Identifier
  with Kind.Explicitness {
  override def toString: String = if (isExplicit) name else "_" + name
  override def asExplicit: NatCollectionIdentifier =
      this.copy(isExplicit = true)
  override def asImplicit: NatCollectionIdentifier =
    this.copy(isExplicit = false)
  override def equals(that: Any): Boolean = that match {
    case ident: NatCollectionIdentifier => this.name == ident.name
    case _                        => false
  }
  override def hashCode(): Int = this.name.hashCode()
}