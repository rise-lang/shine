package rise.core.types

import arithexpr.arithmetic.{ArithExprFunctionCall, SimplifiedExpr}
import rise.core.Expr


private final class NatCollectionIndexing(collection: NatCollection, idxs: Seq[Nat])
  extends ArithExprFunctionCall(s"($collection)#[${idxs.map(_.toString).mkString(",")}]") {
  override lazy val toString: String = this.name

  override def exposedArgs: Seq[Nat] = idxs

  override def substituteExposedArgs(subMap: Map[Nat, SimplifiedExpr]): ArithExprFunctionCall =
    new NatCollectionIndexing(collection, idxs.map(idx => subMap.getOrElse(idx, idx)))

  override def visitAndRebuild(f: Nat => Nat): Nat =
    new NatCollectionIndexing(collection, idxs.map(idx => f(idx)))

  override def substitute(subs: scala.collection.Map[Nat, Nat]): Option[Nat] = {
    Some(visitAndRebuild(x => subs.getOrElse(x, x.substitute(subs).getOrElse(x))))
  }

}

sealed abstract class NatCollection {
  def apply(idxs: Nat*): Nat = new NatCollectionIndexing(this, idxs)
}

final case class NatCollectionIdentifier(name: String) extends NatCollection with Kind.Identifier {
  override def toString : String = name
}

/**
  * Represents an n-dimensional array of natural number, which is indexable at the
  * type level with the # operator, but whose value is runtime dependent.
  * It's implemented as a wrapper around the expression which computes the runtime
  * value as an array of a suitable element type (say, natType)
  * */
final case class NatCollectionFromArray(expr: Expr)
    extends NatCollection {
  override def toString: String = expr.toString
}

