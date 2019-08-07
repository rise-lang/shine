package FSmooth

import FSmooth.DSL.freshTypeVar

abstract class Expr(val t: Type)

final case class Identifier(name: String,
                            override val t: Type = freshTypeVar) extends Expr(t) {
  override def toString: String = name
}

final case class Abstraction(xs: Seq[Identifier], e: Expr,
                             override val t: Type = freshTypeVar) extends Expr(t) {
  override def toString: String = t match {
    case FunType(inT, outT) => s"fun (${xs.mkString(" ")} : $inT) -> ($e): $outT"
    case _ => s"fun ${xs.mkString(" ")} -> $e"
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: Abstraction => e == lifting.liftFunExpr(other).value(xs)
    case _ => false
  }
}

final case class Application(f: Expr, es: Seq[Expr],
                             override val t: Type = freshTypeVar) extends Expr(t) {
  override def toString: String = s"($f (${es.mkString(") (")}))"
}

final case class Let(x: Identifier, init: Expr, e: Expr,
                     override val t: Type = freshTypeVar) extends Expr(t) {
  override def toString: String = s"let $x = $init in\n $e"
}

final case class Conditional(cond: Expr, thenBranch: Expr, elseBranch: Expr,
                             override val t: Type = freshTypeVar) extends Expr(t) {
  override def toString: String = s"if $cond then $thenBranch else $elseBranch"
}

final case class ScalarValue(n: Double) extends Expr(Double)

final case class IndexValue(i: Int) extends Expr(Index)

final case class CardinalityValue(N: Int) extends Expr(Card)

abstract class Constants(t: Type) extends Expr(t) {
  def typeScheme: Type
  def copy(t: Type): Constants
}