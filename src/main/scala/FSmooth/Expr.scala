package FSmooth

sealed trait Expr

final case class Identifier(name: String) extends Expr {
  override def toString: String = name
}

final case class Abstraction(xs: Seq[Identifier], e: Expr) extends Expr {
  override def toString: String = s"fun ${xs.mkString(" ")} -> $e"

  override def equals(obj: Any): Boolean = obj match {
    case other: Abstraction => e == lifting.liftFunExpr(other).value(xs)
    case _ => false
  }
}

final case class Application(f: Expr, es: Seq[Expr]) extends Expr {
  override def toString: String = s"($f ${es.mkString(" ")})"
}

final case class ScalarValue(n: Double) extends Expr

final case class IndexValue(i: Int) extends Expr

final case class CardinalityValue(N: Int) extends Expr

abstract class Constants extends Expr {
  def t: Type
}

final case class Let(x: Identifier, init: Expr, e: Expr) extends Expr {
  override def toString: String = s"let $x = $init in $e"
}

final case class Conditional(cond: Expr, thenBranch: Expr, elseBranch: Expr) extends Expr {
  override def toString: String = s"if $cond then $thenBranch else $elseBranch"
}