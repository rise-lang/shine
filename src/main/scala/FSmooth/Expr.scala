package FSmooth

import FSmooth.DSL.freshTypeVar

abstract class Expr(val t: Type)

final case class Identifier(name: String,
                            override val t: Type = freshTypeVar) extends Expr(t) {
  override def toString: String = name
}

final case class Abstraction(params: Seq[Identifier], body: Expr,
                             override val t: Type = freshTypeVar) extends Expr(t) {
  override def toString: String = t match {
    //case FunType(inT, outT) => s"fun (${params.mkString(" ")} : $inT) -> ($body): $outT"
    case _ => s"fun ${params.mkString(" ")} -> \n$body"
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: Abstraction => body == lifting.liftFunExpr(other).value(params)
    case _ => false
  }
}

final case class Application(fun: Expr, args: Seq[Expr],
                             override val t: Type = freshTypeVar) extends Expr(t) {
  override def toString: String = s"($fun (${args.mkString(") (")}))"
}

final case class Let(x: Identifier, value: Expr, body: Expr,
                     override val t: Type = freshTypeVar) extends Expr(t) {
  override def toString: String = s"let $x = $value in\n $body"
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
  def copyWithType(t: Type): Constants
}