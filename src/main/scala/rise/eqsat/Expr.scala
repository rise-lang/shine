package rise.eqsat

import rise.core.{primitives => rcp}
import rise.core.semantics

case class Expr(node: Node[Expr])

object ExprDSL {
  def %(index: Int): Expr = Expr(Var(index))
  def app(a: Expr, b: Expr): Expr = Expr(App(a, b))
  def mul: Expr = Expr(Primitive(rcp.mul.primitive))
  def div: Expr = Expr(Primitive(rcp.div.primitive))
  def l(d: semantics.Data): Expr = Expr(Literal(d))
}