package rise.eqsat

import rise.core.semantics
import rise.core.{primitives => rcp}

case class PatternVar(name: String)
case class Pattern(node: Either[Node[Pattern], PatternVar]) {
  def patternVars(): Vec[PatternVar] = {
    val vec = Vec.empty[PatternVar]
    def rec(n: Either[Node[Pattern], PatternVar]): Unit = {
      n match {
        case Left(node) => node.children().foreach { child =>
          rec(child.node)
        }
        case Right(pv) =>
          if (!vec.contains(pv)) { vec += pv }
      }
    }
    rec(node)
    vec
  }
}

object PatternDSL {
  def ?(name: String): Pattern = Pattern(Right(PatternVar(name)))
  def %(index: Int): Pattern = Pattern(Left(Var(index)))
  def app(a: Pattern, b: Pattern): Pattern = Pattern(Left(App(a, b)))
  def mul: Pattern = Pattern(Left(Primitive(rcp.mul.primitive)))
  def div: Pattern = Pattern(Left(Primitive(rcp.div.primitive)))
  def l(d: semantics.Data): Pattern = Pattern(Left(Literal(d)))
}