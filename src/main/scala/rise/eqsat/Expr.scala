package rise.eqsat

import rise.core
import rise.core.{primitives => rcp}
import rise.core.semantics

// TODO: could also be outside of eqsat package
case class Expr(node: Node[Expr]) {
  // shifts De-Bruijn indices up or down if they are >= cutoff
  def shifted(up: Boolean, cutoff: Int): Expr = {
    Expr(node match {
      case Var(index) =>
        val newIndex = if (index >= cutoff) {
          if (up) { index + 1 } else { index - 1 }
        } else {
          index
        }
        Var(newIndex)
      case Lambda(e) => Lambda(e.shifted(up, cutoff + 1))
      case App(f, e) => App(f.shifted(up, cutoff), e.shifted(up, cutoff))
      case DepLambda(_, _) => ???
      case DepApp(f, x) => DepApp(f.shifted(up, cutoff), x)
      case Literal(_) | Primitive(_) => node
    })
  }

  def replace(index: Int, subs: Expr): Expr = {
    node match {
      case Var(idx) if (idx == index) => subs
      case Var(_) => this
      case Lambda(e) =>
        val e2 = e.replace(index + 1, subs.shifted(true, 0))
        Expr(Lambda(e2))
      case App(f, e) =>
        val f2 = f.replace(index, subs)
        val e2 = e.replace(index, subs)
        Expr(App(f2, e2))
      case DepLambda(_, _) => ???
      case DepApp(f, x) =>
        val f2 = f.replace(index, subs)
        Expr(DepApp(f2, x))
      case Literal(_) | Primitive(_) => this
    }
  }

  // substitutes %0 for arg in this
  def withArgument(arg: Expr): Expr = {
    replace(0, arg.shifted(true, 0))
      .shifted(false, 0)
  }
}

object Expr {
  def from(e: core.Expr): Expr = {
    def rec(e: core.Expr, bound: Seq[core.Identifier]): Expr = {
      Expr(e match {
        case i: core.Identifier => Var(bound.indexOf(i))
        case core.App(f, e) => App(rec(f, bound), rec(e, bound))
        case core.Lambda(i, e) => Lambda(rec(e, i +: bound))
        case core.DepApp(f, x) => DepApp(rec(f, bound), x)
        case core.DepLambda(_, _) => ???
        case core.Literal(d) => Literal(d)
        case p: core.Primitive => Primitive(p.setType(core.types.TypePlaceholder))
      })
    }

    rec(e, Seq())
  }
}

object ExprDSL {
  def %(index: Int): Expr = Expr(Var(index))
  def app(a: Expr, b: Expr): Expr = Expr(App(a, b))
  def lam(e: Expr): Expr = Expr(Lambda(e))
  def map: Expr = Expr(Primitive(rcp.map.primitive))
  def add: Expr = Expr(Primitive(rcp.add.primitive))
  def mul: Expr = Expr(Primitive(rcp.mul.primitive))
  def div: Expr = Expr(Primitive(rcp.div.primitive))
  def l(d: semantics.Data): Expr = Expr(Literal(d))
}