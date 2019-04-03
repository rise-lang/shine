package elevate.core.strategies

import lift.core._
import lift.core.DSL._

import elevate.core._
import elevate.core.rules._
import elevate.core.rules.algorithmic._

object algorithmic {
  // TODO: only compose simpler rules
  def mapInnerFission: Strategy = {
    case Apply(primitives.map, Lambda(x, gx)) =>
      mapInnerFissionRec(x, fun(e => e), gx)
  }

  // TODO: what if 'x' is used in 'f'?
  def mapInnerFissionRec(x: Identifier, f: Expr, gx: Expr): Expr = {
    gx match {
      case Apply(f2, gx2) =>
        if (gx2 == x) {
          primitives.map(f2) >> primitives.map(f)
        } else {
          mapInnerFissionRec(x, fun(e => f(f2(e))), gx2)
        }
    }
  }
/*
  def mapInnerFission: Strategy =
    peek(e => println(s"start: $e")) `;`
      mapFissionDive(genMapInnerFission(0)) `;`
        peek(e => println(s"end: $e"))

  private def mapFissionDive(s: Strategy): Strategy = e => {
    mapFission(e) match {
      case Lambda(x1, Apply(f, Apply(inner, x2))) if x1 == x2 =>
        println(s"inner: $inner")
        Lambda(x1, Apply(f, Apply(s(inner), x2)))
    }
  }

  private def genMapInnerFission(n: Int): Strategy = {
    mapFissionDive({ genMapInnerFission(n + 1)(_) }) +> { e =>
      println(s"end: $n: $e")
      mapFusion(e)
    }
  }
  */
}