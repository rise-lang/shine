package elevate.core.strategies

import lift.core._
import lift.core.DSL._

import elevate.core._
import elevate.core.rules._
import elevate.core.rules.algorithmic._

object algorithmic {
  // TODO: only compose simpler rules
  // TODO: what if 'x' is used in 'f'?

  // fission of the first function to be applied inside a map
  // *(g >> .. >> f) -> *g >> *(.. >> f)
  def mapFirstFission: Strategy = {
    case Apply(primitives.map, Lambda(x, gx)) =>
      mapFirstFissionRec(x, fun(e => e), gx)
  }

  private def mapFirstFissionRec(x: Identifier, f: Expr, gx: Expr): Expr = {
    gx match {
      case Apply(f2, gx2) =>
        if (gx2 == x) {
          primitives.map(f2) >> primitives.map(f)
        } else {
          mapFirstFissionRec(x, fun(e => f(f2(e))), gx2)
        }
    }
  }

  // fission of all the functions chained inside a map
  // *(g >> .. >> f) -> *g >> .. >> *f
  def mapFullFission: Strategy = {
    case Apply(primitives.map, Lambda(x, gx)) =>
      mapFullFissionRec(x, gx)
  }

  def mapFullFissionRec(x: Identifier, gx: Expr): Expr = {
    gx match {
      case Apply(f, gx2) =>
        if (gx2 == x) {
          primitives.map(f)
        } else {
          mapFullFissionRec(x, gx2) >> primitives.map(f)
        }
    }
  }
}