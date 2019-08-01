package elevate.lift.strategies

import elevate.core.{Failure, Strategy, Success}
import lift.core.DSL._
import lift.core._

object algorithmic {
  // TODO: only compose simpler rules
  // TODO: what if 'x' is used in 'f'?

  // fission of the first function to be applied inside a map
  // *(g >> .. >> f) -> *g >> *(.. >> f)
  def mapFirstFission: Strategy = {
    case Apply(primitives.map, Lambda(x, Apply(f, y))) =>
      Success(mapFirstFissionRec(x, fun(e => e), f, y))
    case _ => Failure(mapFirstFission)
  }

  @scala.annotation.tailrec
  private def mapFirstFissionRec(x: Identifier, f: Expr, g: Expr, y: Expr): Expr = {
    if (y == x) {
      primitives.map(g) >> primitives.map(f)
    } else {
      mapFirstFissionRec(x, fun(e => f(g(e))), g, y)
    }
  }

  // fission of all the functions chained inside a map
  // *(g >> .. >> f) -> *g >> .. >> *f
  def mapFullFission: Strategy = {
    case Apply(primitives.map, Lambda(x, Apply(f, y))) =>
      Success(mapFullFissionRec(x, f, y))
    case _ => Failure(mapFullFission)
  }

  def mapFullFissionRec(x: Identifier, f: Expr, y: Expr): Expr = {
    if (y == x) {
      primitives.map(f)
    } else {
      mapFullFissionRec(x, f, y) >> primitives.map(f)
    }
  }
}
