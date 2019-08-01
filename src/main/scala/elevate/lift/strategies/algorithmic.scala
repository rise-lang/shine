package elevate.lift.strategies

import elevate.core.{Failure, Strategy, Success}
import lift.core.DSL._
import lift.core._

object algorithmic {
  // TODO: only compose simpler rules
  // TODO: what if 'x' is used in 'f'?

  /*
  // fission of the first function to be applied inside a map
  // *(g >> .. >> f) -> *g >> *(.. >> f)
  def mapFirstFission: Strategy = {
    case Apply(primitives.map, Lambda(x, gx)) =>
      Success(mapFirstFissionRec(x, fun(e => e), gx))
    case _ => Failure(mapFirstFission)
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
      Success(mapFullFissionRec(x, gx))
    case _ => Failure(mapFullFission)
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

   */
}
