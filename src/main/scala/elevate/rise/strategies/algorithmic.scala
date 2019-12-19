package elevate.rise.strategies

import com.github.ghik.silencer.silent
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import elevate.rise.Rise
import lift.core.TypedDSL._
import lift.core._

object algorithmic {
  // TODO: only compose simpler rules
  // TODO: what if 'x' is used in 'f'?

  // fission of the first function to be applied inside a map
  // *(g >> .. >> f) -> *g >> *(.. >> f)
  case object mapFirstFission extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(primitives.Map(), Lambda(x, gx)) => Success(mapFirstFissionRec(x, fun(e => e), gx) :: e.t)
      case _                                    => Failure(mapFirstFission)
    }

    // TODO: this should be expressed with elevate strategies
    @silent
    @scala.annotation.tailrec
    private def mapFirstFissionRec(x: Identifier, f: TDSL[Rise], gx: Rise): TDSL[Rise] = {
      gx match {
        case App(f2, gx2) =>
          if (gx2 == x) {
            map(f2) >> map(f)
          } else {
            mapFirstFissionRec(x, fun(e => f(typed(f2)(e))), gx2)
          }
      }
    }
  }

  // fission of all the functions chained inside a map
  // *(g >> .. >> f) -> *g >> .. >> *f
  case object mapFullFission extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(primitives.Map(), Lambda(x, gx)) => Success(mapFullFissionRec(x, gx) :: e.t)
      case _                                    => Failure(mapFullFission)
    }

    // TODO: this should be expressed with elevate strategies
    @silent
    def mapFullFissionRec(x: Identifier, gx: Rise): TDSL[Rise] = {
      gx match {
        case App(f, gx2) =>
          if (gx2 == x) {
            map(f)
          } else {
            mapFullFissionRec(x, gx2) >> map(f)
          }
      }
    }
  }
}
