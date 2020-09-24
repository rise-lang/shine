package rise.elevate.strategies

import elevate.core.strategies.{Traversable, basic}
import elevate.core.strategies.basic.{applyNTimes, id}
import elevate.core.strategies.traversal._
import rise.elevate.strategies.traversal._

import scala.annotation.nowarn
//import rise.elevate.rules.traversal.default._
import elevate.core.{Failure, Strategy, Success}
import elevate.macros.StrategyMacro.strategy
import rise.elevate.Rise
import rise.elevate.rules.algorithmic.fuseReduceMap
import rise.elevate.rules.movement._
import rise.elevate.rules.traversal.{argument, argumentOf, body, function}
import rise.elevate.strategies.normalForm.{DFNF, RNF}
import rise.elevate.strategies.predicate.{isApplied, isMap, isReduceSeq}
import rise.elevate.strategies.traversal.fmap
import rise.core.TypedDSL._
import rise.core._
import rise.core.primitives.{map, reduceSeq}
import rise.elevate._

object algorithmic {
  // TODO: only compose simpler rules
  // TODO: what if 'x' is used in 'f'?

  // fission of the first function to be applied inside a map
  // *(g >> .. >> f) -> *g >> *(.. >> f)
  @strategy def mapFirstFission: Strategy[Rise] = e => {
    // TODO: this should be expressed with elevate strategies
    @nowarn("msg=match may not be exhaustive")
    @scala.annotation.tailrec
    def mapFirstFissionRec(x: Identifier, f: ToBeTyped[Rise], gx: Rise): ToBeTyped[Rise] = {
      gx match {
        case App(f2, gx2) =>
          if (gx2 == x) {
            map(f2) >> map(f)
          } else {
            mapFirstFissionRec(x, fun(e => f(preserveType(f2)(e))), gx2)
          }
      }
    }

    e match {
      case App(primitives.map(), Lambda(x, gx)) => Success(mapFirstFissionRec(x, fun(e => e), gx) :: e.t)
      case _                                    => Failure(mapFirstFission)
    }
  }

  // fission of all the functions chained inside a map
  // *(g >> .. >> f) -> *g >> .. >> *f
  @strategy def mapFullFission: Strategy[Rise] = e => {
    // TODO: this should be expressed with elevate strategies
    @nowarn("msg=match may not be exhaustive")
    def mapFullFissionRec(x: Identifier, gx: Rise): ToBeTyped[Rise] = {
      gx match {
        case App(f, gx2) =>
          if (gx2 == x) {
            map(f)
          } else {
            mapFullFissionRec(x, gx2) >> map(f)
          }
      }
    }

    e match {
      case App(primitives.map(), Lambda(x, gx)) => Success(mapFullFissionRec(x, gx) :: e.t)
      case _                                    => Failure(mapFullFission)
    }
  }

  //scalastyle:off
  def normForReorder(implicit ev: Traversable[Rise]): Strategy[Rise] =
    (splitBeforeMap `@` topDown[Rise]) `;;`
    (fuseReduceMap `@` topDown[Rise]) `;;`
    (fuseReduceMap `@` topDown[Rise]) `;;` RNF()

  @strategy def reorder(l: List[Int])(implicit ev: Traversable[Rise]): Strategy[Rise] = normForReorder `;` (reorderRec(l) `@` topDown[Rise])

  @strategy def reorderRec(l: List[Int])(implicit ev: Traversable[Rise]): Strategy[Rise] = e => {

    def freduce(s: Strategy[Rise]): Strategy[Rise] =
      function(function(argumentOf(reduceSeq.primitive, body(body(s)))))
    def freduceX(s: Strategy[Rise]): Strategy[Rise] =
      argument(function(function(argumentOf(reduceSeq.primitive, body(body(s))))))
    def stepDown(s: Strategy[Rise]): Strategy[Rise] = freduceX(s) <+ freduce(s) <+ fmap(s)

    val isFullyAppliedReduceSeq: Strategy[Rise] = isApplied(isApplied(isApplied(isReduceSeq))) <+
      argument(isApplied(isApplied(isApplied(isReduceSeq)))) // weird reduce special case
    val isFullyAppliedMap: Strategy[Rise] = isApplied(isApplied(isMap))

    // pos = how far nested is the reduction?
    def moveReductionUp(pos: Int): Strategy[Rise] = {
      if (pos <= 1) id
      else
        applyNTimes(pos-2)(stepDown)(function(liftReduce)) `;` DFNF() `;` RNF() `;`  moveReductionUp(pos-1)
    }

    l match {
      // nothing to reorder, go further down
      case x :: xs if x == 1 => (stepDown(reorderRec(xs.map(y => if (y>x) y-1 else y ))))(e)
      // work to do
      case pos :: xs => (
        (applyNTimes(pos-1)(stepDown)(isFullyAppliedReduceSeq) `;`
          // move reduction and normalize the AST
          moveReductionUp(pos) `;`
          // recurse and continue reordering
          stepDown(reorderRec(xs.map(y => if (y>pos) y-1 else y )))) <+
          // other case: is it a map?
          applyNTimes(pos-1)(stepDown)(isFullyAppliedMap) `;`
          basic.fail
        )(e)
      case Nil => id(e)
      case _ => Failure(reorderRec(l))
    }
  }
}
