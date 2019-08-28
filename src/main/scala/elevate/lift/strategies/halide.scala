package elevate.lift.strategies

import elevate.core.{Elevate, Lift, Strategy}
import elevate.core.strategies.basic._
import elevate.lift.strategies.traversal._
import elevate.lift.strategies.normalForm._
import elevate.lift.strategies.tiling._

object halide {

  def reorder(perm: Seq[Int]): Strategy[Lift] = {

    def shiftDimension(i: Int): Strategy[Lift] = {
      i match {
        case 1 => loopInterchange
        //case x => shiftDimension(i-1) `;` moveTowardsArgument(i-1)(loopInterchangeAtLevel(x-1))
        case x => loopInterchangeAtLevel(x-1) `;` moveTowardsArgument(1)(shiftDimension(i-1))
      }
    }

    assert(perm.distinct.length == perm.length)
    assert(perm.forall(_ > 0 ))
    assert(perm.sum == (1 to perm.length).sum)

    if(perm.length == 1) return id()

    // which dimension comes first?
    (perm.head match {
      case 1 => fmap(reorder(perm.tail.map(_-1)))
      case x =>
        val transposes = x-1
        shiftDimension(transposes) `;`
        moveTowardsArgument(transposes)(fmap(reorder(
          perm.tail.map(y => if(y > x) y-1 else y ))))
    }) `;` RNF `;` LCNF // normalize at the very end
  }

}
