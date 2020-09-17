package rise.elevate.strategies

import elevate.core.Strategy
import elevate.core.strategies.Traversable
import elevate.core.strategies.basic._
import rise.elevate.Rise
import rise.elevate.strategies.traversal._
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.tiling._

object halide {

  def reorder(perm: scala.collection.Seq[Int])(implicit ev: Traversable[Rise]): Strategy[Rise] = {

    def shiftDimension(i: Int): Strategy[Rise] = {
      i match {
        case 1 => loopInterchange
        case x => loopInterchangeAtLevel(ev)(x-1) `;` moveTowardsArgument(1)(shiftDimension(i-1))
      }
    }

    assert(perm.distinct.length == perm.length)
    assert(perm.forall(_ > 0 ))
    assert(perm.sum == (1 to perm.length).sum)

    if(perm.length == 1) return id

    // which dimension comes first?
    (perm.head match {
      case 1 => fmap(reorder(perm.tail.map(_-1)))
      case x =>
        val transposes = x-1
        shiftDimension(transposes) `;`
        moveTowardsArgument(transposes)(fmap(reorder(
          perm.tail.map(y => if(y > x) y-1 else y ))))
    }) `;` RNF() `;` DFNF() // normalize at the very end
  }

}
