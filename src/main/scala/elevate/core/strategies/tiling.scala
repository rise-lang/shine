package elevate.core.strategies

import elevate.core._
import elevate.core.strategies.basic._
import elevate.lift.rules.algorithmic._
import elevate.lift.rules.movement._
import elevate.lift.strategies.traversal._
import elevate.lift.strategies.normalForm._

object tiling {

  def tileND: Int => Int => Strategy = d => n => tileNDList(d)(List.tabulate(d)(_ => n))

  def tileNDList: Int => List[Int] => Strategy =
    d => n => {
      assert(n.size == d) // check we have as many tile sizes as dimensions
      tileNDRec(d)(n)
    }

  def tileNDRec: Int => List[Int] => Strategy =
    d => n => d match {
        case x if x <= 0 => id
        // ((map f) arg)
        case 1 => function(splitJoin(n.head)) `;` LCNF `;` RNF
        case i => fmap(tileNDRec(d-1)(n.tail)) `;` tileNDRec(1)(n) `;` shiftDim(i)
      }


  // Notation: a.A -> a == tile dimension; A == original dimension
  // a.b.c.d: 4D array (inner => outer): a == innermost dim; d == outermost dim
  //
  // dim == 2 -> shift one level:
  //    a.A.b.B => a.b.A.B
  //    achieved by: (****f => *T o ****f o *T)
  //
  // dim == 3 -> shift two levels
  //    a.b.A.B.c.C => a.b.c.A.B.C
  //    (******f => *T o **T o ******f o **T o *T)
  // dim == 4 -> shift three levels ...
  def shiftDim: Int => Strategy =
    d => {
      val joins = d
      val transposes = (1 to d-2).sum
      LCNF `;` shiftDimRec(joins + transposes)(d-1)
    }

  // position: how far to move right until we reach maps
  // level:    how deep transpose pairs are nested in maps
  def shiftDimRec: Int => Int => Strategy =
    position => level => LCNF `;`
      (level match {
      case 1 => move(position)(fmap(loopInterchange)) `;` LCNF `;` RNF
      case l => shiftDimRec(position)(l - 1) `;` LCNF `;`
        move(position + l - 1)(loopInterchangeAtLevel(l))
    })

  // in front of **f, creating transpose pairs, move one transpose over **f
  def loopInterchange: Strategy =
      createId `;` createTransposePair `;` LCNF `;` argument(mapMapFBeforeTranspose)

  // level == 0: A.B.C.D => A.B.D.C
  //                 ^ ^        ^ ^
  // level == 1: A.B.C.D => A.C.B.D
  //               ^ ^        ^ ^   ... and so on
  def loopInterchangeAtLevel: Int => Strategy =
    level => applyNTimes(level)(fmap(_))(loopInterchange) `;` LCNF `;` RNF
}
