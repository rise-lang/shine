package elevate.lift.strategies

import elevate.core.{Elevate, Lift, Strategy}
import elevate.core.strategies.basic.{applyNTimes, debug, id}
import elevate.lift.rules.algorithmic._
import elevate.lift.rules.movement._
import elevate.lift.strategies.traversal._
import elevate.lift.strategies.normalForm._

object tiling {

  def tileND: Int => Int => Strategy[Lift] = d => n => tileNDList(List.tabulate(d)(_ => n))

  def tileNDList: List[Int] => Strategy[Lift] =
    l => {
      tileNDRec(l.size)(l)
    }

  def tileNDRec: Int => List[Int] => Strategy[Lift] =
    d => n => d match {
        case x if x <= 0 => id()
        // ((map f) arg)
        case 1 => function(splitJoin(n.head))
        case i => fmap(tileNDRec(d-1)(n.tail)) `;` tileNDRec(1)(n) `;` shiftDim(i)
      }

  // Notation: A.a -> a == tile dimension; A == original dimension
  // a.b.c.d: 4D array (outer => inner): a == outermost dim; d == innermost dim
  //
  // dim == 2 -> shift one level:
  //    A.a.B.b => A.B.a.b
  //    achieved by: (****f => *T o ****f o *T)
  //
  // dim == 3 -> shift two levels
  //    A.a.B.C.b.c => A.B.C.a.b.c
  //    (******f => *T o **T o ******f o **T o *T)
  // dim == 4 -> shift three levels ...
  def shiftDim: Int => Strategy[Lift] =
    d => {
      val joins = d
      val transposes = (1 to d-2).sum
      RNF `;` shiftDimRec(joins + transposes)(d-1)
    }

  // position: how far to move right until we reach maps
  // level:    how deep transpose pairs are nested in maps
  def shiftDimRec: Int => Int => Strategy[Lift] =
    position => level => LCNF `;`
      (level match {
      case 1 => moveTowardsArgument(position)(loopInterchangeAtLevel(1))
      case l => shiftDimRec(position)(l - 1) `;` RNF `;`
        moveTowardsArgument(position + l - 1)(loopInterchangeAtLevel(l))
    })

  // in front of **f, creating transpose pairs, move one transpose over **f
  def loopInterchange: Strategy[Lift] =
      idAfter `;` createTransposePair `;` LCNF `;` argument(mapMapFBeforeTranspose)

  // level == 0: A.B.C.D => A.B.D.C
  //             ^ ^        ^ ^
  // level == 1: A.B.C.D => A.C.B.D
  //               ^ ^        ^ ^   ... and so on
  def loopInterchangeAtLevel: Int => Strategy[Lift] =
    level => applyNTimes(level)((e: Strategy[Lift]) => fmap(e))(loopInterchange) `;` RNF
}
