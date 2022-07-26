package rise.elevate.strategies

import elevate.core.Strategy
import elevate.core.strategies.Traversable
import elevate.core.strategies.basic._
import elevate.macros.RuleMacro.rule
import rise.elevate.{Rise, tunable}
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.movement._
import rise.elevate.rules.traversal.{argument, function}
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.traversal._

object tiling {

  def tileND(implicit ev: Traversable[Rise]): Int => Int => Strategy[Rise] = d => n => tileNDList(ev)(List.tabulate(d)(_ => n))


  // todo add dimensions
  // special syntax for 2D case - for ICFP'20 paper generic
  def tile()(implicit ev: Traversable[Rise]): Strategy[Rise] = tileNDList2(ev)(2)

  def tileNDList2(implicit ev: Traversable[Rise]): Int => Strategy[Rise] =

    n => n match {
      case x if x <= 0 => id
      // ((map f) arg)
      case 1 => function(tunable("tile", splitJoin)) // loop-blocking
      case i => fmap(tileNDList2(ev)(n - 1)) `;` // recurse
        function(tunable("tile", splitJoin)) `;` // loop-blocking
        interchange(ev)(i) // loop-interchange
    }


  // special syntax for 2D case - for ICFP'20 paper
  @rule def tile(x: Int, y: Int)(implicit ev: Traversable[Rise]): Strategy[Rise] = tileNDList(ev)(List(x, y))

  def tileNDList(implicit ev: Traversable[Rise]): List[Int] => Strategy[Rise] =

    n => n.size match {
      case x if x <= 0 => id
      // ((map f) arg)
      case 1 => function(splitJoin(n.head)) // loop-blocking
      case i => fmap(tileNDList(ev)(n.tail)) `;` // recurse
        function(splitJoin(n.head)) `;` // loop-blocking
        interchange(ev)(i) // loop-interchange
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
  def interchange(implicit ev: Traversable[Rise]): Int => Strategy[Rise] =
    d => {
      val joins = d
      val transposes = (1 to d - 2).sum
      RNF() `;` shiftDimRec(ev)(joins + transposes)(d - 1)
    }

  // position: how far to move right until we reach maps
  // level:    how deep transpose pairs are nested in maps
  def shiftDimRec(implicit ev: Traversable[Rise]): Int => Int => Strategy[Rise] =
    position => level => DFNF() `;`
      (level match {
        case 1 => moveTowardsArgument(position)(loopInterchangeAtLevel(ev)(1))
        case l => shiftDimRec(ev)(position)(l - 1) `;` RNF() `;`
          moveTowardsArgument(position + l - 1)(loopInterchangeAtLevel(ev)(l))
      })

  // in front of **f, creating transpose pairs, move one transpose over **f
  def loopInterchange(implicit ev: Traversable[Rise]): Strategy[Rise] =
    idAfter `;` createTransposePair `;` DFNF() `;` argument(mapMapFBeforeTranspose())

  // level == 0: A.B.C.D => A.B.D.C
  //             ^ ^        ^ ^
  // level == 1: A.B.C.D => A.C.B.D
  //               ^ ^        ^ ^   ... and so on
  def loopInterchangeAtLevel(implicit ev: Traversable[Rise]): Int => Strategy[Rise] =
    level => applyNTimes(level)((e: Strategy[Rise]) => fmap(e))(loopInterchange) `;` RNF()
}
