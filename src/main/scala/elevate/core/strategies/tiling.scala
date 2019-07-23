package elevate.core.strategies

import elevate.core._
import elevate.core.strategies.traversal._
import elevate.core.strategies.basic._
import elevate.lift.rules.algorithmic._
import elevate.lift.rules.movement._
import elevate.lift.rules._
import elevate.lift.strategies.traversal._
import elevate.lift.strategies.normalForm._
import lift.core.primitives.map

object tiling {

  def tileND: Int => Int => Strategy =
    d => n => d match {
      case x if x <= 0 => id
      // ((map f) arg)
      case 1 => function(splitJoin(n)) `;` LCNF `;` RNF
      case i => fmap(tileND(d-1)(n)) `;` tileND(1)(n) `;` BENF `;`
        debug(s"\ntileND: $i ########################################################################\n") `;` shiftDim(i)
    }

  def shiftDim: Int => Strategy =
  // dim == 2 -> shift one level
  // dim == 3 -> shift two level
  // dim == 4 -> shift three level...
    d => {
      val overJoins = d
      val overTrans = (1 to d-2).sum
      val position = overJoins + overTrans
      LCNF `;` print(s"----moving-$overJoins-joins-and-$overTrans-transposes--------------------") `;`
        shiftDimRec(position)(d-1)//move(position)(loopInterchange(d - 1))
    }

  def shiftDimRec: Int => Int => Strategy =
  // position: how far to move right until we reach maps
  // level:    how deep transpose pairs are nested in maps
    position => level =>
      BENF `;` debugln(s"************************---shiftDimRec: position: $position, level: $level--------") `;` LCNF `;`
      (level match {
      case 1 => move(position)(loopInterchange(1)) `;` BENF `;` debugln("\nloops interchanged") `;` LCNF
      case l => //move(position)(loopInterchange(l)) `;` shiftDimRec(position + 1)(l - 1)
        shiftDimRec(position)(l - 1) `;` LCNF `;` move(position + l - 1)(loopInterchange(l))
    })

  def loopInterchange: Int => Strategy =
  // in front of maps, creating transpose pairs nested level-deep, move one after map
    depth => test3(depth)

  def test3: Int => Strategy = depth =>
    applyNTimes(depth)(fmap(_))(createId `;` createTransposePair `;` LCNF `;` argument(mapMapFBeforeTranspose)) `;`
    LCNF `;` RNF `;` BENF

  def test2: Strategy = debugln("1") `;`
    fmap(createId) `;` debugln("2") `;`
    fmap(createTransposePair) `;` LCNF `;` debugln("3") `;`
    fmap(argument(mapMapFBeforeTranspose)) `;` LCNF `;` RNF `;` BENF `;` debugln("4")

  def test1: Strategy = debug("1") `;`
    createId `;` debug("2") `;`
    liftId `;` debug("3") `;`
    LCNF `;` debug("4") `;`
    fmap(createTransposePair) `;` debug("5") `;`
    RNF `;` LCNF `;` debug("6") `;`
    argument(mapped(mapMapFBeforeTranspose)) `;` LCNF `;` debug("7")


}
