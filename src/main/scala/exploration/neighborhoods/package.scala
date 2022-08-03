package exploration

package object neighborhoods {

  sealed trait NeighborhoodChoice

  case object NTreeChildrenChoice extends NeighborhoodChoice

  case object NTreeChildrenParentChoice extends NeighborhoodChoice

  case object NTreeLeafsWindowChoice extends NeighborhoodChoice

  case object NTreeLeafsDistanceChoice extends NeighborhoodChoice

  case object NPathDistanceChoice extends NeighborhoodChoice

  case object NGraphChoice extends NeighborhoodChoice

}
