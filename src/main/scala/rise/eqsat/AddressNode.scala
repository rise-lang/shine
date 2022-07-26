package rise.eqsat

import rise.core.{types => rct}

object Address {
  type Shift = Int

  def fromNamed(a: rct.AddressSpace, bound: Expr.Bound): Address = {
    a match {
      case i: rct.AddressSpaceIdentifier => AddressVar(bound.indexOf(i))
      case rct.AddressSpace.Global => Global
      case rct.AddressSpace.Local => Local
      case rct.AddressSpace.Private => Private
      case rct.AddressSpace.Constant => Constant
    }
  }

  def toNamed(a: Address, bound: Expr.Bound): rct.AddressSpace = {
    a match {
      case AddressVar(index) => bound.getAddr(index)
      case Global => rct.AddressSpace.Global
      case Local => rct.AddressSpace.Local
      case Private => rct.AddressSpace.Private
      case Constant => rct.AddressSpace.Constant
    }
  }
}

object AddressPattern {
  def fromAddress(n: AddressNode): AddressPattern =
    AddressPatternNode(n)
}

sealed trait AddressPattern {
  def patternVars(): Set[Any] = {
    this match {
      case AddressPatternNode(_) => Set()
      case pv: AddressPatternVar => Set(pv)
      case AddressPatternAny => Set()
    }
  }
}
case class AddressPatternNode(n: AddressNode) extends AddressPattern {
  override def toString: String = n.toString
}
case class AddressPatternVar(index: Int) extends AddressPattern {
  override def toString: String = s"?a$index"
}
case object AddressPatternAny extends AddressPattern {
  override def toString: String = "?a"
}

sealed trait AddressNode
case class AddressVar(index: Int) extends AddressNode {
  override def toString: String = s"%a$index"
}
case object Global extends AddressNode {
  override def toString: String = "global"
}
case object Local extends AddressNode {
  override def toString: String = "local"
}
case object Private extends AddressNode {
  override def toString: String = "private"
}
case object Constant extends AddressNode {
  override def toString: String = "constant"
}
