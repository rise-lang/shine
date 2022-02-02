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

object FragmentKind {
  type Shift = Int

  def fromNamed(a: rct.Fragment, bound: Expr.Bound): FragmentKindNode = {
    a match {
      case i: rct.FragmentIdentifier => FragmentKindVar(bound.indexOf(i))
      case rct.Fragment.AMatrix => AMatrix
      case rct.Fragment.BMatrix => BMatrix
      case rct.Fragment.Accumulator => Accumulator
    }
  }

  def toNamed(a: FragmentKindNode, bound: Expr.Bound): rct.Fragment = {
    a match {
      case FragmentKindVar(index) => bound.getFrag(index)
      case AMatrix => rct.Fragment.AMatrix
      case BMatrix => rct.Fragment.BMatrix
      case Accumulator => rct.Fragment.Accumulator
    }
  }
}

object FragemntKindPattern {
  def fromAddress(n: FragmentKindNode): FragmentKindPattern =
    FragmentKindPatternNode(n)
}

sealed trait FragmentKindPattern {
  def patternVars(): Set[Any] = {
    this match {
      case FragmentKindPatternNode(_) => Set()
      case pv: FragmentKindPatternVar => Set(pv)
      case FragmentKindPatternAny => Set()
    }
  }
}
case class FragmentKindPatternNode(n: FragmentKindNode) extends FragmentKindPattern {
  override def toString: String = n.toString
}
case class FragmentKindPatternVar(index: Int) extends FragmentKindPattern {
  override def toString: String = s"?f$index"
}
case object FragmentKindPatternAny extends FragmentKindPattern {
  override def toString: String = "?f"
}

sealed trait FragmentKindNode
case class FragmentKindVar(index: Int) extends FragmentKindNode {
  override def toString: String = s"%f$index"
}
case object AMatrix extends FragmentKindNode {
  override def toString: String = "AMatrix"
}
case object BMatrix extends FragmentKindNode {
  override def toString: String = "BMatrix"
}
case object Accumulator extends FragmentKindNode {
  override def toString: String = "Accumulator"
}

object MatrixLayout {
  type Shift = Int

  def fromNamed(a: rct.MatrixLayout, bound: Expr.Bound): MatrixLayoutNode = {
    a match {
      case i: rct.MatrixLayoutIdentifier => MatrixLayoutVar(bound.indexOf(i))
      case rct.MatrixLayout.Row_Major => Row_Major
      case rct.MatrixLayout.Col_Major => Column_Major
      case rct.MatrixLayout.None => Row_Major //dont care about None
    }
  }

  def toNamed(a: MatrixLayoutNode, bound: Expr.Bound): rct.MatrixLayout = {
    a match {
      case MatrixLayoutVar(index) => bound.getML(index)
      case Row_Major => rct.MatrixLayout.Row_Major
      case Column_Major => rct.MatrixLayout.Col_Major
    }
  }
}

object MatrixLayoutPattern {
  def fromAddress(n: MatrixLayoutNode): MatrixLayoutPattern =
    MatrixLayoutPatternNode(n)
}

sealed trait MatrixLayoutPattern {
  def patternVars(): Set[Any] = {
    this match {
      case MatrixLayoutPatternNode(_) => Set()
      case pv: MatrixLayoutPatternVar => Set(pv)
      case MatrixLayoutPatternAny => Set()
    }
  }
}
case class MatrixLayoutPatternNode(n: MatrixLayoutNode) extends MatrixLayoutPattern {
  override def toString: String = n.toString
}
case class MatrixLayoutPatternVar(index: Int) extends MatrixLayoutPattern {
  override def toString: String = s"?ml$index"
}
case object MatrixLayoutPatternAny extends MatrixLayoutPattern {
  override def toString: String = "?ml"
}

sealed trait MatrixLayoutNode
case class MatrixLayoutVar(index: Int) extends MatrixLayoutNode {
  override def toString: String = s"%ml$index"
}
case object Row_Major extends MatrixLayoutNode {
  override def toString: String = "Row_Major"
}
case object Column_Major extends MatrixLayoutNode {
  override def toString: String = "Column_Major"
}
