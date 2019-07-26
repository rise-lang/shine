package lift.core.types

import lift.core._

sealed trait Type

final case class TypeIdentifier(name: String) extends Type {
  override def toString: String = name
}

final case class DataAccessType(dt: DataType, w: AccessType) extends Type {
  override def toString: String = s"${dt}_$w"
}

final case class FunType[T1 <: Type, T2 <: Type](inT: T1, outT: T2) extends Type {
  override def toString: String = s"($inT -> $outT)"
}

final case class DepFunType[K <: Kind, T <: Type](x: K#I, t: T)
                                                 (implicit val kn: KindName[K]) extends Type {
  override def toString: String = s"(${x.name}: ${kn.get} -> $t)"

  override def equals(obj: Any): Boolean = obj match {
    case other: DepFunType[K, _] => t == lifting.liftDependentFunctionType[K](other)(x)
    case _ => false
  }
}