package Core

sealed trait DataType {
  def isBasicType = false
}

object bool extends DataType { override def isBasicType = true }

object int extends DataType { override def isBasicType = true }

object int4 extends DataType { override def isBasicType = true }

object float extends DataType { override def isBasicType = true }

case class ArrayType(size: Int, elemType: DataType) extends DataType

case class RecordType(fst: DataType, snd: DataType) extends DataType