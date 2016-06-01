package Core

sealed trait DataType {
  def isBasicType = false
}

object bool extends DataType { override def isBasicType = true; override def toString = "bool" }

object int extends DataType { override def isBasicType = true; override def toString = "int" }

object int4 extends DataType { override def isBasicType = true; override def toString = "int4"  }

object float extends DataType { override def isBasicType = true; override def toString = "float"  }

case class ArrayType(size: Int, elemType: DataType) extends DataType

case class RecordType(fst: DataType, snd: DataType) extends DataType