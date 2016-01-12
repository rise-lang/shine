package Core

sealed trait DataType

object bool extends DataType

object int extends DataType

object int4 extends DataType

object float extends DataType

case class ArrayType(size: Int, elemType: DataType) extends DataType

case class RecordType(fields: DataType*) extends DataType