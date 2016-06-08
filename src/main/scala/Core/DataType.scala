package Core

import apart.arithmetic.ArithExpr

sealed trait DataType
sealed abstract class BasicType extends DataType

object bool extends BasicType { override def toString = "bool" }

object int extends BasicType { override def toString = "int" }

object int4 extends BasicType { override def toString = "int4" }

object float extends BasicType { override def toString = "float" }

case class ArrayType(size: ArithExpr, elemType: DataType) extends DataType

case class RecordType(fst: DataType, snd: DataType) extends DataType

object DataType {
  def toType(dt: DataType): ir.Type = {
    dt match {
      case b: BasicType => b match {
        case Core.bool => opencl.ir.Int
        case Core.int => opencl.ir.Int
        case Core.float => opencl.ir.Float
        case Core.int4 => opencl.ir.Int4
      }
      case a: ArrayType => ir.ArrayType(DataType.toType(a.elemType), a.size)
      case r: RecordType => ir.TupleType(DataType.toType(r.fst), DataType.toType(r.snd))
    }
  }
}