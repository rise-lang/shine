package Core

import apart.arithmetic.{ArithExpr, Cst}

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

  def getLengths(dt: DataType, tupleAccesss: List[ArithExpr], list: List[ArithExpr]): List[ArithExpr] = {
    dt match {
      case _: BasicType => 1 :: list
      case r: RecordType => {
        val t = tupleAccesss.head
        val elemT = if (t == Cst(1)) { r.fst } else if (t == Cst(2)) { r.snd } else { throw new Exception("This should not happen") }
        getLengths(elemT, tupleAccesss.tail, list)
      }
      case a: ArrayType => getLengths(a.elemType, tupleAccesss, a.size :: list)
    }
  }
}