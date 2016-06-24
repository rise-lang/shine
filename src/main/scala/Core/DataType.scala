package Core

import apart.arithmetic.{ArithExpr, Cst}
import ir.ScalarType

sealed trait DataType
sealed abstract class BasicType extends DataType

object bool extends BasicType { override def toString = "bool" }

object int extends BasicType { override def toString = "int" }

object float extends BasicType { override def toString = "float" }

final case class VectorType(size: ArithExpr, elemType: BasicType) extends BasicType

final case class ArrayType(size: ArithExpr, elemType: DataType) extends DataType

final case class RecordType(fst: DataType, snd: DataType) extends DataType

object DataType {
  def toType(dt: DataType): ir.Type = {
    dt match {
      case b: BasicType => b match {
        case Core.bool => opencl.ir.Int
        case Core.int => opencl.ir.Int
        case Core.float => opencl.ir.Float
        case v: VectorType => ir.VectorType(DataType.toType(v.elemType) match {
          case s: ScalarType => s
          case _ => throw new Exception("This should not happen")
        }, v.size)
      }
      case a: ArrayType => ir.ArrayType(DataType.toType(a.elemType), a.size)
      case r: RecordType => ir.TupleType(DataType.toType(r.fst), DataType.toType(r.snd))
    }
  }

  def scalarType(dt: DataType): ir.ScalarType = {
    dt match {
      case b: BasicType => b match {
        case Core.bool => opencl.ir.Int
        case Core.int => opencl.ir.Int
        case Core.float => opencl.ir.Float
        case v: VectorType => scalarType(v.elemType)
      }
      case a: ArrayType => scalarType(a.elemType)
      case r: RecordType => ???
    }
  }

  def toString(dt: DataType): String = {
    dt match {
      case b: BasicType => b match {
        case Core.bool | Core.int => "int"
        case Core.float => "float"
        case v: VectorType => toString(v.elemType) + v.size.toString
      }
      case _: RecordType => ???
      case _: ArrayType => ???
    }
  }

  def getLengths(dt: DataType, tupleAccesss: List[ArithExpr], list: List[ArithExpr]): List[ArithExpr] = {
    dt match {
      case _: BasicType => 1 :: list
      case r: RecordType =>
        val t = tupleAccesss.head
        val elemT = if (t == Cst(1)) { r.fst } else if (t == Cst(2)) { r.snd } else { throw new Exception("This should not happen") }
        getLengths(elemT, tupleAccesss.tail, list)
      case a: ArrayType => getLengths(a.elemType, tupleAccesss, a.size :: list)
    }
  }
}