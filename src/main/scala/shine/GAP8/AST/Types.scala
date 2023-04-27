package shine.GAP8.AST

import rise.core.types.DataType

object Types {
  //TODO: Refactor in the style of OpenCL
  def sizeInBytes(dt: DataType): Int = dt match {
    case rise.core.types.DataType.bool => 1
    case rise.core.types.DataType.u8 => 1
    case rise.core.types.DataType.u16 => 2
    case rise.core.types.DataType.u32 => 4
    case rise.core.types.DataType.u64 => 8
    case rise.core.types.DataType.i8 => 1
    case rise.core.types.DataType.i16 => 2
    case rise.core.types.DataType.i32 => 4
    case rise.core.types.DataType.i64 => 8
    case rise.core.types.DataType.int => 4
    case rise.core.types.DataType.NatType => 4
    case rise.core.types.DataType.IndexType(size) => 4
    case rise.core.types.DataType.VectorType(size, elemType) => (sizeInBytes(elemType) * size).evalInt
    case rise.core.types.DataType.PairType(e1, e2) => sizeInBytes(e1) + sizeInBytes(e2)
    case rise.core.types.DataType.ArrayType(size, elemType) => (sizeInBytes(elemType) * size).evalInt
    case rise.core.types.DataType.DepArrayType(size, natToData) => ???
    case _ =>
      throw new RuntimeException("Should not happen")
  }
}

//TODO
case class SizeInBytes()
