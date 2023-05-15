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

  def toStdint(dt: DataType): shine.C.AST.Type = {
    dt match {
      case rise.core.types.DataType.bool => shine.C.AST.BasicType("bool")
      case rise.core.types.DataType.u8 => shine.C.AST.Type.u8
      case rise.core.types.DataType.u16 => shine.C.AST.Type.u16
      case rise.core.types.DataType.u32 => shine.C.AST.Type.u32
      case rise.core.types.DataType.u64 => shine.C.AST.Type.u64
      case rise.core.types.DataType.i8 => shine.C.AST.Type.i8
      case rise.core.types.DataType.i16 => shine.C.AST.Type.i16
      case rise.core.types.DataType.i32 => shine.C.AST.Type.i32
      case rise.core.types.DataType.i64 => shine.C.AST.Type.i64
      case rise.core.types.DataType.int => shine.C.AST.Type.int
      case rise.core.types.DataType.f32 => shine.C.AST.Type.float
      case rise.core.types.DataType.f64 => shine.C.AST.Type.double
      case rise.core.types.DataType.ArrayType(size, elemType) => toStdint(elemType)
      case _ => throw new RuntimeException("Not implemented")
    }
  }
}

//TODO
case class SizeInBytes()
