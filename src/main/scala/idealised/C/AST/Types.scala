package idealised.C.AST

import idealised.DPIA.Types.{DataTypeIdentifier, RecordType}
import lift.arithmetic._

sealed abstract class Type(val const: Boolean) {
  def print: String

  override def toString: String = {
    if (const) { "const " } else { "" } + print
  }
}

case class BasicType(name: String, override val const: Boolean = false) extends Type(const) {
  override def print: String = name
}

case class StructType(name: String, fields: Seq[(Type, String)], override val const: Boolean = false) extends Type(const) {
  //  override def print: String = s"struct $name {${fields.map(_.print).mkString("; ")} }"
  override def print: String = s"struct $name"
}

case class ArrayType(elemType: Type, size: Option[ArithExpr], override val const: Boolean = false) extends Type(const) {
  override def print: String = elemType.print + "[" + (size match {
    case Some(n) => s"$n"
    case None => ""
  }) + "]"

  def getBaseType: Type = {
    elemType match {
      case _: BasicType => elemType
      case _: StructType => elemType
      case _: PointerType => elemType
      case _: UnionType => elemType
      case a: ArrayType => a.getBaseType
    }
  }

  def getSizes: Option[ArithExpr] = {
    size match {
      case None     =>  None
      case Some(s)  => elemType match {
        case a: ArrayType =>
          a.getSizes match {
            case None => Some(s)
            case Some(s2) => Some(s * s2)
          }
        case _ => Some(s)
      }
    }
  }
}

case class PointerType(valueType: Type, override val const: Boolean = false) extends Type(const) {
  override def print: String = valueType.print + "*"
}

case class UnionType(fields: Seq[Type], override val const: Boolean = false) extends Type(const) {
  override def print: String = "union {" + fields.map(_.print).mkString("; ") + "}"
}


object Type {
  val void = BasicType("void")

  val char = BasicType("char")
  val const_char = BasicType("char", const = true)

  val uchar = BasicType("unsigned char")
  val uconst_char = BasicType("unsigned char", const = true)

  val int = BasicType("int")
  val const_int = BasicType("int", const = true)

  val float = BasicType("float")
  val const_float = BasicType("float", const = true)

  def fromDataType(dt: idealised.DPIA.Types.DataType): Type = {
    dt match {
      case b: idealised.DPIA.Types.BasicType => b match {
        case idealised.DPIA.Types.bool => Type.int
        case idealised.DPIA.Types.int => Type.int
        case idealised.DPIA.Types.float => Type.float
        case _: idealised.DPIA.Types.IndexType => Type.int
        case v: idealised.DPIA.Types.VectorType =>
          // this sets the representation of vector types in C:
          // struct float4 {
          //    float data[4];
          // };
          StructType(v.toString,
            Seq((ArrayType(fromDataType(v.elemType), Some(v.size)), "data")))
      }
      case a: idealised.DPIA.Types.ArrayType => ArrayType(fromDataType(a.elemType), Some(a.size))
      case r: idealised.DPIA.Types.RecordType =>
        StructType(r.fst.toString + "_" + r.snd.toString, Seq(
            (fromDataType(r.fst), "fst"),
            (fromDataType(r.snd), "snd")))
      case _: idealised.DPIA.Types.DataTypeIdentifier => throw new Exception("This should not happen")
    }
  }
}

