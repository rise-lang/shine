package idealised.C.AST

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

case class StructType(fields: Seq[Type], override val const: Boolean = false) extends Type(const) {
  override def print: String = "struct {" + fields.map(_.print).mkString("; ") + "}"
}

case class ArrayType(elemType: Type, size: Option[ArithExpr], override val const: Boolean = false) extends Type(const) {
  override def print: String = elemType.print + "[" + (size match {
    case Some(n) => s"$n"
    case None => ""
  }) + "]"
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
        case v: idealised.DPIA.Types.VectorType => ArrayType(fromDataType(v.elemType), Some(v.size))
          //throw new Exception("Vector types are not supported in plain C")
      }
      case a: idealised.DPIA.Types.ArrayType => ArrayType(fromDataType(a.elemType), Some(a.size))
      case r: idealised.DPIA.Types.RecordType => StructType(Seq(fromDataType(r.fst), fromDataType(r.snd)))
      case _: idealised.DPIA.Types.DataTypeIdentifier => throw new Exception("This should not happen")
    }
  }
}

