package shine.C.AST

import arithexpr.arithmetic._
import shine.C

sealed abstract class Type(val const: Boolean) {
  def print: String

  override def toString: String = {
    if (const) { "const " } else { "" } + print
  }
}

abstract class BasicType(val name: String, override val const: Boolean = false) extends Type(const) {
  override def print: String = name
}

abstract class StructType(val name: String, val fields: Seq[(Type, String)], override val const: Boolean = false) extends Type(const) {
  //  override def print: String = s"struct $name {${fields.map(_.print).mkString("; ")} }"
  override def print: String = s"struct $name"
}

// TODO: enforce flatness ???
abstract class ArrayType(val elemType: Type, val size: Option[ArithExpr], override val const: Boolean = false) extends Type(const) {
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

abstract class PointerType(val valueType: Type, override val const: Boolean = false) extends Type(const) {
  override def print: String = valueType.print + "*"
}

abstract class UnionType(val fields: Seq[Type], override val const: Boolean = false) extends Type(const) {
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

  val u8 = BasicType("uint8_t")
  val u16 = BasicType("uint16_t")
  val u32 = BasicType("uint32_t")
  val u64 = BasicType("uint64_t")

  val i8 = BasicType("int8_t")
  val i16 = BasicType("int16_t")
  val i32 = BasicType("int32_t")
  val i64 = BasicType("int64_t")

  val float = BasicType("float")
  val const_float = BasicType("float", const = true)

  val double = BasicType("double")
  val const_double = BasicType("double", const = true)

//  def fromDataType(dt: idealised.DPIA.Types.DataType): Type = {
//    dt match {
//      case b: idealised.DPIA.Types.BasicType => b match {
//        case idealised.DPIA.Types.bool => Type.int
//        case idealised.DPIA.Types.int => Type.int
//        case idealised.DPIA.Types.float => Type.float
//        case _: idealised.DPIA.Types.IndexType => Type.int
//        case v: idealised.DPIA.Types.VectorType =>
//          // this sets the representation of vector types in C:
//          // struct float4 {
//          //    float data[4];
//          // };
//          StructType(v.toString,
//            Seq((ArrayType(fromDataType(v.elemType), Some(v.size)), "data")))
//      }
//      case a: idealised.DPIA.Types.ArrayType => ArrayType(fromDataType(a.elemType), Some(a.size))
//      case a: idealised.DPIA.Types.DepArrayType => ArrayType(fromDataType(a.elemType), None) // TODO: be more precise with the size?
//      case r: idealised.DPIA.Types.RecordType =>
//        StructType(r.fst.toString + "_" + r.snd.toString, Seq(
//            (fromDataType(r.fst), "fst"),
//            (fromDataType(r.snd), "snd")))
//      case _: idealised.DPIA.Types.DataTypeIdentifier => throw new Exception("This should not happen")
//    }
//  }

  def getLength(t: Type): ArithExpr = {
    t match {
      case _: BasicType => Cst(1)
      case ArrayType(_, ae, _) if ae.isDefined => ae.get
      case _ => ???
    }
  }

  def getLengths(t: Type): Seq[ArithExpr] = {
    t match {
      case at: ArrayType => Seq(Type.getLength(at)) ++ getLengths(at.elemType)
      case _ => Seq(getLength(t))
    }
  }

  def getBaseType(t: Type): Type = {
    t match {
      case at: ArrayType => getBaseType(at.elemType)
      case _ => t
    }
  }
}

object BasicType {
  def apply(name: String, const: Boolean = false): BasicType = DefaultTypeImplementations.BasicType(name, const)
  def unapply(arg: BasicType): Option[(String, Boolean)] = Some((arg.name, arg.const))
}

object StructType {
  def apply(name: String, fields: Seq[(Type, String)], const: Boolean = false): StructType = DefaultTypeImplementations.StructType(name, fields, const)
  def unapply(arg: StructType): Option[(String, Seq[(Type, String)], Boolean)] = Some((arg.name, arg.fields, arg.const))
}

object ArrayType {
  def apply(elemType: Type, size: Option[ArithExpr], const: Boolean = false): ArrayType = DefaultTypeImplementations.ArrayType(elemType, size, const)
  def unapply(arg: ArrayType): Option[(Type, Option[ArithExpr], Boolean)] = Some((arg.elemType, arg.size, arg.const))
}

object PointerType {
  def apply(valueType: Type, const: Boolean = false): PointerType = DefaultTypeImplementations.PointerType(valueType, const)
  def unapply(arg: PointerType): Option[(Type, Boolean)] = Some((arg.valueType, arg.const))
}

object UnionType {
  def apply(fields: Seq[Type], const: Boolean = false): UnionType = DefaultTypeImplementations.UnionType(fields, const)
  def unapply(arg: UnionType): Option[(Seq[Type], Boolean)] = Some((arg.fields, arg.const))
}

object DefaultTypeImplementations {
  case class BasicType(override val name: String, override val const: Boolean = false)
    extends C.AST.BasicType(name, const)

  case class StructType(override val name: String, override val fields: Seq[(Type, String)], override val const: Boolean = false)
    extends C.AST.StructType(name, fields, const)

  case class ArrayType(override val elemType: Type,
                       override val size: Option[ArithExpr],
                       override val const: Boolean = false)
    extends C.AST.ArrayType(elemType, size, const)

  case class PointerType(override val valueType: Type, override val const: Boolean = false)
    extends C.AST.PointerType(valueType, const)

  case class UnionType(override val fields: Seq[Type], override val const: Boolean = false)
    extends C.AST.UnionType(fields, const)
}

