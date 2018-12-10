package idealised.SurfaceLanguage.Types

import idealised.DPIA.freshName
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Semantics._
import lift.arithmetic.{ArithExpr, NamedVar}

sealed trait Type

// data types
sealed trait DataType extends Type

final case class DataTypeIdentifier(name: String) extends DataType


sealed trait ComposedType extends DataType

final case class ArrayType(size: Nat, elemType: DataType) extends ComposedType {
  override def toString: String = s"$size.$elemType"
}

final case class DepArrayType(size:Nat, elemType: `(nat)->dt`) extends ComposedType {
  override def toString: String = s"$size.$elemType"
}

object DepArrayType {
  def apply(size:Nat, f:Nat => DataType): DepArrayType = {
    val newName = NamedVar(freshName())
    DepArrayType(size, NatDependentFunctionType(newName, f(newName)))
  }
}

final case class TupleType(elemTypes: DataType*) extends ComposedType {
  assert(elemTypes.size == 2)
  override def toString: String = elemTypes.map(_.toString).mkString("(", ", ", ")")
}


sealed trait BasicType extends DataType


sealed trait ScalarType extends BasicType

object bool extends ScalarType { override def toString: String = "bool" }

object int extends ScalarType { override def toString: String = "int" }

object float extends ScalarType { override def toString: String = "float" }

final case class IndexType(size: Nat) extends BasicType


sealed case class VectorType(size: Nat, elemType: ScalarType) extends BasicType {
  override def toString: String = s"$elemType$size"
}

object int2 extends VectorType(2, int)
object int3 extends VectorType(3, int)
object int4 extends VectorType(4, int)
object int8 extends VectorType(8, int)
object int16 extends VectorType(16, int)

object float2 extends VectorType(2, float)
object float3 extends VectorType(3, float)
object float4 extends VectorType(4, float)
object float8 extends VectorType(8, float)
object float16 extends VectorType(16, float)


// function types
final case class FunctionType[T1 <: Type, T2 <: Type](inT: T1, outT: T2) extends Type

final case class TypeDependentFunctionType[T <: Type](x: DataTypeIdentifier, t: T) extends Type

final case class NatDependentFunctionType[T <: Type](x: NatIdentifier, t: T) extends Type

object NatDependentFunctionType {
  def apply[T <: Type](f: NatIdentifier => T): NatDependentFunctionType[T] = {
    val newX = NamedVar(freshName())
    NatDependentFunctionType(newX, f(newX))
  }
}

object Type {

  def substitute[T <: Type](dt: DataType,
                            `for`: DataTypeIdentifier,
                            in: Expr[T]): Expr[T] = {

    object Visitor extends VisitAndRebuild.Visitor {
      override def apply[DT <: DataType](in: DT): DT = substitute(dt, `for`, in)
    }

    VisitAndRebuild(in, Visitor)

  }

  def substitute[T <: Type](ae: Nat,
                            `for`: NatIdentifier,
                            in: Expr[T]): Expr[T] = {

    object Visitor extends VisitAndRebuild.Visitor {
      override def apply[T2 <: Type](e: Expr[T2]): Result[Expr[T2]] = {
        e match {
          case IdentifierExpr(name, _) =>
            if (`for`.name == name) {
              Stop(LiteralExpr(IndexData(ae)).asInstanceOf[Expr[T2]])
            } else {
              Continue(e, this)
            }
          case LiteralExpr(IndexData(index, IndexType(size))) =>
            val newIndex = substitute(ae, `for`, in = index)
            val newSize = substitute(ae, `for`, in = size)
            Stop(LiteralExpr(IndexData(newIndex, IndexType(newSize))).asInstanceOf[Expr[T2]])
          case _ =>
            Continue(e, this)
        }
      }

      override def apply(e: Nat): Nat = substitute(ae, `for`, e)

      override def apply[DT <: DataType](dt: DT): DT = substitute(ae, `for`, dt)
    }

    VisitAndRebuild(in, Visitor)

  }

  def substitute[T <: DataType](dt: DataType, `for`: DataType, in: T): T = {
    if (`for` == in) {
      dt.asInstanceOf[T]
    } else {
      (in match {
        case _: BasicType => in
        case ArrayType(size, elemType) => ArrayType(size, substitute(dt, `for`, elemType))
        case TupleType(ts @ _*) => TupleType(ts.map(substitute(dt, `for`, _)):_*)
      }).asInstanceOf[T]
    }
  }

  def substitute[T <: DataType](ae: Nat, `for`: NatIdentifier, in: T): T = {
    (in match {
      case IndexType(size) =>
        IndexType(substitute(ae, `for`, size))
      case b: BasicType => b
      case ArrayType(size, elemType) =>
        ArrayType(ArithExpr.substitute(size, Map((`for`, ae))), substitute(ae, `for`, elemType))
      case TupleType(ts @ _*) => TupleType(ts.map(substitute(ae, `for`, _)):_*)
    }).asInstanceOf[T]
  }

  private def substitute(ae: Nat, `for`: NatIdentifier, in: Nat): Nat = {
    in.visitAndRebuild {
      case v: NamedVar =>
        if (`for`.name == v.name) {
          ae
        } else {
          v
        }
      case e => e
    }
  }

}