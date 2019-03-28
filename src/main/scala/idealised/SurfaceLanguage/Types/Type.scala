package idealised.SurfaceLanguage.Types

import idealised.DPIA.freshName
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Semantics._
import lift.arithmetic.{ArithExpr, NamedVar, RangeAdd}

sealed trait Type

// data types
sealed trait DataType extends Type

final case class DataTypeIdentifier(name: String) extends DataType


sealed trait ComposedType extends DataType

final case class ArrayType(size: Nat, elemType: DataType) extends ComposedType {
  override def toString: String = s"$size.$elemType"
}

final case class DepArrayType(size: Nat, elemType: `(nat)->dt`) extends ComposedType {
  override def toString: String = s"$size.${elemType.n} -> ${elemType.t}"
}

object DepArrayType {
  def apply(size: Nat, f: Nat => DataType): DepArrayType = {
    val newName = NamedVar(freshName(), RangeAdd(0, size, 1))
    DepArrayType(size, NatDependentFunctionType(newName, f(newName)))
  }
}

final case class TupleType(elemTypes: DataType*) extends ComposedType {
  assert(elemTypes.size == 2)

  override def toString: String = elemTypes.map(_.toString).mkString("(", ", ", ")")
}


sealed trait BasicType extends DataType


sealed trait ScalarType extends BasicType

object bool extends ScalarType {
  override def toString: String = "bool"
}

object int extends ScalarType {
  override def toString: String = "int"
}

object float extends ScalarType {
  override def toString: String = "float"
}

object double extends ScalarType { override def toString: String = "double" }

object NatType extends ScalarType { override def toString: String = "nat"}

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

final case class TypeDependentFunctionType[T <: Type](dt: DataTypeIdentifier, t: T) extends Type

final case class NatDependentFunctionType[T <: Type](n: NatIdentifier, t: T) extends Type

object NatDependentFunctionType {
  def apply[T <: Type](f: NatIdentifier => T): NatDependentFunctionType[T] = {
    val newN = NamedVar(freshName())
    NatDependentFunctionType(newN, f(newN))
  }
}

object Type {

  def substitute[T <: Type](dt: DataType,
                            `for`: DataTypeIdentifier,
                            in: Expr): Expr = {

    object Visitor extends VisitAndRebuild.Visitor {
      override def apply[T2 <: Type](in: T2): T2 =
        substitute(dt, `for`, in).asInstanceOf[T2]
    }

    VisitAndRebuild(in, Visitor)

  }

  def substitute[T <: Type](ae: Nat,
                            `for`: NatIdentifier,
                            in: Expr): Expr = {

    object Visitor extends VisitAndRebuild.Visitor {
      import VisitAndRebuild.{Result, Stop, Continue}

      override def apply(e: Expr): Result[Expr] = {
        e match {
          case IdentifierExpr(name, _) =>
            if (`for`.name == name) {
              Stop(LiteralExpr(IndexData(ae)))
            } else {
              Continue(e, this)
            }
          case LiteralExpr(IndexData(index, IndexType(size)), _) =>
            val newIndex = substitute(ae, `for`, in = index)
            val newSize = substitute(ae, `for`, in = size)
            Stop(LiteralExpr(IndexData(newIndex, IndexType(newSize))))
          case _ =>
            Continue(e, this)
        }
      }

      override def apply(e: Nat): Nat = substitute(ae, `for`, e)

      override def apply[T2 <: Type](t: T2): T2 =
        substitute(ae, `for`, t).asInstanceOf[T2]
    }

    VisitAndRebuild(in, Visitor)

  }

  def substitute(t: Type, `for`: Type, in: Type): Type = {
    def st(in: Type) = substitute(t, `for`, in)

    if (`for` == in) {
      t
    } else {
      in match {
        case _: BasicType => in
        case ArrayType(size, elemType) => ArrayType(size, st(elemType).asInstanceOf[DataType])
        case DepArrayType(size, elemType) => DepArrayType(size, st(elemType).asInstanceOf[NatDependentDataType])
        case TupleType(ts@_*) => TupleType(ts.map(st(_).asInstanceOf[DataType]): _*)
        case FunctionType(inT, outT) =>
          FunctionType(st(inT), st(outT))
        case TypeDependentFunctionType(dt, body) =>
          TypeDependentFunctionType(dt, st(body))
        case NatDependentFunctionType(n, body) =>
          NatDependentFunctionType(n, st(body))
      }
    }
  }

  def substitute(ae: Nat, `for`: NatIdentifier, in: Type): Type = {
    def sn(n: Nat) = substitute(ae, `for`, n)
    def sdt(dt: DataType) = substitute(ae, `for`, dt)
    def st(t: Type) = substitute(ae, `for`, t)

    in match {
      case _: ScalarType => in
      case IndexType(size) => IndexType(sn(size))
      case ArrayType(size, elemType) =>
        ArrayType(sn(size), sdt(elemType))
      case DepArrayType(size, elemType) =>
        DepArrayType(sn(size), st(elemType).asInstanceOf[NatDependentDataType])
      case TupleType(ts@_*) => TupleType(ts.map(sdt): _*)
      case FunctionType(inT, outT) =>
        FunctionType(st(inT), st(outT))
      case TypeDependentFunctionType(dt, body) =>
        TypeDependentFunctionType(dt, st(body))
      case NatDependentFunctionType(n, body) =>
        NatDependentFunctionType(n, st(body))
    }
  }

  def substitute[T <: DataType](dt: DataType, `for`: DataType, in: T): T = {
    if (`for` == in) {
      dt.asInstanceOf[T]
    } else {
      (in match {
        case _: BasicType => in
        case ArrayType(size, elemType) => ArrayType(size, substitute(dt, `for`, elemType))
        case TupleType(ts@_*) => TupleType(ts.map(substitute(dt, `for`, _)): _*)
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
      case DepArrayType(size, NatDependentFunctionType(x, elemT)) =>
        val innerT = NatDependentFunctionType(x, substitute(ae, `for`, elemT))
        DepArrayType(ArithExpr.substitute(size, Map((`for`, ae))), innerT)
      case TupleType(ts@_*) => TupleType(ts.map(substitute(ae, `for`, _)): _*)
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

  def rebuild[T <: Type](f: Nat => Nat, in: T): T = {
    (in match {
      case IndexType(size) => IndexType(f(size))
      case b: BasicType => b
      case ArrayType(size, elemType) => ArrayType(f(size), rebuild(f, elemType))
      case DepArrayType(size, elemType) => DepArrayType(f(size), rebuild(f, elemType))
      case TupleType(ts@_*) => TupleType(ts.map(t => rebuild(f, t)): _*)
      case FunctionType(inT, outT) => FunctionType(rebuild(f, inT), rebuild(f, outT))
      case NatDependentFunctionType(ident, outT) => NatDependentFunctionType(f(ident).asInstanceOf[NatIdentifier], rebuild(f, outT))
      case TypeDependentFunctionType(ident, outT) => TypeDependentFunctionType(ident, rebuild(f, outT))
    }).asInstanceOf[T]
  }
}