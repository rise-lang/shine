package lift.core

import lift.core.types._
import lift.arithmetic._

object substitute {
  def apply(expr: Expr, `for`: Expr, in: Expr): Expr = {
    import traversal.{Result, Stop, Continue}

    object Visitor extends traversal.Visitor {
      override def apply(e: Expr): Result[Expr] = {
        if (`for` == e) { Stop(expr) } else { Continue(e, this) }
      }
    }

    traversal.DepthFirstLocalResult(in, Visitor)
  }

  def apply[T <: Type](dt: DataType,
                       `for`: DataTypeIdentifier,
                       in: Expr): Expr = {

    object Visitor extends traversal.Visitor {
      override def apply[T2 <: Type](in: T2): T2 =
        substitute(dt, `for`, in).asInstanceOf[T2]
    }

    traversal.DepthFirstLocalResult(in, Visitor)
  }

  def apply[T <: Type](ae: Nat,
                       `for`: NatIdentifier,
                       in: Expr): Expr = {

    object Visitor extends traversal.Visitor {
      import traversal.{Result, Stop, Continue}

      override def apply(e: Expr): Result[Expr] = {
        e match {
          case Identifier(name) =>
            if (`for`.name == name) {
              Stop(NatExpr(ae))
            } else {
              Continue(e, this)
            }
          case _ => Continue(e, this)
        }
      }

      override def apply(e: Nat): Nat = substitute(ae, `for`, e)

      override def apply[T2 <: Type](t: T2): T2 =
        substitute(ae, `for`, t).asInstanceOf[T2]
    }

    traversal.DepthFirstLocalResult(in, Visitor)
  }

  def apply(t: Type, `for`: Type, in: Type): Type = {
    def st(in: Type) = substitute(t, `for`, in)

    if (`for` == in) {
      t
    } else {
      in match {
        case _: BasicType | _: DataTypeIdentifier => in
        case ArrayType(size, elemType) => ArrayType(size, st(elemType).asInstanceOf[DataType])
        case DepArrayType(size, elemType) => DepArrayType(size, st(elemType).asInstanceOf[NatDependentDataType])
        case TupleType(ts@_*) => TupleType(ts.map(st(_).asInstanceOf[DataType]): _*)
        case FunctionType(inT, outT) =>
          FunctionType(st(inT), st(outT))
        case TypeDependentFunctionType(dt, body) =>
          TypeDependentFunctionType(st(dt).asInstanceOf[DataTypeIdentifier], st(body))
        case NatDependentFunctionType(n, body) =>
          NatDependentFunctionType(n, st(body))
      }
    }
  }

  def apply(ae: Nat, `for`: NatIdentifier, in: Type): Type = {
    def sn(n: Nat) = substitute(ae, `for`, n)
    def sdt(dt: DataType) = substitute(ae, `for`, dt)
    def st(t: Type) = substitute(ae, `for`, t)

    in match {
      case _: ScalarType | _: DataTypeIdentifier => in
      case IndexType(size) => IndexType(sn(size))
      case VectorType(size, elemType) =>
        VectorType(sn(size), elemType)
      case ArrayType(size, elemType) =>
        ArrayType(sn(size), st(elemType).asInstanceOf[DataType])
      case DepArrayType(size, elemType) =>
        DepArrayType(sn(size), st(elemType).asInstanceOf[NatDependentDataType])
      case TupleType(ts@_*) => TupleType(ts.map(st(_).asInstanceOf[DataType]): _*)
      case FunctionType(inT, outT) =>
        FunctionType(st(inT), st(outT))
      case TypeDependentFunctionType(dt, body) =>
        TypeDependentFunctionType(sdt(dt).asInstanceOf[DataTypeIdentifier], st(body))
      case NatDependentFunctionType(n, body) =>
        NatDependentFunctionType(sn(n).asInstanceOf[NatIdentifier], st(body))
    }
  }
/*
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
*/

  def apply(ae: Nat, `for`: NatIdentifier, in: Nat): Nat = {
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
/*
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
  */
}