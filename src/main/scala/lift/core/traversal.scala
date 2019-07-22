package lift.core

import lift.arithmetic.NamedVar
import lift.core.types._

object traversal {
  sealed abstract class Result[+T](val value: T) {
    def map[U](f: T => U): Result[U]
    def mapVisitor(f: Visitor => Visitor): Result[T]
  }

  final case class Stop[+T](override val value: T) extends Result[T](value) {
    override def map[U](f: T => U): Result[U] = Stop(f(value))
    def mapVisitor(f: Visitor => Visitor): Result[T] = this
  }

  final case class Continue[+T](override val value: T, v: Visitor) extends Result[T](value) {
    override def map[U](f: T => U): Result[U] = Continue(f(value), v)
    def mapVisitor(f: Visitor => Visitor): Result[T] = Continue(value, f(v))
  }

  class Visitor {
    def visitExpr(e: Expr): Result[Expr] = Continue(e, this)
    def visitNat(ae: Nat): Result[Nat] = Continue(ae, this)
    def visitType[T <: Type](t: T): Result[T] = Continue(t, this)
    def visitDataType[DT <: DataType](dt: DT): Result[DT] = Continue(dt, this)
    def visitAddressSpace(a: AddressSpace): Result[AddressSpace] = Continue(a, this)
    def visitAccess(a: AccessType): Result[AccessType] = Continue(a, this)
    def visitN2N(n2n: NatToNat): Result[NatToNat] = Continue(n2n, this)
    def visitN2D(n2d: NatToData): Result[NatToData] = Continue(n2d, this)
  }

  object DepthFirstLocalResult {
    def apply(expr: Expr, v: Visitor): Expr = {
      v.visitExpr(expr) match {
        case s: Stop[Expr] => s.value
        case c: Continue[Expr] =>
          val v = c.v
          c.value match {
            case i: Identifier => i
            case Lambda(x, e) =>
              Lambda(x, apply(e, v))
            case Apply(f, e) =>
              Apply(apply(f, v), apply(e, v))
            case DepLambda(x, e) => x match {
              case n: NatIdentifier => DepLambda[NatKind]((v.visitNat(n).value: @unchecked) match {
                case a: NamedVar => NatIdentifier(a)
              }, apply(e, v))
              case dt: DataTypeIdentifier => DepLambda[DataKind](v.visitDataType(dt).value, apply(e, v))
            }
            case DepApply(f, x) => x match {
              case n: Nat           => DepApply[NatKind](apply(f, v), v.visitNat(n).value)
              case dt: DataType     => DepApply[DataKind](apply(f, v), v.visitDataType(dt).value)
              case a: AddressSpace  => DepApply[AddressSpaceKind](apply(f, v), v.visitAddressSpace(a).value)
              case w: AccessType    => DepApply[AccessKind](apply(f, v), v.visitAccess(w).value)
              case n2n: NatToNat    => DepApply[NatToNatKind](apply(f, v), v.visitN2N(n2n).value)
              case n2d: NatToData   => DepApply[NatToDataKind](apply(f, v), v.visitN2D(n2d).value)
            }
            case l: Literal => l
            case Index(n, size) =>
              Index(v.visitNat(n).value, v.visitNat(size).value)
            case NatExpr(n) =>
              NatExpr(v.visitNat(n).value)
            case TypedExpr(e, t) =>
              TypedExpr(apply(e, v), v.visitType(t).value)
            // could be avoided if foreign fun could be parametric
            case ForeignFunction(decl, t) =>
              ForeignFunction(decl, v.visitType(t).value)
            case p: Primitive => p
          }
      }
    }
  }

  object DepthFirstGlobalResult {
    def chain[A, B](a: Result[A], b: B,
                    visit_b: Visitor => Result[B]): Result[(A, B)] = {
      a match {
        case Stop(as) => Stop((as, b))
        case Continue(ac, vc) => visit_b(vc).map((ac, _))
      }
    }

    def chainE[A](a: Result[A], e: Expr): Result[(A, Expr)] =
      chain(a, e, apply(e, _))

    def chainN[A](a: Result[A], n: Nat): Result[(A, Nat)] =
      chain(a, n, v => v.visitNat(n))

    def chainT[A, T <: Type](a: Result[A], t: T): Result[(A, T)] =
      chain(a, t, v => v.visitType(t))

    def chainDT[A, DT <: DataType](a: Result[A], dt: DT): Result[(A, DT)] =
      chain(a, dt, v => v.visitDataType(dt))

    def apply(expr: Expr, visit: Visitor): Result[Expr] = {
      visit.visitExpr(expr) match {
        case Stop(r) => Stop(r)
        case Continue(c, v) => c match {
          case i: Identifier => Continue(i, v)
          case Lambda(x, e) =>
            apply(e, v).map(Lambda(x, _))
          case Apply(f, e) =>
            chainE(apply(f, v), e).map(r => Apply(r._1, r._2))
          case DepLambda(x, e) => x match {
            case n: NatIdentifier       => chainE(v.visitNat(n), e).map(r =>
              DepLambda[NatKind]((r._1: @unchecked) match { case a: NamedVar => NatIdentifier(a) }, r._2) )
            case dt: DataTypeIdentifier => chainE(v.visitDataType(dt), e).map(r => DepLambda[DataKind](r._1, r._2))
          }
          case DepApply(f, x) => x match {
            case n: Nat       => chainN(apply(f, v), n).map(r => DepApply[NatKind](r._1, r._2))
            case dt: DataType => chainDT(apply(f, v), dt).map(r => DepApply[DataKind](r._1, r._2))
          }
          case l: Literal => Continue(l, v)
          case Index(n, size) =>
            chainN(v.visitNat(n), size).map(r => Index(r._1, r._2))
          case NatExpr(n) =>
            v.visitNat(n).map(NatExpr)
          case TypedExpr(e, t) =>
            chainT(apply(e, v), t).map(r => TypedExpr(r._1, r._2))
          // could be avoided if foreign fun could be parametric
          case ForeignFunction(decl, t) =>
            v.visitType(t).map(ForeignFunction(decl, _))
          case p: Primitive => Continue(p, v)
        }
      }
    }
  }

  object types {
    object DepthFirstLocalResult {
      def apply[T <: Type](ty: T, visit: Visitor): T = {
        visit.visitType(ty) match {
          case s: Stop[T] => s.value
          case c: Continue[T] =>
            val v = c.v
            (c.value match {
              case DataAccessType(dt, w) => DataAccessType(data(dt, v), w)
              case FunType(a, b) => FunType(apply(a, v), apply(b, v))
              case DepFunType(x, t) =>
                x match {
                  case n: NatIdentifier =>
                    DepFunType[NatKind, Type]((v.visitNat(n).value: @unchecked) match {
                      case n: NamedVar => NatIdentifier(n.name, n.range)
                    }, apply(t, v))
                  case dt: DataTypeIdentifier =>
                    DepFunType[DataKind, Type](data(dt, v), apply(t, v))
                  case n2n: NatToNatIdentifier =>
                    DepFunType[NatToNatKind, Type](v.visitN2N(n2n).value.asInstanceOf[NatToNatIdentifier], apply(t, v))
                  case n2d: NatToDataIdentifier =>
                    DepFunType[NatToDataKind, Type](v.visitN2D(n2d).value.asInstanceOf[NatToDataIdentifier], apply(t, v))
                }
              case i: TypeIdentifier => i
            }).asInstanceOf[T]
        }
      }

      def data[DT <: DataType](dt: DT, visit: Visitor): DT = {
        visit.visitDataType(dt) match {
          case s: Stop[DT] => s.value
          case c: Continue[DT] =>
            val v = c.v
            (c.value match {
              case i: DataTypeIdentifier => i
              case ArrayType(n, e) => ArrayType(v.visitNat(n).value, data(e, v))
              case DepArrayType(n, e) => DepArrayType(v.visitNat(n).value, e.map(data(_, v)))
              case TupleType(ts@_*) => TupleType(ts.map(data(_, v)): _*)
              case s: ScalarType => s
              case IndexType(n) => IndexType(v.visitNat(n).value)
              case VectorType(n, e) => VectorType(v.visitNat(n).value, data(e, v))
              case NatToDataApply(ndtf, n) =>
                val newNDTF = ndtf match {
                  case i: NatToDataIdentifier => i
                  case NatToDataLambda(x, t) => NatToDataLambda(x, data(t, v))
                }
                NatToDataApply(newNDTF, v.visitNat(n).value)
            }).asInstanceOf[DT]
        }
      }
    }

    object DepthFirstGlobalResult {
      import traversal.DepthFirstGlobalResult.chain

      def chainT[A, T <: Type](a: Result[A], t: T):Result[(A,T)] =
        chain(a, t, apply(t, _))

      def chainDT[A, DT <: DataType](a: Result[A], dt: DT):Result[(A,DT)] =
        chain(a, dt, data(dt, _))

      def apply[T <: Type](ty: T, visit: Visitor): Result[T] = {
        visit.visitType(ty) match {
          case Stop(r) => Stop(r)
          case Continue(c, v) => (c match {
            case DataAccessType(dt, w) => DataAccessType(data(dt, v).value, w)
            case FunType(a, b) =>
              chainT(apply(a, v), b).map(r => FunType(r._1, r._2))
            case DepFunType(i, t) => i match {
              case dt: DataTypeIdentifier =>
                chainT(data(dt, v), t).map(r => DepFunType[DataKind, Type](r._1, r._2))
              case n: NatIdentifier =>
                chainT(v.visitNat(n), t).map(r =>
                  DepFunType[NatKind, Type]((r._1: @unchecked) match {
                    case n: NamedVar => NatIdentifier(n.name, n.range)
                  }, r._2))
            }
            case i: TypeIdentifier => i
          }).asInstanceOf[Result[T]]
        }
      }

      def data[DT <: DataType](dt: DT, visit: Visitor): Result[DT] = {
        visit.visitDataType(dt) match {
          case Stop(r) => Stop(r)
          case Continue(c, v) => (c match {
            case i: DataTypeIdentifier => Continue(i, v)
            case ArrayType(n, e) =>
              chainDT(v.visitNat(n), e).map(r => ArrayType(r._1, r._2))
            case DepArrayType(n, e) => e match {
                case ident: NatToDataIdentifier => v.visitNat(n).map(DepArrayType(_, ident))
                case NatToDataLambda(binder, body) =>
                  chainDT(v.visitNat(n), body).map(r => DepArrayType(r._1, NatToDataLambda(binder, r._2)))
              }
            case TupleType(ts@_*) =>
              ts.foldLeft(Continue(Vector(), v): Result[Vector[DataType]])({ case (r, t) =>
                chainDT(r, t).map(x => x._1 :+ x._2)
              }).map(ts => TupleType(ts: _*))
            case s: ScalarType => Continue(s, v)
            case IndexType(n) => v.visitNat(n).map(IndexType)
            case VectorType(n, e) =>
              chainDT(v.visitNat(n), e).map(r => VectorType(r._1, r._2))
          }).asInstanceOf[Result[DT]]
        }
      }
    }
  }
}