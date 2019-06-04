package lift.core

import lift.arithmetic.NamedVar
import lift.core.types._

object traversal {
  sealed abstract class Result[+T](val value: T) {
    def map[U](f: T => U): Result[U]
  }

  final case class Stop[+T](override val value: T) extends Result[T](value) {
    override def map[U](f: T => U): Result[U] = Stop(f(value))
  }

  final case class Continue[+T](override val value: T, v: Visitor) extends Result[T](value) {
    override def map[U](f: T => U): Result[U] = Continue(f(value), v)
  }

  class Visitor {
    def apply(e: Expr): Result[Expr] = Continue(e, this)
    def apply(ae: Nat): Result[Nat] = Continue(ae, this)
    def apply[T <: Type](t: T): Result[T] = Continue(t, this)
    def apply(n2n: NatToNat): Result[NatToNat] = Continue(n2n, this)
  }

  object DepthFirstLocalResult {
    def apply(expr: Expr, v: Visitor): Expr = {
      v(expr) match {
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
              case n: NatIdentifier => DepLambda[NatKind]((v(n).value: @unchecked) match {
                case a: NamedVar => NatIdentifier(a)
              }, apply(e, v))
              case dt: DataTypeIdentifier => DepLambda[DataKind](v(dt).value, apply(e, v))
            }
            case DepApply(f, x) => x match {
              case n: Nat         => DepApply[NatKind](apply(f, v), v(n).value)
              case dt: DataType   => DepApply[DataKind](apply(f, v), v(dt).value)
              case n2n: NatToNat  => DepApply[NatToNatKind](apply(f, v), v(n2n).value)
            }
            case l: Literal => l
            case Index(n, size) =>
              Index(v(n).value, v(size).value)
            case NatExpr(n) =>
              NatExpr(v(n).value)
            case TypedExpr(e, t) =>
              TypedExpr(apply(e, v), v(t).value)
            // could be avoided if foreign fun could be parametric
            case primitives.ForeignFunction(decl, t) =>
              primitives.ForeignFunction(decl, v(t).value)
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
      chain(a, n, v => v(n))

    def chainT[A, T <: Type](a: Result[A], t: T): Result[(A, T)] =
      chain(a, t, v => v(t))

    def apply(expr: Expr, visit: Visitor): Result[Expr] = {
      visit(expr) match {
        case Stop(r) => Stop(r)
        case Continue(c, v) => c match {
          case i: Identifier => Continue(i, v)
          case Lambda(x, e) =>
            apply(e, v).map(Lambda(x, _))
          case Apply(f, e) =>
            chainE(apply(f, v), e).map(r => Apply(r._1, r._2))
          case DepLambda(x, e) => x match {
            case n: NatIdentifier       => chainE(v(n), e).map(r =>
              DepLambda[NatKind]((r._1: @unchecked) match { case a: NamedVar => NatIdentifier(a) }, r._2) )
            case dt: DataTypeIdentifier => chainE(v(dt), e).map(r => DepLambda[DataKind](r._1, r._2))
          }
          case DepApply(f, x) => x match {
            case n: Nat       => chainN(apply(f, v), n).map(r => DepApply[NatKind](r._1, r._2))
            case dt: DataType => chainT(apply(f, v), dt).map(r => DepApply[DataKind](r._1, r._2))
          }
          case l: Literal => Continue(l, v)
          case Index(n, size) =>
            chainN(v(n), size).map(r => Index(r._1, r._2))
          case NatExpr(n) =>
            v(n).map(NatExpr)
          case TypedExpr(e, t) =>
            chainT(apply(e, v), t).map(r => TypedExpr(r._1, r._2))
          // could be avoided if foreign fun could be parametric
          case primitives.ForeignFunction(decl, t) =>
            v(t).map(primitives.ForeignFunction(decl, _))
          case p: Primitive => Continue(p, v)
        }
      }
    }
  }

  object types {
    object DepthFirstLocalResult {
      def apply[T <: Type](ty: T, visit: Visitor): T = {
        visit(ty) match {
          case s: Stop[T] => s.value
          case c: Continue[T] =>
            val v = c.v
            (c.value match {
              case i: DataTypeIdentifier => i
              case ArrayType(n, e) => ArrayType(v(n).value, apply(e, v))
              case DepArrayType(n, e) => DepArrayType(v(n).value, e.map(apply(_, v)))
              case TupleType(ts@_*) => TupleType(ts.map(apply(_, v)): _*)
              case s: ScalarType => s
              case IndexType(n) => IndexType(v(n).value)
              case VectorType(n, e) => VectorType(v(n).value, apply(e, v))
              case FunType(a, b) => FunType(apply(a, v), apply(b, v))
              case DepFunType(x, t) =>
                x match {
                  case n: NatIdentifier =>
                    DepFunType[NatKind, Type]((v(n).value: @unchecked) match {
                      case n: NamedVar => NatIdentifier(n.name, n.range)
                    }, apply(t, v))
                  case dt: DataTypeIdentifier =>
                    DepFunType[DataKind, Type](apply(dt, v), apply(t, v))
                  case n2n: NatToNatIdentifier =>
                    DepFunType[NatToNatKind, Type](v(n2n).value.asInstanceOf[NatToNatIdentifier], apply(t, v))
                }

              case NatToDataApply(ndtf, n) =>
                val newNDTF = ndtf match {
                case i:NatToDataIdentifier => i
                case NatToDataLambda(x, dt) => NatToDataLambda(x, apply(dt, v))
              }
                NatToDataApply(newNDTF, v(n).value)
            }).asInstanceOf[T]
        }
      }
    }

    object DepthFirstGlobalResult {
      import traversal.DepthFirstGlobalResult.chain

      def chainT[A, T <: Type](a: Result[A], t: T):Result[(A,T)] =
        chain(a, t, apply(t, _))

      def apply[T <: Type](ty: T, visit: Visitor): Result[T] = {
        visit(ty) match {
          case Stop(r) => Stop(r)
          case Continue(c, v) => (c match {
            case i: DataTypeIdentifier => Continue(i, v)
            case ArrayType(n, e) =>
              chainT(v(n), e).map(r => ArrayType(r._1, r._2))
            case DepArrayType(n, e) =>
              e match {
                case ident:NatToDataIdentifier => v(n).map(DepArrayType(_, ident))
                case NatToDataLambda(binder, body) =>
                  chainT(v(n), body).map(r => DepArrayType(r._1, NatToDataLambda(binder, r._2)))
              }
            case TupleType(ts@_*) =>
              ts.foldLeft(Continue(Vector(), v) : Result[Vector[DataType]])({ case (r, t) =>
                chainT(r, t).map(x => x._1 :+ x._2)
              }).map(ts => TupleType(ts: _*))
            case s: ScalarType => Continue(s, v)
            case IndexType(n) => v(n).map(IndexType)
            case VectorType(n, e) =>
              chainT(v(n), e).map(r => VectorType(r._1, r._2))
            case FunType(a, b) =>
              chainT(apply(a, v), b).map(r => FunType(r._1, r._2))
            case DepFunType(dt: DataTypeIdentifier, t) =>
              chainT(apply(dt, v), t).map(r => DepFunType[DataKind, Type](r._1, r._2))
            case DepFunType(n: NatIdentifier, t) =>
              chainT(v(n), t).map(r =>
                DepFunType[NatKind, Type]((r._1: @unchecked) match {
                  case n: NamedVar => NatIdentifier(n.name, n.range)
                }, r._2))
          }).asInstanceOf[Result[T]]
        }
      }
    }
  }
}