package rise.core

import scala.language.implicitConversions

import arithexpr.arithmetic.NamedVar
import rise.core.semantics._
import rise.core.types._

object Traverse {
  trait Monad[M[_]] {
    def return_[T] : T => M[T]
    def bind[T,S] : M[T] => (T => M[S]) => M[S]
  }

  implicit def monadicSyntax[M[_], A](m: M[A])(implicit tc: Monad[M]) = new {
    def map[B](f: A => B): M[B] = tc.bind(m)(a => tc.return_(f(a)) )
    def flatMap[B](f: A => M[B]): M[B] = tc.bind(m)(f)
  }

  trait Traversal[M[_]] {
    protected[this] implicit def monad : Monad[M]
    def return_[T] : T => M[T] = monad.return_
    def bind[T,S] : M[T] => (T => M[S]) => M[S] = monad.bind

    def nat : Nat => M[Nat] = return_
    def binding[I <: Identifier] : I => M[I] = identifier
    def reference[I <: Identifier] : I => M[I] = identifier
    def depBinding[I <: Kind.Identifier] : I => M[I] = typeIdentifier
    def depReference[I <: Kind.Identifier] : I => M[I] = typeIdentifier
    def identifier[I <: Identifier] : I => M[I] = i =>
      for { t1 <- etype(i.t) }
        yield i.setType(t1).asInstanceOf[I]

    def addressSpace : AddressSpace => M[AddressSpace] = {
      case i : AddressSpaceIdentifier =>
        for { i1 <- typeIdentifier(i) }
          yield i1
      case a => return_(a)
    }
    def datatype : DataType => M[DataType] = {
      case i: DataTypeIdentifier => return_(i).asInstanceOf[M[DataType]]
      case NatType               => return_(NatType.asInstanceOf[DataType])
      case s : ScalarType        => return_(s.asInstanceOf[DataType])
      case ArrayType(n, d) =>
        for {n1 <- nat(n); d1 <- datatype(d)}
          yield ArrayType(n1, d1)
      case DepArrayType(n, n2d) =>
        for {n1 <- nat(n); n2d1 <- natToData(n2d)}
          yield DepArrayType(n1, n2d1)
      case PairType(p1, p2) =>
        for {p11 <- datatype(p1); p21 <- datatype(p2)}
          yield PairType(p11, p21)
      case pair@DepPairType(x, e) =>
        for {x1 <- depBinding(x); e1 <- datatype(e)}
          yield DepPairType(x1, e1)(pair.kindName)
      case IndexType(n) =>
        for {n1 <- nat(n)}
          yield IndexType(n1)
      case VectorType(n, e) =>
        for {n1 <- nat(n); e1 <- datatype(e)}
          yield VectorType(n1, e1)
      case NatToDataApply(ntdf, n) =>
        for {ntdf1 <- natToData(ntdf); n1 <- nat(n)}
          yield NatToDataApply(ntdf1, n1)
    }

    def natToNat : NatToNat => M[NatToNat] = {
      case i : NatToNatIdentifier =>
        for { i1 <- depReference(i) }
          yield i1
      case NatToNatLambda(x, e) =>
        for { x1 <- depBinding(x); e1 <- nat(e) }
          yield NatToNatLambda(x1, e1)
    }

    def natToData : NatToData => M[NatToData] = {
      case i : NatToDataIdentifier =>
        for { i1 <- depReference(i) }
          yield i1
      case NatToDataLambda(x, e) =>
        for { x1 <- depBinding(x); e1 <- datatype(e) }
          yield NatToDataLambda(x1, e1)
    }

    def typeIdentifier[I <: Kind.Identifier] : I => M[I] = i => (i match {
      case n: NatIdentifier => nat(n)
      case dt: DataTypeIdentifier => datatype(dt)
      case a: AddressSpaceIdentifier => addressSpace(a)
      case n2n: NatToNatIdentifier => natToNat(n2n)
      case n2d: NatToDataIdentifier => natToData(n2d)
    }).asInstanceOf[M[I]]

    def data : Data => M[Data] = {
      case NatData(n) =>
        for { n1 <- nat(n) }
          yield NatData(n1)
      case IndexData(i, n) =>
        for { i1 <- nat(i); n1 <- nat(n) }
          yield IndexData(i1, n1)
      case d => return_(d) // TODO
    }

    def primitive : Primitive => M[Expr] = p =>
      for { t1 <- etype(p.t) }
        yield p.setType(t1)

    def etype[T <: Type ] : T => M[T] = t => (t match {
      case TypePlaceholder => return_(TypePlaceholder)
      case i: TypeIdentifier => depReference(i)
      case dt: DataType => datatype(dt)
      case FunType(a, b) =>
        for {a1 <- etype(a); b1 <- etype(b)}
          yield FunType(a1, b1)
      case DepFunType(x, t) => x match {
        case n: NatIdentifier =>
          for { n1 <- depBinding(n); t1 <- etype(t) }
            yield DepFunType[NatKind, Type](n1, t1)
        case dt: DataTypeIdentifier =>
          for { dt1 <- depBinding(dt); t1 <- etype(t) }
            yield DepFunType[DataKind, Type](dt1, t1)
        case a: AddressSpaceIdentifier =>
          for { a1 <- depBinding(a); t1 <- etype(t) }
            yield DepFunType[AddressSpaceKind, Type](a1, t1)
        case n2n: NatToNatIdentifier =>
          for { n2n1 <- depBinding(n2n); t1 <- etype(t) }
            yield DepFunType[NatToNatKind, Type](n2n1, t1)
        case n2d: NatToDataIdentifier =>
          for { n2d1 <- depBinding(n2d); t1 <- etype(t) }
            yield DepFunType[NatToDataKind, Type](n2d1, t1)
      }
    }).asInstanceOf[M[T]]

    def expr : Expr => M[Expr] = {
      case i : Identifier => for { r <- reference(i) } yield r
      case l@Lambda(x, e) =>
        for {
          x1 <- binding(x)
          e1 <- expr(e)
          t1 <- etype(l.t)
        } yield Lambda(x1, e1)(t1)
      case a@App(f, e) =>
        for {
          f1 <- expr(f)
          e1 <- expr(e)
          t1 <- etype(a.t)
        } yield App(f1, e1)(t1)
      case dl@DepLambda(x,e) => x match {
        case n: NatIdentifier =>
          for {n1 <- depBinding(n); e1 <- expr(e); t1 <- etype(dl.t)}
            yield DepLambda[NatKind](n1, e1)(t1)
        case dt: DataTypeIdentifier =>
          for {dt1 <- depBinding(dt); e1 <- expr(e); t1 <- etype(dl.t)}
            yield DepLambda[DataKind](dt1, e1)(t1)
      }
      case da@DepApp(f, x) => x match {
        case n: Nat =>
          for {f1 <- expr(f); n1 <- nat(n); t1 <- etype(da.t)}
            yield DepApp[NatKind](f1, n1)(t1)
        case dt: DataType =>
          for {f1 <- expr(f); dt1 <- datatype(dt); t1 <- etype(da.t)}
            yield DepApp[DataKind](f1, dt1)(t1)
        case a: AddressSpace =>
          for {f1 <- expr(f); a1 <- addressSpace(a); t1 <- etype(da.t)}
            yield DepApp[AddressSpaceKind](f1, a1)(t1)
        case n2n: NatToNat =>
          for {f1 <- expr(f); n2n1 <- natToNat(n2n); t1 <- etype(da.t)}
            yield DepApp[NatToNatKind](f1, n2n1)(t1)
        case n2d: NatToData =>
          for {f1 <- expr(f); n2d1 <- natToData(n2d); t1 <- etype(da.t)}
            yield DepApp[NatToDataKind](f1, n2d1)(t1)
      }
      case Literal(d) =>
        for { d1 <- data(d) }
          yield Literal(d1)
      case p : Primitive => primitive(p)
    }
  }

  trait ExprTraversal[M[_]] extends Traversal[M] {
    override def etype[T <: Type] : T => M[T] = return_
  }

  case class Pure[T](unwrap : T)
  implicit object PureMonad extends Monad[Pure] {
    override def return_[T] : T => Pure[T] = t => Pure(t)
    override def bind[T,S] : Pure[T] => (T => Pure[S]) => Pure[S] =
      v => f => v match { case Pure(v) => f(v) }
  }

  implicit object OptionMonad extends Monad[Option] {
    def return_[T]: T => Option[T] = Some(_)
    def bind[T, S]: Option[T] => (T => Option[S]) => Option[S] = v => v.flatMap
  }

  trait PureTraversal extends Traversal[Pure] { override def monad = PureMonad }
  trait PureExprTraversal extends PureTraversal with ExprTraversal[Pure]

  def apply                  (e : Expr, f : PureTraversal) : Expr    = f.expr(e).unwrap
  def apply[T <: Type]       (t : T   , f : PureTraversal) : T       = f.etype(t).unwrap
  def apply[M[_]]            (e : Expr, f : Traversal[M])  : M[Expr] = f.expr(e)
  def apply[T <: Type, M[_]] (e : T   , f : Traversal[M])  : M[T]    = f.etype(e)
}

object traversal {
  sealed abstract class Result[+T](val value: T) {
    def map[U](f: T => U): Result[U]
    def mapVisitor(f: Visitor => Visitor): Result[T]
  }

  final case class Stop[+T](override val value: T) extends Result[T](value) {
    override def map[U](f: T => U): Result[U] = Stop(f(value))
    def mapVisitor(f: Visitor => Visitor): Result[T] = this
  }

  final case class Continue[+T](override val value: T, v: Visitor)
      extends Result[T](value) {
    override def map[U](f: T => U): Result[U] = Continue(f(value), v)
    def mapVisitor(f: Visitor => Visitor): Result[T] = Continue(value, f(v))
  }

  class Visitor {
    def visitExpr(e: Expr): Result[Expr] = Continue(e, this)
    def visitType[T <: Type](t: T): Result[T] = Continue(t, this)
    def visitNat(ae: Nat): Result[Nat] = Continue(ae, this)
    def visitAddressSpace(a: AddressSpace): Result[AddressSpace] =
      Continue(a, this)
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
            case i: Identifier => i.setType(v.visitType(i.t).value)
            case l @ Lambda(x, e) =>
              apply(x, v) match {
                case newX: Identifier =>
                  Lambda(newX, apply(e, v))(v.visitType(l.t).value)
                case otherwise =>
                  throw new Exception(s"Expected Identifier found: $otherwise")
              }
            case a @ App(f, e) =>
              App(apply(f, v), apply(e, v))(v.visitType(a.t).value)
            case dl @ DepLambda(x, e) =>
              x match {
                case n: NatIdentifier =>
                  DepLambda[NatKind]((v.visitNat(n).value: @unchecked) match {
                    case a: NamedVar => NatIdentifier(a, isExplicit = true)
                  }, apply(e, v))(v.visitType(dl.t).value)
                case dt: DataTypeIdentifier =>
                  DepLambda[DataKind](v.visitType(dt).value, apply(e, v))(
                    v.visitType(dl.t).value
                  )
              }
            case da @ DepApp(f, x) =>
              x match {
                case n: Nat =>
                  DepApp[NatKind](apply(f, v), v.visitNat(n).value)(
                    v.visitType(da.t).value
                  )
                case dt: DataType =>
                  DepApp[DataKind](apply(f, v), v.visitType(dt).value)(
                    v.visitType(da.t).value
                  )
                case a: AddressSpace =>
                  DepApp[AddressSpaceKind](
                    apply(f, v),
                    v.visitAddressSpace(a).value
                  )(v.visitType(da.t).value)
                case n2n: NatToNat =>
                  DepApp[NatToNatKind](apply(f, v), v.visitN2N(n2n).value)(
                    v.visitType(da.t).value
                  )
                case n2d: NatToData =>
                  DepApp[NatToDataKind](apply(f, v), v.visitN2D(n2d).value)(
                    v.visitType(da.t).value
                  )
              }
            case l: Literal =>
              l.d match {
                case NatData(n) => Literal(NatData(v.visitNat(n).value))
                case IndexData(i, n) =>
                  Literal(IndexData(v.visitNat(i).value, v.visitNat(n).value))
                case _ => l
              }
            case p: Primitive => p.setType(v.visitType(p.t).value)
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
              case dt: DataType  => data(dt, v)
              case FunType(a, b) => FunType(apply(a, v), apply(b, v))
              case DepFunType(x, t) =>
                x match {
                  case n: NatIdentifier =>
                    DepFunType[NatKind, Type](
                      (v.visitNat(n).value: @unchecked) match {
                        case n: NamedVar =>
                          NatIdentifier(n.name, n.range, isExplicit = true)
                      },
                      apply(t, v)
                    )
                  case dt: DataTypeIdentifier =>
                    DepFunType[DataKind, Type](data(dt, v), apply(t, v))
                  case a: AddressSpaceIdentifier =>
                    DepFunType[AddressSpaceKind, Type](
                      v.visitAddressSpace(a)
                        .value
                        .asInstanceOf[AddressSpaceIdentifier],
                      apply(t, v)
                    )
                  case n2n: NatToNatIdentifier =>
                    DepFunType[NatToNatKind, Type](
                      v.visitN2N(n2n).value.asInstanceOf[NatToNatIdentifier],
                      apply(t, v)
                    )
                  case n2d: NatToDataIdentifier =>
                    DepFunType[NatToDataKind, Type](
                      v.visitN2D(n2d).value.asInstanceOf[NatToDataIdentifier],
                      apply(t, v)
                    )
                }
              case i: TypeIdentifier => i
              case TypePlaceholder   => TypePlaceholder
            }).asInstanceOf[T]
        }
      }

      def data[DT <: DataType](dt: DT, visit: Visitor): DT = {
        visit.visitType(dt) match {
          case s: Stop[DT] => s.value
          case c: Continue[DT] =>
            val v = c.v
            (c.value match {
              case i: DataTypeIdentifier => i
              case ArrayType(n, e) => ArrayType(v.visitNat(n).value, data(e, v))
              case DepArrayType(n, fdt) =>
                DepArrayType(v.visitNat(n).value, natToData(fdt, v))
              case PairType(p1, p2) => PairType(data(p1, v), data(p2, v))
              case pair@DepPairType(x, e) =>
                  x match {
                    case n: NatIdentifier =>
                      val n2 = visit.visitNat(n).value
                        .asInstanceOf[NatIdentifier]
                      DepPairType[NatKind](n2, data(e, v))
                    case _ => DepPairType(x, data(e,v))(pair.kindName)
                  }
              case NatType          => NatType
              case s: ScalarType    => s
              case IndexType(n)     => IndexType(v.visitNat(n).value)
              case VectorType(n, e) =>
                VectorType(v.visitNat(n).value, data(e, v))
              case NatToDataApply(ndtf, n) =>
                NatToDataApply(natToData(ndtf, v), v.visitNat(n).value)
            }).asInstanceOf[DT]
        }
      }

      def natToNat(n2n: NatToNat, visit: Visitor): NatToNat = {
        visit.visitN2N(n2n) match {
          case s: Stop[NatToNat] => s.value
          case c: Continue[NatToNat] =>
            val v = c.v
            c.value match {
              case i: NatToNatIdentifier => i
              case NatToNatLambda(x, body) =>
                NatToNatLambda(explNatIdentifier(v.visitNat(x).value),
                  v.visitNat(body).value)
            }
        }
      }

      def natToData(n2d: NatToData, visit: Visitor): NatToData = {
        visit.visitN2D(n2d) match {
          case s: Stop[NatToData] => s.value
          case c: Continue[NatToData] =>
            val v = c.v
            c.value match {
              case i: NatToDataIdentifier => i
              case NatToDataLambda(x, body) =>
                val x2 = v.visitNat(x).value
                NatToDataLambda(explNatIdentifier(x2), data(body, v))
            }
        }
      }
    }
  }

  private def explNatIdentifier(m: Nat): NatIdentifier = m match {
    case n: NamedVar => NatIdentifier(n.name, n.range, isExplicit = true)
    case _ => throw new Exception(s"did not expect $m")
  }
}
