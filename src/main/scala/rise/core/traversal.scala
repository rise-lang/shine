package rise.core

import scala.language.implicitConversions
import arithexpr.arithmetic.NamedVar
import rise.core.semantics._
import rise.core.types._

object Traverse {
  trait Monad[M[_]] {
    def return_[T] : T => M[T]
    def bind[T,S] : M[T] => (T => M[S]) => M[S]
    def traverse[A] : Seq[M[A]] => M[Seq[A]] =
      _.foldRight(return_(Nil : Seq[A]))({case (mx, mxs) =>
        bind(mx)(x => bind(mxs)(xs => return_(x +: xs)))})
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
      for { t1 <- `type`(i.t)}
        yield i.setType(t1).asInstanceOf[I]

    def addressSpace : AddressSpace => M[AddressSpace] = {
      case i : AddressSpaceIdentifier =>
        for { i1 <- typeIdentifier(i) }
          yield i1
      case a => return_(a)
    }
    def datatype : DataType => M[DataType] = {
      case i: DataTypeIdentifier => return_(i).asInstanceOf[M[DataType]]
      case NatType               => return_(NatType : DataType)
      case s : ScalarType        => return_(s : DataType)
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
      case t: TypeIdentifier => `type`(t)
    }).asInstanceOf[M[I]]

    def data : Data => M[Data] = {
      case (sd : ScalarData) => return_(sd : Data)
      case VectorData(vd) => return_(VectorData(vd) : Data)
      case NatData(n) =>
        for { n1 <- nat(n) }
          yield NatData(n1)
      case IndexData(i, n) =>
        for { i1 <- nat(i); n1 <- nat(n) }
          yield IndexData(i1, n1)
      case ArrayData(ad) =>
        for { ad1 <- monad.traverse(ad.map(data)) }
          yield ArrayData(ad1)
      case PairData(l, r) =>
        for { l1 <- data(l); r1 <- data(r) }
          yield PairData(l1, r1)
    }

    def primitive : Primitive => M[Expr] = p =>
      for { t1 <- `type`(p.t)}
        yield p.setType(t1)

    def `type`[T <: Type ] : T => M[T] = t => (t match {
      case TypePlaceholder => return_(TypePlaceholder)
      case i: DataTypeIdentifier => depReference(i)
      case i: TypeIdentifier => depReference(i)
      case dt: DataType => datatype(dt)
      case FunType(a, b) =>
        for {a1 <- `type`(a); b1 <- `type`(b)}
          yield FunType(a1, b1)
      case DepFunType(x, t) => x match {
        case n: NatIdentifier =>
          for { n1 <- depBinding(n); t1 <- `type`(t)}
            yield DepFunType[NatKind, Type](n1, t1)
        case dt: DataTypeIdentifier =>
          for { dt1 <- depBinding(dt); t1 <- `type`(t)}
            yield DepFunType[DataKind, Type](dt1, t1)
        case a: AddressSpaceIdentifier =>
          for { a1 <- depBinding(a); t1 <- `type`(t)}
            yield DepFunType[AddressSpaceKind, Type](a1, t1)
        case n2n: NatToNatIdentifier =>
          for { n2n1 <- depBinding(n2n); t1 <- `type`(t)}
            yield DepFunType[NatToNatKind, Type](n2n1, t1)
        case n2d: NatToDataIdentifier =>
          for { n2d1 <- depBinding(n2d); t1 <- `type`(t)}
            yield DepFunType[NatToDataKind, Type](n2d1, t1)
      }
    }).asInstanceOf[M[T]]

    def expr : Expr => M[Expr] = {
      case i : Identifier => for { r <- reference(i) } yield r
      case l@Lambda(x, e) =>
        for {
          x1 <- binding(x)
          e1 <- expr(e)
          t1 <- `type`(l.t)
        } yield Lambda(x1, e1)(t1)
      case a@App(f, e) =>
        for {
          f1 <- expr(f)
          e1 <- expr(e)
          t1 <- `type`(a.t)
        } yield App(f1, e1)(t1)
      case dl@DepLambda(x,e) => x match {
        case n: NatIdentifier =>
          for {n1 <- depBinding(n); e1 <- expr(e); t1 <- `type`(dl.t)}
            yield DepLambda[NatKind](n1, e1)(t1)
        case dt: DataTypeIdentifier =>
          for {dt1 <- depBinding(dt); e1 <- expr(e); t1 <- `type`(dl.t)}
            yield DepLambda[DataKind](dt1, e1)(t1)
      }
      case da@DepApp(f, x) => x match {
        case n: Nat =>
          for {f1 <- expr(f); n1 <- nat(n); t1 <- `type`(da.t)}
            yield DepApp[NatKind](f1, n1)(t1)
        case dt: DataType =>
          for {f1 <- expr(f); dt1 <- datatype(dt); t1 <- `type`(da.t)}
            yield DepApp[DataKind](f1, dt1)(t1)
        case a: AddressSpace =>
          for {f1 <- expr(f); a1 <- addressSpace(a); t1 <- `type`(da.t)}
            yield DepApp[AddressSpaceKind](f1, a1)(t1)
        case n2n: NatToNat =>
          for {f1 <- expr(f); n2n1 <- natToNat(n2n); t1 <- `type`(da.t)}
            yield DepApp[NatToNatKind](f1, n2n1)(t1)
        case n2d: NatToData =>
          for {f1 <- expr(f); n2d1 <- natToData(n2d); t1 <- `type`(da.t)}
            yield DepApp[NatToDataKind](f1, n2d1)(t1)
      }
      case Literal(d) =>
        for { d1 <- data(d) }
          yield Literal(d1)
      case p : Primitive => primitive(p)
    }
  }

  trait ExprTraversal[M[_]] extends Traversal[M] {
    override def `type`[T <: Type] : T => M[T] = return_
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

  def apply(e : Expr, f : PureTraversal) : Expr = f.expr(e).unwrap
  def apply[M[_]](e : Expr, f : Traversal[M]) : M[Expr] = f.expr(e)
  def apply[T <: Type](t : T, f : PureTraversal) : T = f.`type`(t).unwrap
  def apply[T <: Type, M[_]](e : T, f : Traversal[M]) : M[T] = f.`type`(e)
}
