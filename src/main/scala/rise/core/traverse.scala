package rise.core

import scala.language.implicitConversions
import util.monads._
import rise.core.semantics._
import rise.core.types._

object traverse {
  sealed trait VarType
  case object Binding extends VarType
  case object Reference extends VarType

  trait Traversal[M[+_]] {
    protected[this] implicit def monad : Monad[M]
    def return_[T] : T => M[T] = monad.return_
    def bind[T,S] : M[T] => (T => M[S]) => M[S] = monad.bind

    // Identifiers are first routed through here before being sent
    // to nat/datatype/addressSpace/natToNat/natToData
    def nat : Nat => M[Nat] = return_
    def typeIdentifier[I <: Kind.Identifier] : VarType => I => M[I] = _ => return_
    def identifier[I <: Identifier] : VarType => I => M[I] = _ => i =>
      for { t1 <- `type`(i.t)}
        yield i.setType(t1).asInstanceOf[I]
    def typeIdentifierDispatch[I <: Kind.Identifier] : VarType => I => M[I] = vt => i => (i match {
      case n: NatIdentifier => bind(typeIdentifier(vt)(n))(nat)
      case dt: DataTypeIdentifier => bind(typeIdentifier(vt)(dt))(datatype)
      case a: AddressSpaceIdentifier => bind(typeIdentifier(vt)(a))(addressSpace)
      case m: MatrixLayoutIdentifier => bind(typeIdentifier(vt)(m))(matrixLayout)
      case f: FragmentKindIdentifier => bind(typeIdentifier(vt)(f))(fragmentKind)
      case n2n: NatToNatIdentifier => bind(typeIdentifier(vt)(n2n))(natToNat)
      case n2d: NatToDataIdentifier => bind(typeIdentifier(vt)(n2d))(natToData)
      case t: TypeIdentifier => typeIdentifier(vt)(t)
    }).asInstanceOf[M[I]]
    def natDispatch : VarType => Nat => M[Nat] = vt => {
      case i : NatIdentifier =>
        bind(typeIdentifier(vt)(i))(nat)
      case n => nat(n)
    }

    def addressSpace : AddressSpace => M[AddressSpace] = return_
    def matrixLayout : MatrixLayout => M[MatrixLayout] = return_
    def fragmentKind : FragmentKind => M[FragmentKind] = return_
    def datatype : DataType => M[DataType] = {
      case i: DataTypeIdentifier => return_(i.asInstanceOf[DataType])
      case NatType               => return_(NatType : DataType)
      case s : ScalarType        => return_(s : DataType)
      case ArrayType(n, d) =>
        for {n1 <- natDispatch(Reference)(n); d1 <- `type`[DataType](d)}
          yield ArrayType(n1, d1)
      case DepArrayType(n, n2d) =>
        for {n1 <- natDispatch(Reference)(n); n2d1 <- natToData(n2d)}
          yield DepArrayType(n1, n2d1)
      case PairType(p1, p2) =>
        for {p11 <- `type`(p1); p21 <- `type`(p2)}
          yield PairType(p11, p21)
      case pair@DepPairType(x, e) =>
        for {x1 <- typeIdentifierDispatch(Binding)(x); e1 <- `type`(e)}
          yield DepPairType(x1, e1)(pair.kindName)
      case IndexType(n) =>
        for {n1 <- natDispatch(Reference)(n)}
          yield IndexType(n1)
      case VectorType(n, e) =>
        for {n1 <- natDispatch(Reference)(n); e1 <- `type`(e)}
          yield VectorType(n1, e1)
      case FragmentType(rows, columns, d3, dt, fragKind, layout) =>
        for {rows1 <- nat(rows); columns1 <- nat(columns); d31 <- nat(d3); dt1 <- datatype(dt);
          fragKind1 <- fragmentKind(fragKind); layout1 <- matrixLayout(layout)}
        yield FragmentType(rows1, columns1, d31, dt1, fragKind1, layout1)
      case NatToDataApply(ntdf, n) =>
        for {ntdf1 <- natToData(ntdf); n1 <- natDispatch(Reference)(n)}
          yield NatToDataApply(ntdf1, n1)
    }

    def natToNat : NatToNat => M[NatToNat] = {
      case i : NatToNatIdentifier => return_(i.asInstanceOf[NatToNat])
      case NatToNatLambda(x, e) =>
        for { x1 <- typeIdentifierDispatch(Binding)(x); e1 <- natDispatch(Reference)(e) }
          yield NatToNatLambda(x1, e1)
    }

    def natToData : NatToData => M[NatToData] = {
      case i : NatToDataIdentifier => return_(i.asInstanceOf[NatToData])
      case NatToDataLambda(x, e) =>
        for { x1 <- typeIdentifierDispatch(Binding)(x); e1 <- `type`(e) }
          yield NatToDataLambda(x1, e1)
    }

    def data : Data => M[Data] = {
      case (sd : ScalarData) => return_(sd : Data)
      case VectorData(vd) => return_(VectorData(vd) : Data)
      case NatData(n) =>
        for { n1 <- natDispatch(Reference)(n) }
          yield NatData(n1)
      case IndexData(i, n) =>
        for { i1 <- natDispatch(Reference)(i); n1 <- natDispatch(Reference)(n) }
          yield IndexData(i1, n1)
      case ArrayData(ad) =>
        for { ad1 <- monad.traverse(ad.map(data)) }
          yield ArrayData(ad1)
      case PairData(l, r) =>
        for { l1 <- data(l); r1 <- data(r) }
          yield PairData(l1, r1)
    }

    def `type`[T <: Type ] : T => M[T] = t => (t match {
      case TypePlaceholder => return_(TypePlaceholder)
      case i: DataTypeIdentifier => typeIdentifierDispatch(Reference)(i)
      case i: TypeIdentifier => typeIdentifierDispatch(Reference)(i)
      case dt: DataType => datatype(dt)
      case FunType(a, b) =>
        for {a1 <- `type`(a); b1 <- `type`(b)}
          yield FunType(a1, b1)
      case DepFunType(x, t) => x match {
        case n: NatIdentifier =>
          for { n1 <- typeIdentifierDispatch(Binding)(n); t1 <- `type`(t)}
            yield DepFunType[NatKind, Type](n1, t1)
        case dt: DataTypeIdentifier =>
          for { dt1 <- typeIdentifierDispatch(Binding)(dt); t1 <- `type`(t)}
            yield DepFunType[DataKind, Type](dt1, t1)
        case a: AddressSpaceIdentifier =>
          for { a1 <- typeIdentifierDispatch(Binding)(a); t1 <- `type`(t)}
            yield DepFunType[AddressSpaceKind, Type](a1, t1)
        case n2n: NatToNatIdentifier =>
          for { n2n1 <- typeIdentifierDispatch(Binding)(n2n); t1 <- `type`(t)}
            yield DepFunType[NatToNatKind, Type](n2n1, t1)
        case n2d: NatToDataIdentifier =>
          for { n2d1 <- typeIdentifierDispatch(Binding)(n2d); t1 <- `type`(t)}
            yield DepFunType[NatToDataKind, Type](n2d1, t1)
      }
    }).asInstanceOf[M[T]]

    def expr : Expr => M[Expr] = {
      case i : Identifier => for { r <- identifier(Reference)(i) } yield r
      case l@Lambda(x, e) =>
        for {
          x1 <- identifier(Binding)(x)
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
          for {n1 <- typeIdentifierDispatch(Binding)(n); e1 <- expr(e); t1 <- `type`(dl.t)}
            yield DepLambda[NatKind](n1, e1)(t1)
        case dt: DataTypeIdentifier =>
          for {dt1 <- typeIdentifierDispatch(Binding)(dt); e1 <- expr(e); t1 <- `type`(dl.t)}
            yield DepLambda[DataKind](dt1, e1)(t1)
      }
      case da@DepApp(f, x) => x match {
        case n: Nat =>
          for {f1 <- expr(f); n1 <- natDispatch(Reference)(n); t1 <- `type`(da.t)}
            yield DepApp[NatKind](f1, n1)(t1)
        case dt: DataType =>
          for {f1 <- expr(f); dt1 <- `type`(dt); t1 <- `type`(da.t)}
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
      case TypeAnnotation(e, t) =>
        for { e1 <- expr(e); t1 <- `type`(t)}
          yield TypeAnnotation(e1, t1)
      case TypeAssertion(e, t) =>
        for { e1 <- expr(e); t1 <- `type`(t)}
          yield TypeAssertion(e1, t1)
      case Opaque(e, t) =>
        for { e1 <- expr(e); t1 <- `type`(t)}
          yield Opaque(e1, t1)
      case p : Primitive =>
        for { t1 <- `type`(p.t)}
          yield p.setType(t1)
    }
  }

  trait ExprTraversal[M[+_]] extends Traversal[M] {
    override def `type`[T <: Type] : T => M[T] = return_
  }

  trait PureTraversal extends Traversal[Pure] {override def monad : PureMonad.type = PureMonad }
  trait PureExprTraversal extends PureTraversal with ExprTraversal[Pure]
  trait AccumulatorTraversal[F,M[+_]] extends Traversal[InMonad[M]#SetFst[F]#Type] {
    type Pair[T] = InMonad[M]#SetFst[F]#Type[T]
    implicit val accumulator : Monoid[F]
    implicit val wrapperMonad : Monad[M]
    def accumulate[T] : F => T => Pair[T] = f => t => wrapperMonad.return_((f, t))
    override def monad : PairMonoidMonad[F,M] = new PairMonoidMonad[F,M] {
      override val monoid = implicitly(accumulator)
      override val monad = implicitly(wrapperMonad)
    }
  }
  trait PureAccumulatorTraversal[F] extends AccumulatorTraversal[F, Pure] {
    override val wrapperMonad : PureMonad.type = PureMonad
  }

  def traverse(e: Expr, f: PureTraversal): Expr = f.expr(e).unwrap
  def traverse[T <: Type](t: T, f: PureTraversal): T = f.`type`(t).unwrap
  def traverse[F](e: Expr, f: PureAccumulatorTraversal[F]): (F, Expr) = f.expr(e).unwrap
  def traverse[F,T <: Type](t: T, f: PureAccumulatorTraversal[F]): (F, T) = f.`type`(t).unwrap
  def traverse[M[+_]](e: Expr, f: Traversal[M]): M[Expr] = f.expr(e)
  def traverse[T <: Type, M[+_]](e: T, f: Traversal[M]): M[T] = f.`type`(e)
}
